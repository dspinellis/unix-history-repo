/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_acc.c	6.5 (Berkeley) %G%
 */

#include "acc.h"
#if NACC > 0

/*
 * ACC LH/DH ARPAnet IMP interface driver.
 */
#include "../machine/pte.h"

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "protosw.h"
#include "socket.h"
#include "vmmac.h"

#include "../net/if.h"
#include "../netimp/if_imp.h"

#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "if_accreg.h"
#include "if_uba.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/ubavar.h"

int     accprobe(), accattach(), accrint(), accxint();
struct  uba_device *accinfo[NACC];
u_short accstd[] = { 0 };
struct  uba_driver accdriver =
	{ accprobe, 0, accattach, 0, accstd, "acc", accinfo };
#define	ACCUNIT(x)	minor(x)

int	accinit(), accstart(), accreset();

/*
 * "Lower half" of IMP interface driver.
 *
 * Each IMP interface is handled by a common module which handles
 * the IMP-host protocol and a hardware driver which manages the
 * hardware specific details of talking with the IMP.
 *
 * The hardware portion of the IMP driver handles DMA and related
 * management of UNIBUS resources.  The IMP protocol module interprets
 * contents of these messages and "controls" the actions of the
 * hardware module during IMP resets, but not, for instance, during
 * UNIBUS resets.
 *
 * The two modules are coupled at "attach time", and ever after,
 * through the imp interface structure.  Higher level protocols,
 * e.g. IP, interact with the IMP driver, rather than the ACC.
 */
struct	acc_softc {
	struct	ifnet *acc_if;		/* pointer to IMP's ifnet struct */
	struct	impcb *acc_ic;		/* data structure shared with IMP */
	struct	ifuba acc_ifuba;	/* UNIBUS resources */
	struct	mbuf *acc_iq;		/* input reassembly queue */
	short	acc_olen;		/* size of last message sent */
	char	acc_flush;		/* flush remainder of message */
} acc_softc[NACC];

/*
 * Reset the IMP and cause a transmitter interrupt by
 * performing a null DMA.
 */
accprobe(reg)
	caddr_t reg;
{
	register int br, cvec;		/* r11, r10 value-result */
	register struct accdevice *addr = (struct accdevice *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	accrint(0); accxint(0);
#endif
	addr->icsr = ACC_RESET; DELAY(5000);
	addr->ocsr = ACC_RESET; DELAY(5000);
	addr->ocsr = OUT_BBACK; DELAY(5000);
	addr->owc = 0;
	addr->ocsr = ACC_IE | ACC_GO; DELAY(5000);
	addr->ocsr = 0;
	if (cvec && cvec != 0x200)	/* transmit -> receive */
		cvec -= 4;
	return (1);
}

/*
 * Call the IMP module to allow it to set up its internal
 * state, then tie the two modules together by setting up
 * the back pointers to common data structures.
 */
accattach(ui)
	struct uba_device *ui;
{
	register struct acc_softc *sc = &acc_softc[ui->ui_unit];
	register struct impcb *ip;
	struct ifimpcb {
		struct	ifnet ifimp_if;
		struct	impcb ifimp_impcb;
	} *ifimp;

	if ((ifimp = (struct ifimpcb *)impattach(ui, accreset)) == 0)
		panic("accattach");
	sc->acc_if = &ifimp->ifimp_if;
	ip = &ifimp->ifimp_impcb;
	sc->acc_ic = ip;
	ip->ic_init = accinit;
	ip->ic_start = accstart;
	sc->acc_ifuba.ifu_flags = UBA_CANTWAIT;
#ifdef notdef
	sc->acc_ifuba.ifu_flags |= UBA_NEEDBDP;
#endif
}

/*
 * Reset interface after UNIBUS reset.
 * If interface is on specified uba, reset its state.
 */
accreset(unit, uban)
	int unit, uban;
{
	register struct uba_device *ui;
	struct acc_softc *sc;

	if (unit >= NACC || (ui = accinfo[unit]) == 0 || ui->ui_alive == 0 ||
	    ui->ui_ubanum != uban)
		return;
	printf(" acc%d", unit);
	sc = &acc_softc[unit];
	sc->acc_if->if_flags &= ~IFF_RUNNING;
	/* must go through IMP to allow it to set state */
	(*sc->acc_if->if_init)(unit);
}

/*
 * Initialize interface: clear recorded pending operations,
 * and retrieve, and initialize UNIBUS resources.  Note
 * return value is used by IMP init routine to mark IMP
 * unavailable for outgoing traffic.
 */
accinit(unit)
	int unit;
{	
	register struct acc_softc *sc;
	register struct uba_device *ui;
	register struct accdevice *addr;
	int info;

	if (unit >= NACC || (ui = accinfo[unit]) == 0 || ui->ui_alive == 0) {
		printf("acc%d: not alive\n", unit);
		return (0);
	}
	sc = &acc_softc[unit];
	/*
	 * Header length is 0 since we have to passs
	 * the IMP leader up to the protocol interpretation
	 * routines.  If we had the header length as
	 * sizeof(struct imp_leader), then the if_ routines
	 * would asssume we handle it on input and output.
	 */
	if (if_ubainit(&sc->acc_ifuba, ui->ui_ubanum, 0,
	     (int)btoc(IMPMTU)) == 0) {
		printf("acc%d: can't initialize\n", unit);
		ui->ui_alive = 0;
		return (0);
	}
	sc->acc_if->if_flags |= IFF_RUNNING;
	addr = (struct accdevice *)ui->ui_addr;

	/*
	 * Reset the imp interface;
	 * the delays are pure guesswork.
	 */
        addr->ocsr = ACC_RESET; DELAY(5000);
	addr->ocsr = OUT_BBACK;	DELAY(5000);	/* reset host master ready */
	addr->ocsr = 0;
	if (accinputreset(addr, unit) == 0) {
		ui->ui_alive = 0;
		return (0);
	}

	/*
	 * Put up a read.  We can't restart any outstanding writes
	 * until we're back in synch with the IMP (i.e. we've flushed
	 * the NOOPs it throws at us).
	 * Note: IMPMTU includes the leader.
	 */
	info = sc->acc_ifuba.ifu_r.ifrw_info;
	addr->iba = (u_short)info;
	addr->iwc = -(IMPMTU >> 1);
#ifdef LOOPBACK
	addr->ocsr |= OUT_BBACK;
#endif
	addr->icsr = 
		IN_MRDY | ACC_IE | IN_WEN | ((info & 0x30000) >> 12) | ACC_GO;
	return (1);
}

accinputreset(addr, unit)
	register struct accdevice *addr;
	register int unit;
{
	register int i;

	addr->icsr = ACC_RESET; DELAY(5000);
	addr->icsr = IN_MRDY | IN_WEN;		/* close the relay */
	DELAY(10000);
	/* YECH!!! */
	for (i = 0; i < 500; i++) {
		if ((addr->icsr & IN_HRDY) ||
		    (addr->icsr & (IN_RMR | IN_IMPBSY)) == 0)
			return (1);
		addr->icsr = IN_MRDY | IN_WEN; DELAY(10000);
		/* keep turning IN_RMR off */
	}
	printf("acc%d: imp doesn't respond, icsr=%b\n", unit,
		addr->icsr, ACC_INBITS);
	return (0);
}

/*
 * Start output on an interface.
 */
accstart(dev)
	dev_t dev;
{
	int unit = ACCUNIT(dev), info;
	register struct acc_softc *sc = &acc_softc[unit];
	register struct accdevice *addr;
	struct mbuf *m;
	u_short cmd;

	if (sc->acc_ic->ic_oactive)
		goto restart;
	
	/*
	 * Not already active, deqeue a request and
	 * map it onto the UNIBUS.  If no more
	 * requeusts, just return.
	 */
	IF_DEQUEUE(&sc->acc_if->if_snd, m);
	if (m == 0) {
		sc->acc_ic->ic_oactive = 0;
		return;
	}
	sc->acc_olen = if_wubaput(&sc->acc_ifuba, m);

restart:
	/*
	 * Have request mapped to UNIBUS for
	 * transmission; start the output.
	 */
	if (sc->acc_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(sc->acc_ifuba.ifu_uba, sc->acc_ifuba.ifu_w.ifrw_bdp);
	addr = (struct accdevice *)accinfo[unit]->ui_addr;
	info = sc->acc_ifuba.ifu_w.ifrw_info;
	addr->oba = (u_short)info;
	addr->owc = -((sc->acc_olen + 1) >> 1);
	cmd = ACC_IE | OUT_ENLB | ((info & 0x30000) >> 12) | ACC_GO;
#ifdef LOOPBACK
	cmd |= OUT_BBACK;
#endif
	addr->ocsr = cmd;
	sc->acc_ic->ic_oactive = 1;
}

/*
 * Output interrupt handler.
 */
accxint(unit)
	int unit;
{
	register struct acc_softc *sc = &acc_softc[unit];
	register struct accdevice *addr;

	addr = (struct accdevice *)accinfo[unit]->ui_addr;
	if (sc->acc_ic->ic_oactive == 0) {
		printf("acc%d: stray xmit interrupt, csr=%b\n", unit,
			addr->ocsr, ACC_OUTBITS);
		return;
	}
	sc->acc_if->if_opackets++;
	sc->acc_ic->ic_oactive = 0;
	if (addr->ocsr & ACC_ERR) {
		printf("acc%d: output error, ocsr=%b, icsr=%b\n", unit,
			addr->ocsr, ACC_OUTBITS, addr->icsr, ACC_INBITS);
		sc->acc_if->if_oerrors++;
	}
	if (sc->acc_ifuba.ifu_xtofree) {
		m_freem(sc->acc_ifuba.ifu_xtofree);
		sc->acc_ifuba.ifu_xtofree = 0;
	}
	if (sc->acc_if->if_snd.ifq_head)
		accstart(unit);
}

/*
 * Input interrupt handler
 */
accrint(unit)
	int unit;
{
	register struct acc_softc *sc = &acc_softc[unit];
	register struct accdevice *addr;
    	struct mbuf *m;
	int len, info;

	addr = (struct accdevice *)accinfo[unit]->ui_addr;
	sc->acc_if->if_ipackets++;

	/*
	 * Purge BDP; flush message if error indicated.
	 */
	if (sc->acc_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(sc->acc_ifuba.ifu_uba, sc->acc_ifuba.ifu_r.ifrw_bdp);
	if (addr->icsr & ACC_ERR) {
		printf("acc%d: input error, csr=%b\n", unit,
		    addr->icsr, ACC_INBITS);
		sc->acc_if->if_ierrors++;
		sc->acc_flush = 1;
	}

	if (sc->acc_flush) {
		if (addr->icsr & IN_EOM)
			sc->acc_flush = 0;
		goto setup;
	}
	len = IMPMTU + (addr->iwc << 1);
	if (len < 0 || len > IMPMTU) {
		printf("acc%d: bad length=%d\n", len);
		sc->acc_if->if_ierrors++;
		goto setup;
	}

	/*
	 * The last parameter is always 0 since using
	 * trailers on the ARPAnet is insane.
	 */
	m = if_rubaget(&sc->acc_ifuba, len, 0);
	if (m == 0)
		goto setup;
	if ((addr->icsr & IN_EOM) == 0) {
		if (sc->acc_iq)
			m_cat(sc->acc_iq, m);
		else
			sc->acc_iq = m;
		goto setup;
	}
	if (sc->acc_iq) {
		m_cat(sc->acc_iq, m);
		m = sc->acc_iq;
		sc->acc_iq = 0;
	}
	impinput(unit, m);

setup:
	/*
	 * Setup for next message.
	 */
	info = sc->acc_ifuba.ifu_r.ifrw_info;
	addr->iba = (u_short)info;
	addr->iwc = -(IMPMTU >> 1);
	addr->icsr =
		IN_MRDY | ACC_IE | IN_WEN | ((info & 0x30000) >> 12) | ACC_GO;
}
#endif
