/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)if_acc.c	7.8 (Berkeley) 12/16/90
 */

#include "acc.h"
#if NACC > 0

/*
 * ACC LH/DH ARPAnet IMP interface driver.
 */
#include "machine/pte.h"

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/mbuf.h"
#include "sys/buf.h"
#include "sys/protosw.h"
#include "sys/socket.h"
#include "sys/vmmac.h"

#include "net/if.h"
#include "netimp/if_imp.h"

#include "../include/cpu.h"
#include "../include/mtpr.h"
#include "if_accreg.h"
#include "if_uba.h"
#include "../uba/ubareg.h"
#include "../uba/ubavar.h"

int     accprobe(), accattach(), accrint(), accxint();
struct  uba_device *accinfo[NACC];
u_short accstd[] = { 0 };
struct  uba_driver accdriver =
	{ accprobe, 0, accattach, 0, accstd, "acc", accinfo };

int	accinit(), accoutput(), accdown(), accreset();

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
	struct	imp_softc *acc_imp;	/* data structure shared with IMP */
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
	register struct uba_device *ui;
{
	register struct acc_softc *sc = &acc_softc[ui->ui_unit];
	register struct impcb *ip;

	if ((sc->acc_imp = impattach(ui->ui_driver->ud_dname, ui->ui_unit,
	    accreset)) == 0)
		return;
	ip = &sc->acc_imp->imp_cb;
	ip->ic_init = accinit;
	ip->ic_output = accoutput;
	ip->ic_down = accdown;
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
	sc->acc_imp->imp_if.if_flags &= ~IFF_RUNNING;
	accoflush(unit);
	/* must go through IMP to allow it to set state */
	(*sc->acc_imp->imp_if.if_init)(sc->acc_imp->imp_if.if_unit);
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
	if ((sc->acc_imp->imp_if.if_flags & IFF_RUNNING) == 0 &&
	    if_ubainit(&sc->acc_ifuba, ui->ui_ubanum, 0,
	     (int)btoc(IMP_RCVBUF)) == 0) {
		printf("acc%d: can't initialize\n", unit);
		sc->acc_imp->imp_if.if_flags &= ~(IFF_UP | IFF_RUNNING);
		return (0);
	}
	sc->acc_imp->imp_if.if_flags |= IFF_RUNNING;
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
	 * Note: IMP_RCVBUF includes the leader.
	 */
	info = sc->acc_ifuba.ifu_r.ifrw_info;
	addr->iba = (u_short)info;
	addr->iwc = -((IMP_RCVBUF) >> 1);
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
 * Drop the host ready line to mark host down.
 */
accdown(unit)
	int unit;
{
	register struct accdevice *addr;

	addr = (struct accdevice *)(accinfo[unit]->ui_addr);
        addr->ocsr = ACC_RESET;
	DELAY(5000);
	addr->ocsr = OUT_BBACK;		/* reset host master ready */
	accoflush(unit);
	return (1);
}

accoflush(unit)
	int unit;
{
	register struct acc_softc *sc = &acc_softc[unit];

	sc->acc_imp->imp_cb.ic_oactive = 0;
	if (sc->acc_ifuba.ifu_xtofree) {
		m_freem(sc->acc_ifuba.ifu_xtofree);
		sc->acc_ifuba.ifu_xtofree = 0;
	}
}

/*
 * Start output on an interface.
 */
accoutput(unit, m)
	int unit;
	struct mbuf *m;
{
	int info;
	register struct acc_softc *sc = &acc_softc[unit];
	register struct accdevice *addr;
	u_short cmd;

	sc->acc_olen = if_wubaput(&sc->acc_ifuba, m);
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
	sc->acc_imp->imp_cb.ic_oactive = 1;
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
	if (sc->acc_imp->imp_cb.ic_oactive == 0) {
		printf("acc%d: stray xmit interrupt, csr=%b\n", unit,
			addr->ocsr, ACC_OUTBITS);
		return;
	}
	sc->acc_imp->imp_if.if_opackets++;
	sc->acc_imp->imp_cb.ic_oactive = 0;
	if (addr->ocsr & ACC_ERR) {
		printf("acc%d: output error, ocsr=%b, icsr=%b\n", unit,
			addr->ocsr, ACC_OUTBITS, addr->icsr, ACC_INBITS);
		sc->acc_imp->imp_if.if_oerrors++;
	}
	if (sc->acc_ifuba.ifu_xtofree) {
		m_freem(sc->acc_ifuba.ifu_xtofree);
		sc->acc_ifuba.ifu_xtofree = 0;
	}
	impstart(sc->acc_imp);
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
	sc->acc_imp->imp_if.if_ipackets++;

	/*
	 * Purge BDP; flush message if error indicated.
	 */
	if (sc->acc_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(sc->acc_ifuba.ifu_uba, sc->acc_ifuba.ifu_r.ifrw_bdp);
	if (addr->icsr & ACC_ERR) {
		printf("acc%d: input error, csr=%b\n", unit,
		    addr->icsr, ACC_INBITS);
		sc->acc_imp->imp_if.if_ierrors++;
		sc->acc_flush = 1;
	}

	if (sc->acc_flush) {
		if (addr->icsr & IN_EOM)
			sc->acc_flush = 0;
		goto setup;
	}
	len = IMP_RCVBUF + (addr->iwc << 1);
	if (len < 0 || len > IMP_RCVBUF) {
		printf("acc%d: bad length=%d\n", unit, len);
		sc->acc_imp->imp_if.if_ierrors++;
		goto setup;
	}

	/*
	 * The offset parameter is always 0 since using
	 * trailers on the ARPAnet is insane.
	 */
	m = if_rubaget(&sc->acc_ifuba, len, 0, &sc->acc_imp->imp_if);
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
	addr->iwc = -((IMP_RCVBUF)>> 1);
	addr->icsr =
		IN_MRDY | ACC_IE | IN_WEN | ((info & 0x30000) >> 12) | ACC_GO;
}
#endif
