/*	if_acc.c	4.5	82/02/21	*/

#include "acc.h"
#ifdef NACC > 0

/*
 * ACC LH/DH ARPAnet IMP interface driver.
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/ubareg.h"
#include "../h/ubavar.h"
#include "../h/cpu.h"
#include "../h/mtpr.h"
#include "../h/vmmac.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/if_acc.h"
#include "../net/if_imp.h"
#include "../net/if_uba.h"

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

COUNT(ACCPROBE);
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

COUNT(ACCATTACH);
	if ((ifimp = (struct ifimpcb *)impattach(ui)) == 0)
		panic("accattach");
	sc->acc_if = &ifimp->ifimp_if;
	ip = &ifimp->ifimp_impcb;
	sc->acc_ic = ip;
	ip->ic_init = accinit;
	ip->ic_start = accstart;
#ifdef notdef
	sc->acc_ifuba.ifu_flags = UBA_NEEDBDP;
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

COUNT(ACCRESET);
	if (unit >= NACC || (ui = accinfo[unit]) == 0 || ui->ui_alive == 0 ||
	    ui->ui_ubanum != uban)
		return;
	printf(" acc%d", unit);
	sc = &acc_softc[unit];
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
	int x, info;

COUNT(ACCINIT);
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
	if (if_ubainit(&sc->acc_ifuba, ui->ui_ubanum, 0, btoc(IMP_MTU)) == 0) {
		printf("acc%d: can't initialize\n", unit);
		goto down;
	}
	addr = (struct accdevice *)ui->ui_addr;

	/*
	 * Reset the imp interface;
	 * the delays are pure guesswork.
	 */
	x = spl5();
	addr->icsr = ACC_RESET; DELAY(5000);
        addr->ocsr = ACC_RESET; DELAY(5000);
	addr->ocsr = OUT_BBACK;	DELAY(1000);	/* reset host master ready */
	addr->ocsr = 0;
	splx(x);
	addr->icsr = IN_MRDY;			/* close the relay */

	/* YECH!!! */
	x = 500;
	while (x-- > 0) {
		if ((addr->icsr & IN_HRDY) ||
		    (addr->icsr & (IN_RMR | IN_IMPBSY) == 0))
			break;
		addr->icsr = IN_MRDY; DELAY(5000); /* keep turning IN_RMR off */
	}
	if (x <= 0) {
		printf("acc%d: imp doesn't respond, icsr=%b\n", unit,
			addr->icsr, ACC_INBITS);
		goto down;
	}

	/*
	 * Put up a read.  We can't restart any outstanding writes
	 * until we're back in synch with the IMP (i.e. we've flushed
	 * the NOOPs it throws at us).
	 * Note: IMP_MTU includes the leader.
	 */
	x = spl5();
	info = sc->acc_ifuba.ifu_r.ifrw_info;
	addr->iba = (u_short)info;
	addr->iwc = -(IMP_MTU >> 1);
#ifdef LOOPBACK
	addr->ocsr |= OUT_BBACK;
#endif
	addr->icsr = 
		IN_MRDY | ACC_IE | IN_WEN | ((info & 0x30000) >> 12) | ACC_GO;
	splx(x);
	return (1);
down:
	ui->ui_alive = 0;
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

COUNT(ACCSTART);
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
{
	register struct acc_softc *sc = &acc_softc[unit];
	register struct accdevice *addr;

COUNT(ACCXINT);
	if (sc->acc_ic->ic_oactive == 0) {
		printf("acc%d: stray xmit interrupt\n", unit);
		return;
	}
	addr = (struct accdevice *)accinfo[unit]->ui_addr;
	sc->acc_if->if_opackets++;
	sc->acc_ic->ic_oactive = 0;
	if (addr->ocsr & ACC_ERR) {
		printf("acc%d: output error, csr=%b\n", unit,
			addr->ocsr, ACC_OUTBITS);
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
{
	register struct acc_softc *sc = &acc_softc[unit];
	register struct accdevice *addr;
	register struct ifqueue *inq;
    	struct mbuf *m;
	int len, info;

COUNT(ACCRINT);
	sc->acc_if->if_ipackets++;

	/*
	 * Purge BDP; flush message if error indicated.
	 */
	if (sc->acc_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(sc->acc_ifuba.ifu_uba, sc->acc_ifuba.ifu_r.ifrw_bdp);
	addr = (struct accdevice *)accinfo[unit]->ui_addr;
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
	len = IMP_MTU + (addr->iwc << 1);
	if (len < 0 || len > IMP_MTU) {
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
#ifdef notdef
	/* adjust message length for padding. */
	m->m_len -= 2;
#endif
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
	addr->iwc = -(IMP_MTU >> 1);
	addr->icsr =
		IN_MRDY | ACC_IE | IN_WEN | ((info & 0x30000) >> 12) | ACC_GO;
}
#endif
