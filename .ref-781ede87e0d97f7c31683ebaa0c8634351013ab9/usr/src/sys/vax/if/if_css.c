/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_css.c	7.9 (Berkeley) %G%
 */

#include "css.h"
#if NCSS > 0

/*
 * DEC/CSS IMP11-A ARPAnet IMP interface driver.
 * Since "imp11a" is such a mouthful, it is called
 * "css" after the LH/DH being called "acc".
 *
 * Configuration notes:
 *
 * As delivered from DEC/CSS, it
 * is addressed and vectored as two DR11-B's.  This makes
 * Autoconfig almost IMPOSSIBLE.  To make it work, the
 * interrupt vectors must be restrapped to make the vectors
 * consecutive.  The 020 hole between the CSR addresses is
 * tolerated, althought that could be cleaned-up also.
 *
 * Additionally, the TRANSMIT side of the IMP11-A has the
 * lower address of the two subunits, so the vector ordering
 * in the CONFIG file is reversed from most other devices.
 * It should be:
 *
 * device css0 ....  cssxint cssrint
 *
 * If you get it wrong, it will still autoconfig, but will just
 * sit there with RECEIVE IDLE indicated on the front panel.
 */
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/mbuf.h"
#include "sys/buf.h"
#include "sys/protosw.h"
#include "sys/socket.h"
#include "sys/vmmac.h"

#include "../include/pte.h"

#include "net/if.h"
#include "netimp/if_imp.h"

#include "../include/cpu.h"
#include "../include/mtpr.h"
#include "if_cssreg.h"
#include "if_uba.h"
#include "../uba/ubareg.h"
#include "../uba/ubavar.h"

int     cssprobe(), cssattach(), cssrint(), cssxint();
struct  uba_device *cssinfo[NCSS];
u_short cssstd[] = { 0 };
struct  uba_driver cssdriver =
        { cssprobe, 0, cssattach, 0, cssstd, "css", cssinfo };

int     cssinit(), cssoutput(), cssdown(), cssreset();

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
 * e.g. IP, interact with the IMP driver, rather than the CSS.
 */
struct  css_softc {
	struct	imp_softc *css_imp;	/* pointer to IMP's imp_softc struct */
	struct	ifuba css_ifuba;	/* UNIBUS resources */
	struct	mbuf *css_iq;		/* input reassembly queue */
	short	css_olen;		/* size of last message sent */
	char	css_flush;		/* flush remainder of message */
} css_softc[NCSS];

/*
 * Reset the IMP and cause a transmitter interrupt by
 * performing a null DMA.
 */
cssprobe(reg)
        caddr_t reg;
{
        register int br, cvec;          /* r11, r10 value-result */
        register struct cssdevice *addr = (struct cssdevice *)reg;

#ifdef lint
        br = 0; cvec = br; br = cvec;
        cssrint(0); cssxint(0);
#endif

        addr->css_icsr = CSS_CLR;
        addr->css_ocsr = CSS_CLR;
        DELAY(50000);
	addr->css_icsr = 0;
	addr->css_ocsr = 0;
        DELAY(50000);

	addr->css_oba = 0;
	addr->css_owc = -1;
        addr->css_ocsr = CSS_IE | CSS_GO;	/* enable interrupts */
        DELAY(50000);
        addr->css_ocsr = 0;

        return (1);
}

/*
 * Call the IMP module to allow it to set up its internal
 * state, then tie the two modules together by setting up
 * the back pointers to common data structures.
 */
cssattach(ui)
        register struct uba_device *ui;
{
        register struct css_softc *sc = &css_softc[ui->ui_unit];
        register struct impcb *ip;

        if ((sc->css_imp = impattach(ui->ui_driver->ud_dname, ui->ui_unit,
	    cssreset)) == 0)
                return;
	ip = &sc->css_imp->imp_cb;
        ip->ic_init = cssinit;
        ip->ic_output = cssoutput;
        ip->ic_down = cssdown;
	sc->css_ifuba.ifu_flags = UBA_CANTWAIT | UBA_NEED16;
#ifdef notdef
	sc->css_ifuba.ifu_flags |= UBA_NEEDBDP;
#endif
}

/*
 * Reset interface after UNIBUS reset.
 * If interface is on specified uba, reset its state.
 */
cssreset(unit, uban)
        int unit, uban;
{
        register struct uba_device *ui;
        register struct css_softc *sc;

        if (unit >= NCSS || (ui = cssinfo[unit]) == 0 || ui->ui_alive == 0 ||
            ui->ui_ubanum != uban)
                return;
        printf(" css%d", unit);
        sc = &css_softc[unit];
	sc->css_imp->imp_if.if_flags &= ~IFF_RUNNING;
	cssoflush(unit);
        /* must go through IMP to allow it to set state */
        (*sc->css_imp->imp_if.if_init)(sc->css_imp->imp_if.if_unit);
}

/*
 * Initialize interface: clear recorded pending operations,
 * and retrieve, and reinitialize UNIBUS resources.
 */
cssinit(unit)
        int unit;
{       
        register struct css_softc *sc;
        register struct uba_device *ui;
        register struct cssdevice *addr;
        int x, info;

	if (unit >= NCSS || (ui = cssinfo[unit]) == 0 || ui->ui_alive == 0) {
		printf("css%d: not alive\n", unit);
		return(0);
	}
	sc = &css_softc[unit];

	/*
	 * Header length is 0 to if_ubainit since we have to pass
	 * the IMP leader up to the protocol interpretaion
	 * routines.  If we had the deader length as
	 * sizeof(struct imp_leader), then the if_ routines
	 * would assume we handle it on input and output.
	 */
	
        if ((sc->css_imp->imp_if.if_flags & IFF_RUNNING) == 0 &&
	    if_ubainit(&sc->css_ifuba, ui->ui_ubanum, 0,
	    (int)btoc(IMP_RCVBUF)) == 0) {
                printf("css%d: can't initialize\n", unit);
		ui->ui_alive = 0;
		sc->css_imp->imp_if.if_flags &= ~(IFF_UP | IFF_RUNNING);
		return(0);
        }
	sc->css_imp->imp_if.if_flags |= IFF_RUNNING;
        addr = (struct cssdevice *)ui->ui_addr;

        /* reset the imp interface. */
        x = spl5();
        addr->css_icsr = CSS_CLR;
        addr->css_ocsr = CSS_CLR;
	DELAY(100);
	addr->css_icsr = 0;
	addr->css_ocsr = 0;
        addr->css_icsr = IN_HRDY;       /* close the relay */
	DELAY(5000);
        splx(x);

        /*
	 * This may hang if the imp isn't really there.
	 * Will test and verify safe operation.
	 */

	x = 500;
	while (x-- > 0) {
		if ((addr->css_icsr & (IN_HRDY|IN_IMPNR)) == IN_HRDY) 
			break;
                addr->css_icsr = IN_HRDY;	/* close the relay */
                DELAY(5000);
        }

	if (x <= 0) {
		printf("css%d: imp doesn't respond, icsr=%b\n", unit,
			CSS_INBITS, addr->css_icsr);
		goto down;
	}

        /*
         * Put up a read.  We can't restart any outstanding writes
         * until we're back in synch with the IMP (i.e. we've flushed
         * the NOOPs it throws at us).
	 * Note: IMP_RCVBUF includes the leader.
         */

        x = spl5();
        info = sc->css_ifuba.ifu_r.ifrw_info;
        addr->css_iba = (u_short)info;
        addr->css_iwc = -(IMP_RCVBUF >> 1);
        addr->css_icsr = 
                IN_HRDY | CSS_IE | IN_WEN | ((info & 0x30000) >> 12) | CSS_GO;
        splx(x);
	return(1);

down:
	ui->ui_alive = 0;
	return(0);
}

/*
 * Drop the host ready line to mark host down.
 * UNTESTED.
 */
cssdown(unit)
	int unit;
{
        register struct cssdevice *addr;

	addr = (struct cssdevice *)(cssinfo[unit]->ui_addr);
        /* reset the imp interface. */
        addr->css_icsr = CSS_CLR;
        addr->css_ocsr = CSS_CLR;
	DELAY(100);
	addr->css_icsr = 0;
	addr->css_ocsr = 0;
	cssoflush(unit);
	return (1);
}

cssoflush(unit)
	int unit;
{
	register struct css_softc *sc = &css_softc[unit];

	sc->css_imp->imp_cb.ic_oactive = 0;
	if (sc->css_ifuba.ifu_xtofree) {
		m_freem(sc->css_ifuba.ifu_xtofree);
		sc->css_ifuba.ifu_xtofree = 0;
	}
}

/*
 * Start output on an interface.
 */
cssoutput(unit, m)
        int unit;
        struct mbuf *m;
{
        int info;
        struct uba_device *ui = cssinfo[unit];
        register struct css_softc *sc = &css_softc[unit];
        register struct cssdevice *addr;

        sc->css_olen = if_wubaput(&sc->css_ifuba, m);
        /*
         * Have request mapped to UNIBUS for transmission.
         * Purge any stale data from the BDP, and start the output.
         */
	if (sc->css_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(sc->css_ifuba.ifu_uba, sc->css_ifuba.ifu_w.ifrw_bdp);
        addr = (struct cssdevice *)ui->ui_addr;
        info = sc->css_ifuba.ifu_w.ifrw_info;
        addr->css_oba = (u_short)info;
        addr->css_owc = -((sc->css_olen + 1) >> 1);
        addr->css_ocsr =
	    (u_short)(CSS_IE | OUT_ENLB | ((info & 0x30000) >> 12) | CSS_GO);
        sc->css_imp->imp_cb.ic_oactive = 1;
}

/*
 * Output interrupt handler.
 */
cssxint(unit)
{
        register struct uba_device *ui = cssinfo[unit];
        register struct css_softc *sc = &css_softc[unit];
        register struct cssdevice *addr;

        addr = (struct cssdevice *)ui->ui_addr;
        if (sc->css_imp->imp_cb.ic_oactive == 0) {
                printf("css%d: stray output interrupt csr=%b\n",
			unit, addr->css_ocsr, CSS_OUTBITS);
                return;
        }
        sc->css_imp->imp_if.if_opackets++;
        sc->css_imp->imp_cb.ic_oactive = 0;
        if (addr->css_ocsr & CSS_ERR){
                sc->css_imp->imp_if.if_oerrors++;
                printf("css%d: output error, ocsr=%b icsr=%b\n", unit,
                        addr->css_ocsr, CSS_OUTBITS,
			addr->css_icsr, CSS_INBITS);
	}
	if (sc->css_ifuba.ifu_xtofree) {
		m_freem(sc->css_ifuba.ifu_xtofree);
		sc->css_ifuba.ifu_xtofree = 0;
	}
	impstart(sc->css_imp);
}

/*
 * Input interrupt handler
 */
cssrint(unit)
{
        register struct css_softc *sc = &css_softc[unit];
        register struct cssdevice *addr;
        struct mbuf *m;
        int len, info;

        sc->css_imp->imp_if.if_ipackets++;

        /*
         * Purge BDP; flush message if error indicated.
         */

        addr = (struct cssdevice *)cssinfo[unit]->ui_addr;
	if (sc->css_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(sc->css_ifuba.ifu_uba, sc->css_ifuba.ifu_r.ifrw_bdp);
        if (addr->css_icsr & CSS_ERR) {
                printf("css%d: recv error, csr=%b\n", unit,
                    addr->css_icsr, CSS_INBITS);
                sc->css_imp->imp_if.if_ierrors++;
                sc->css_flush = 1;
        }

        if (sc->css_flush) {
                if (addr->css_icsr & IN_EOM)
                        sc->css_flush = 0;
                goto setup;
        }

        len = IMP_RCVBUF + (addr->css_iwc << 1);
	if (len < 0 || len > IMP_RCVBUF) {
		printf("css%d: bad length=%d\n", len);
		sc->css_imp->imp_if.if_ierrors++;
		goto setup;
	}

        /*
         * The offset parameter is always 0 since using
         * trailers on the ARPAnet is insane.
         */
        m = if_rubaget(&sc->css_ifuba, len, 0, &sc->css_imp->imp_if);
        if (m == 0)
                goto setup;
        if ((addr->css_icsr & IN_EOM) == 0) {
		if (sc->css_iq)
			m_cat(sc->css_iq, m);
		else
			sc->css_iq = m;
		goto setup;
	}
	if (sc->css_iq) {
		m_cat(sc->css_iq, m);
		m = sc->css_iq;
		sc->css_iq = 0;
        }
        impinput(unit, m);

setup:
        /*
         * Setup for next message.
         */
        info = sc->css_ifuba.ifu_r.ifrw_info;
        addr->css_iba = (u_short)info;
        addr->css_iwc = - (IMP_RCVBUF >> 1);
        addr->css_icsr =
                IN_HRDY | CSS_IE | IN_WEN | ((info & 0x30000) >> 12) | CSS_GO;
}
#endif
