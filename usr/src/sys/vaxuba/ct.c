/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ct.c	7.1 (Berkeley) 6/5/86
 */

#include "ct.h"
#if NCT > 0
/*
 * GP DR11C driver used for C/A/T or Autologic APS micro-5
 */
#include "../machine/pte.h"

#include "param.h"
#include "systm.h"
#include "ioctl.h"
#include "tty.h"
#include "map.h"
#include "buf.h"
#include "conf.h"
#include "dir.h"
#include "user.h"
#include "kernel.h"

#include "ubareg.h"
#include "ubavar.h"

#define	PCAT	(PZERO+9)
#define	CATHIWAT	100
#define	CATLOWAT	30

#define	REQUEST_B	0x8000
#define REQUEST_A	0x80
#define	INT_ENB_A	0x40
#define	INT_ENB_B	0x20
#define	CSR1		0x2
#define	CSR0		0x1

struct ct_softc {
	int	sc_state;
	struct	clist sc_oq;
} ct_softc[NCT];

#define	CT_OPEN		0x1
#define	CT_RUNNING	0x2

struct ctdevice {
	u_short	ctcsr;
	u_short	ctobuf;
	u_short ctibuf;
};

int	ctprobe(), ctattach(), ctintr();
struct	uba_device *ctdinfo[NCT];
u_short	ctstd[] = { 0167770, 0 };
struct	uba_driver ctdriver = 
    { ctprobe, 0, ctattach, 0, ctstd, "ct", ctdinfo };

#define	CTUNIT(dev)	(minor(dev))

int	ct_init	= 0;	/* set to CSR1 for testing loopback on controller */

ctprobe(reg)
	caddr_t reg;
{
	register int br, cvec;		/* value-result */
	register struct ctdevice *ctaddr = (struct ctdevice *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	ctintr(0);
#endif
	/*
	 * There is no way to make a DR11c interrupt without some
	 * external support. We can't always trust that the typesetter
	 * will be online and ready so we've made other provisions.
	 * This probe assumes setting the B Int Enb will generate
	 * an interrupt. To do this, we set CSR0 and loop this back
	 * to REQUEST_B in the second plug on the controller.
	 * Then, we reset the vector to be that for the "real" device.
	 */
	ctaddr->ctcsr = INT_ENB_B | CSR0; /* Assume hardware loopback! */
	DELAY(1000);
	ctaddr->ctcsr = ct_init; /* should be CSR1 for loopback testing */
	if (cvec & 04) {
		printf("ct: resetting vector %o to %o\n", cvec, cvec&0773);
		cvec &= 0773;
	}
	return (sizeof (struct ctdevice));
}

/*ARGSUSED*/
ctattach(ui)
	struct uba_device *ui;
{
}

ctopen(dev)
	dev_t dev;
{
	register struct ct_softc *sc;
	register struct uba_device *ui;
	register struct ctdevice *ctaddr;

	if (CTUNIT(dev) >= NCT || (ui = ctdinfo[CTUNIT(dev)]) == 0 ||
	    ui->ui_alive == 0)
		return (ENODEV);
	if ((sc = &ct_softc[CTUNIT(dev)])->sc_state&CT_OPEN)
		return (EBUSY);
	sc->sc_state = CT_OPEN;
	ctaddr = (struct ctdevice *)ui->ui_addr;
	ctaddr->ctcsr |= INT_ENB_A;
	return (0);
}

ctclose(dev)
	dev_t dev;
{
	ct_softc[CTUNIT(dev)].sc_state = 0;
	ctintr(dev);
	return (0);
}

ctwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct ct_softc *sc = &ct_softc[CTUNIT(dev)];
	register int c;
	int s;

	while ((c = uwritec(uio)) >= 0) {
		s = spl5();
		while (sc->sc_oq.c_cc > CATHIWAT)
			sleep((caddr_t)&sc->sc_oq, PCAT);
		while (putc(c, &sc->sc_oq) < 0)
			sleep((caddr_t)&lbolt, PCAT);
		if ( ! (sc->sc_state & CT_RUNNING) )
			ctintr(dev);
		splx(s);
	}
	return (0);
}

/*
 * The C/A/T is usually wired to accept data on the .5us DATA_AVAIL strobe.
 * If you use this with a C/A/T you can remove the lines with "APSu5" below.
 * This is way out of spec for the Autologic APS micro-5 which requires
 * at least a 40 microsec strobe. We therefore use CSR1 output as the
 * "strobe". It is set after data is loaded and reset only in the
 * interrupt routine. Therefore, the "strobe" is high for adequate time.
 * The constant "ctdelay" determines the "low" time for the strobe
 * and may have to be larger on a 780. "2" gives about 10us on a 750.
 */
int	ctdelay	= 2;	/* here so it's visible & changeable */

ctintr(dev)
	dev_t dev;
{
	register int c;
	register struct ct_softc *sc = &ct_softc[CTUNIT(dev)];
	register struct ctdevice *ctaddr =
	    (struct ctdevice *)ctdinfo[CTUNIT(dev)]->ui_addr;

	if ((ctaddr->ctcsr&(INT_ENB_B|REQUEST_B)) == (INT_ENB_B|REQUEST_B)) {
		ctaddr->ctcsr &= ~(CSR0 | INT_ENB_B);	/* set in ctprobe */
	}
	if ((ctaddr->ctcsr&(INT_ENB_A|REQUEST_A)) == (INT_ENB_A|REQUEST_A)) {
		if ((c = getc(&sc->sc_oq)) >= 0) {
			ctaddr->ctcsr &= ~CSR1;	/* APSu5 - drop strobe */
			ctaddr->ctobuf = c;
			DELAY(ctdelay);		/* APSu5 - pause a bit */
			ctaddr->ctcsr |= CSR1;	/* APSu5 - raise strobe */
			sc->sc_state |= CT_RUNNING;
			if (sc->sc_oq.c_cc==0 || sc->sc_oq.c_cc==CATLOWAT)
				wakeup((caddr_t)&sc->sc_oq);
		} else if (sc->sc_state == 0) {
				ctaddr->ctcsr = 0;
		} else
			sc->sc_state &= ~CT_RUNNING;
	}
}
#endif
