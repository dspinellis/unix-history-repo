/*	lp.c	4.25	82/06/14	*/

#include "lp.h"
#if NLP > 0
/*
 * LP-11 Line printer driver
 *
 * This driver has been modified to work on printers where
 * leaving IENABLE set would cause continuous interrupts.
 */

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/systm.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/ubavar.h"
#include "../h/ioctl.h"
#include "../h/tty.h"

#define	LPPRI	(PZERO+8)
#define	IENABLE	0100
#define	DONE	0200
#define	ERROR	0100000
#define	LPLWAT	650
#define	LPHWAT	800

#ifndef CAD
#define MAXCOL	132
#else
#define	MAXCOL	512
#endif
#define CAP	1

#define LPUNIT(dev) (minor(dev) >> 3)

struct lpdevice {
	short	lpsr;
	short	lpbuf;
};

struct lp_softc {
	struct	clist sc_outq;
	int	sc_state;
	int	sc_physcol;
	int	sc_logcol;
	int	sc_physline;
	char	sc_flags;
	int	sc_lpchar;
	struct	buf *sc_inbuf;
} lp_softc[NLP];

struct uba_device *lpinfo[NLP];

int lpprobe(), lpattach(), lptout();
u_short lpstd[] = { 0177514 };
struct uba_driver lpdriver =
	{ lpprobe, 0, lpattach, 0, lpstd, "lp", lpinfo };

/* bits for state */
#define	OPEN		1	/* device is open */
#define	TOUT		2	/* timeout is active */
#define	MOD		4	/* device state has been modified */
#define	ASLP		8	/* awaiting draining of printer */

extern	lbolt;
int	lptout();

lpattach(ui)
	struct uba_device *ui;
{
	register struct lp_softc *sc;

	sc = &lp_softc[ui->ui_unit];
	sc->sc_lpchar = -1;
}

lpprobe(reg)
	caddr_t reg;
{
	register int br, cvec;			/* value-result */
	register struct lpdevice *lpaddr = (struct lpdevice *)reg;
#ifdef lint
	br = 0; cvec = br; br = cvec;
	lpintr(0);
#endif

#ifdef INGVAX
	br = 0x14;
	cvec = 0200;
	return (1);
#else
	lpaddr->lpsr = IENABLE;
	DELAY(5);
	lpaddr->lpsr = 0;
	return (1);
#endif
}

/*ARGSUSED*/
lpopen(dev, flag)
	dev_t dev;
	int flag;
{
	register int unit;
	register struct lpdevice *lpaddr;
	register struct lp_softc *sc;
	register struct uba_device *ui;

	if ((unit = LPUNIT(dev)) >= NLP ||
	    (sc = &lp_softc[unit])->sc_state&OPEN ||
	    (ui = lpinfo[unit]) == 0 || ui->ui_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	lpaddr = (struct lpdevice *)ui->ui_addr;
	if (lpaddr->lpsr&ERROR) {
		u.u_error = EIO;
		return;
	}
	sc->sc_state |= OPEN;
	sc->sc_inbuf = geteblk(512);
	sc->sc_flags = minor(dev) & 07;
	(void) spl4();
	if ((sc->sc_state&TOUT) == 0) {
		sc->sc_state |= TOUT;
		timeout(lptout, (caddr_t)dev, 10*hz);
	}
	(void) spl0();
	lpcanon(dev, '\f');
}

/*ARGSUSED*/
lpclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct lp_softc *sc = &lp_softc[LPUNIT(dev)];

	lpcanon(dev, '\f');
	brelse(sc->sc_inbuf);
	sc->sc_state &= ~OPEN;
}

lpwrite(dev)
	dev_t dev;
{
	register unsigned n;
	register char *cp;
	register struct lp_softc *sc = &lp_softc[LPUNIT(dev)];

	while (n = min(512, u.u_count)) {
		cp = sc->sc_inbuf->b_un.b_addr;
		iomove(cp, n, B_WRITE);
		do
			lpcanon(dev, *cp++);
		while (--n);
	}
}

lpcanon(dev, c)
	dev_t dev;
	register int c;
{
	register int logcol, physcol;
	register struct lp_softc *sc = &lp_softc[LPUNIT(dev)];

	if (sc->sc_flags&CAP) {
		register c2;

		if (c>='a' && c<='z')
			c += 'A'-'a'; else
		switch (c) {

		case '{':
			c2 = '(';
			goto esc;

		case '}':
			c2 = ')';
			goto esc;

		case '`':
			c2 = '\'';
			goto esc;

		case '|':
			c2 = '!';
			goto esc;

		case '~':
			c2 = '^';

		esc:
			lpcanon(dev, c2);
			sc->sc_logcol--;
			c = '-';
		}
	}
	logcol = sc->sc_logcol;
	physcol = sc->sc_physcol;
	if (c == ' ')
		logcol++;
	else switch(c) {

	case '\t':
		logcol = (logcol+8) & ~7;
		break;

	case '\f':
		if (sc->sc_physline == 0 && physcol == 0)
			break;
		/* fall into ... */

	case '\n':
		lpoutput(dev, c);
		if (c == '\f')
			sc->sc_physline = 0;
		else
			sc->sc_physline++;
		physcol = 0;
		/* fall into ... */

	case '\r':
		logcol = 0;
		(void) spl4();
		lpintr(LPUNIT(dev));
		(void) spl0();
		break;

	case '\b':
		if (logcol > 0)
			logcol--;
		break;

	default:
		if (logcol < physcol) {
			lpoutput(dev, '\r');
			physcol = 0;
		}
		if (logcol < MAXCOL) {
			while (logcol > physcol) {
				lpoutput(dev, ' ');
				physcol++;
			}
			lpoutput(dev, c);
			physcol++;
		}
		logcol++;
	}
	if (logcol > 1000)	/* ignore long lines  */
		logcol = 1000;
	sc->sc_logcol = logcol;
	sc->sc_physcol = physcol;
}

lpoutput(dev, c)
	dev_t dev;
	int c;
{
	register struct lp_softc *sc = &lp_softc[LPUNIT(dev)];

	if (sc->sc_outq.c_cc >= LPHWAT) {
		(void) spl4();
		lpintr(LPUNIT(dev));				/* unchoke */
		while (sc->sc_outq.c_cc >= LPHWAT) {
			sc->sc_state |= ASLP;		/* must be ERROR */
			sleep((caddr_t)sc, LPPRI);
		}
		(void) spl0();
	}
	while (putc(c, &sc->sc_outq))
		sleep((caddr_t)&lbolt, LPPRI);
}

lpintr(lp11)
	int lp11;
{
	register int n;
	register struct lp_softc *sc = &lp_softc[lp11];
	register struct uba_device *ui = lpinfo[lp11];
	register struct lpdevice *lpaddr = (struct lpdevice *)ui->ui_addr;

	lpaddr->lpsr &= ~IENABLE;
	n = sc->sc_outq.c_cc;
	if (sc->sc_lpchar < 0)
		sc->sc_lpchar = getc(&sc->sc_outq);
	while ((lpaddr->lpsr&DONE) && sc->sc_lpchar >= 0) {
		lpaddr->lpbuf = sc->sc_lpchar;
		sc->sc_lpchar = getc(&sc->sc_outq);
	}
	sc->sc_state |= MOD;
	if (sc->sc_outq.c_cc > 0 && (lpaddr->lpsr&ERROR)==0)
		lpaddr->lpsr |= IENABLE;	/* ok and more to do later */
	if (n>LPLWAT && sc->sc_outq.c_cc<=LPLWAT && sc->sc_state&ASLP) {
		sc->sc_state &= ~ASLP;
		wakeup((caddr_t)sc);		/* top half should go on */
	}
}

lptout(dev)
	dev_t dev;
{
	register struct lp_softc *sc;
	register struct uba_device *ui;
	register struct lpdevice *lpaddr;

	sc = &lp_softc[LPUNIT(dev)];
	ui = lpinfo[LPUNIT(dev)];
	lpaddr = (struct lpdevice *) ui->ui_addr;
	if ((sc->sc_state&MOD) != 0) {
		sc->sc_state &= ~MOD;		/* something happened */
		timeout(lptout, (caddr_t)dev, 2*hz);	/* so don't sweat */
		return;
	}
	if ((sc->sc_state&OPEN) == 0) {
		sc->sc_state &= ~TOUT;		/* no longer open */
		lpaddr->lpsr = 0;
		return;
	}
	if (sc->sc_outq.c_cc && (lpaddr->lpsr&DONE) && (lpaddr->lpsr&ERROR)==0)
		lpintr(LPUNIT(dev));			/* ready to go */
	timeout(lptout, (caddr_t)dev, 10*hz);
}

lpreset(uban)
	int uban;
{
	register struct uba_device *ui;
	register struct lpdevice *lpaddr;
	register int unit;

	for (unit = 0; unit < NLP; unit++) {
		if ((ui = lpinfo[unit]) == 0 || ui->ui_ubanum != uban ||
		    ui->ui_alive == 0)
			continue;
		printf(" lp%d", unit);
		lpaddr = (struct lpdevice *)ui->ui_addr;
		lpaddr->lpsr |= IENABLE;
	}
}
