/*	lp.c	4.6	12/31/80	*/

#include "lp.h"
#if NLP > 0
/*
 * LP-11 Line printer driver
 *
 * This driver is only set up to handle one printer;
 * thats all our user-level spoolers can handle anyways right now.
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
#include "../h/uba.h"
#include "../h/ioctl.h"
#include "../h/tty.h"
#include "../h/lpio.h"

#define	LPPRI	(PZERO+8)
#define	IENABLE	0100
#define	DONE	0200
#define	ERROR	0100000
#define	LPLWAT	650
#define	LPHWAT	800

struct lpregs {
	short	lpsr;
	short	lpbuf;
};

struct {
	struct	clist outq;
	int	state;
	int	physcol;
	int	logcol;
	int	physline;
	struct	lpioctl lpio;
	struct	buf *inbuf;
} lp11;
#define	flags	lpio.lp_flags
#define	indent	lpio.lp_indent
#define	maxcol	lpio.lp_maxcol

/* bits for state */
#define	OPEN		1	/* device is open */
#define	TOUT		2	/* timeout is active */
#define	MOD		4	/* device state has been modified */
#define	ASLP		8	/* awaiting draining of printer */

extern	lbolt;
int	lptout();

/*ARGSUSED*/
lpopen(dev, flag)
{

	if (lp11.state&OPEN || LPADDR->lpsr&ERROR) {
		u.u_error = EIO;
		return;
	}
	lp11.state |= OPEN;
	lp11.inbuf = geteblk();
	lp11.flags = LPFLAGS;
	lp11.indent = INDENT;
	lp11.maxcol = MAXCOL;
	spl4();
	if ((lp11.state&TOUT) == 0) {
		lp11.state |= TOUT;
		timeout(lptout, 0, 10*HZ);
	}
	spl0();
	lpcanon('\f');
}

/*ARGSUSED*/
lpclose(dev, flag)
{

	lpcanon('\f');
	brelse(lp11.inbuf);
	lp11.state &= ~OPEN;
}

lpwrite()
{
	register c, n;
	register char *cp;

	while (n = min(BSIZE, u.u_count)) {
		cp = lp11.inbuf->b_un.b_addr;
		iomove(cp, n, B_WRITE);
		do
			lpcanon(*cp++);
		while (--n);
	}
}

lpcanon(c)
register c;
{
	register int logcol, physcol;

#ifdef HALFASCII
	if (lp11.flags&CAP) {
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
			lpcanon(c2);
			lp11.logcol--;
			c = '-';
		}
	}
#endif HALFASCII
	logcol = lp11.logcol;
	physcol = lp11.physcol;
	if (c == ' ')
		logcol++;
	else switch(c) {

	case '\t':
		logcol = lp11.indent + ((logcol-lp11.indent+8) & ~7);
		break;

	case '\f':
		if (lp11.physline == 0 && physcol == 0)
			break;
		/* fall into ... */

	case '\n':
		lpoutput(c);
		if (c == '\f')
			lp11.physline = 0;
		else
			lp11.physline++;
		physcol = 0;
		/* fall into ... */

	case '\r':
		logcol = lp11.indent;
		spl4();
		lpintr();
		spl0();
		break;

	case '\b':
		if (logcol > 0)
			logcol--;
		break;

	default:
		if (logcol < physcol) {
			lpoutput('\r');
			physcol = 0;
		}
		if (logcol < lp11.maxcol) {
			while (logcol > physcol) {
				lpoutput(' ');
				physcol++;
			}
			lpoutput(c);
			physcol++;
		}
		logcol++;
	}
	if (logcol > 1000)	/* ignore long lines  */
		logcol = 1000;
	lp11.logcol = logcol;
	lp11.physcol = physcol;
}

lpoutput(c)
{

	if (lp11.outq.c_cc >= LPHWAT) {
		spl4();
		lpintr();				/* unchoke */
		while (lp11.outq.c_cc >= LPHWAT) {
			lp11.state |= ASLP;		/* must be ERROR */
			sleep((caddr_t)&lp11, LPPRI);
		}
		spl0();
	}
	while (putc(c, &lp11.outq))
		sleep((caddr_t)&lbolt, LPPRI);
}

int	lpchar = -1;

lpintr()
{
	register int n;
	int i;

	LPADDR->lpsr &= ~IENABLE;
	n = lp11.outq.c_cc;
	if (lpchar < 0)
		lpchar = getc(&lp11);
	while ((LPADDR->lpsr&DONE) && lpchar >= 0) {
		LPADDR->lpbuf = lpchar;
		lpchar = getc(&lp11);
	}
	lp11.state |= MOD;
	if (lp11.outq.c_cc > 0 && (LPADDR->lpsr&ERROR)==0)
		LPADDR->lpsr |= IENABLE;	/* ok and more to do later */
	if (n>LPLWAT && lp11.outq.c_cc<=LPLWAT && lp11.state&ASLP) {
		lp11.state &= ~ASLP;
		wakeup((caddr_t)&lp11);		/* top half should go on */
	}
}

lptout()
{
	register short *sr;

	if ((lp11.state&MOD) != 0) {
		lp11.state &= ~MOD;		/* something happened */
		timeout(lptout, 0, 2*HZ);	/* so don't sweat */
		return;
	}
	sr = &LPADDR->lpsr;
	if ((lp11.state&OPEN) == 0) {
		lp11.state &= ~TOUT;		/* no longer open */
		*sr = 0;
		return;
	}
	if (lp11.outq.c_cc && (*sr&DONE) && (*sr&ERROR)==0)
		lpintr();			/* ready to go */
	timeout(lptout, 0, 10*HZ);
}

/*ARGSUSED*/
lpioctl(dev, cmd, addr, flag)
	dev_t dev;
	caddr_t addr;
{
	register int m;
	struct lpioctl lpio;

	switch (cmd) {

	case LGETSTATE:
		copyout((caddr_t)&lp11.lpio, addr, sizeof (lp11.lpio));
		return;

	case LSETSTATE:
		m = copyin(addr, (caddr_t)&lpio, sizeof (lpio));
		if (m < 0) {
			u.u_error = EFAULT;
			return;
		}
		if (lpio.lp_indent <= 0 || lpio.lp_indent >= lpio.lp_maxcol ||
		    lpio.lp_ejline <= 2 || lpio.lp_ejline <= lpio.lp_skpline ||
		    lpio.lp_skpline < 0 || lpio.lp_maxcol <= 10)
			u.u_error = EINVAL;
		else
			lp11.lpio = lpio;
		return;

	default:
		u.u_error = ENOTTY;
		return;
	}
}

lpreset()
{

	printf("lp ");
	LPADDR->lpsr |= IENABLE;
}

