/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell and Rick Macklem.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dc.c	7.12 (Berkeley) %G%
 */

/*
 * devDC7085.c --
 *
 *     	This file contains machine-dependent routines that handle the
 *	output queue for the serial lines.
 *
 *	Copyright (C) 1989 Digital Equipment Corporation.
 *	Permission to use, copy, modify, and distribute this software and
 *	its documentation for any purpose and without fee is hereby granted,
 *	provided that the above copyright notice appears in all copies.
 *	Digital Equipment Corporation makes no representations about the
 *	suitability of this software for any purpose.  It is provided "as is"
 *	without express or implied warranty.
 *
 * from: $Header: /sprite/src/kernel/dev/ds3100.md/RCS/devDC7085.c,
 *	v 1.4 89/08/29 11:55:30 nelson Exp $ SPRITE (DECWRL)";
 */

#include <dc.h>
#if NDC > 0
/*
 * DC7085 (DZ-11 look alike) Driver
 */
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/proc.h>
#include <sys/map.h>
#include <sys/buf.h>
#include <sys/conf.h>
#include <sys/file.h>
#include <sys/uio.h>
#include <sys/kernel.h>
#include <sys/syslog.h>

#include <machine/dc7085cons.h>
#include <machine/pmioctl.h>

#include <pmax/pmax/pmaxtype.h>
#include <pmax/pmax/cons.h>

#include <pmax/dev/device.h>
#include <pmax/dev/pdma.h>
#include <pmax/dev/fbreg.h>

extern int pmax_boardtype;
extern struct consdev cn_tab;

/*
 * Driver information for auto-configuration stuff.
 */
int	dcprobe();
void	dcintr();
struct	driver dcdriver = {
	"dc", dcprobe, 0, 0, dcintr,
};

#define	NDCLINE 	(NDC*4)

void dcstart	__P((struct tty *));
void dcxint	__P((struct tty *));
void dcPutc	__P((dev_t, int));
void dcscan	__P((void *));
extern void ttrstrt __P((void *));
int dcGetc	__P((dev_t));
int dcparam	__P((struct tty *, struct termios *));
extern void KBDReset	__P((dev_t, void (*)()));
extern void MouseInit	__P((dev_t, void (*)(), int (*)()));

struct	tty dc_tty[NDCLINE];
int	dc_cnt = NDCLINE;
void	(*dcDivertXInput)();	/* X windows keyboard input routine */
void	(*dcMouseEvent)();	/* X windows mouse motion event routine */
void	(*dcMouseButtons)();	/* X windows mouse buttons event routine */
#ifdef DEBUG
int	debugChar;
#endif

/*
 * Software copy of brk register since it isn't readable
 */
int	dc_brk[NDC];
char	dcsoftCAR[NDC];		/* mask of dc's with carrier on (DSR) */

/*
 * The DC7085 doesn't interrupt on carrier transitions, so
 * we have to use a timer to watch it.
 */
int	dc_timer;		/* true if timer started */

/*
 * Pdma structures for fast output code
 */
struct	pdma dcpdma[NDCLINE];

struct speedtab dcspeedtab[] = {
	0,	0,
	50,	LPR_B50,
	75,	LPR_B75,
	110,	LPR_B110,
	134,	LPR_B134,
	150,	LPR_B150,
	300,	LPR_B300,
	600,	LPR_B600,
	1200,	LPR_B1200,
	1800,	LPR_B1800,
	2400,	LPR_B2400,
	4800,	LPR_B4800,
	9600,	LPR_B9600,
	19200,	LPR_B19200,
	-1,	-1
};

#ifndef	PORTSELECTOR
#define	ISPEED	TTYDEF_SPEED
#define	LFLAG	TTYDEF_LFLAG
#else
#define	ISPEED	B4800
#define	LFLAG	(TTYDEF_LFLAG & ~ECHO)
#endif

/*
 * Test to see if device is present.
 * Return true if found and initialized ok.
 */
dcprobe(cp)
	register struct pmax_ctlr *cp;
{
	register dcregs *dcaddr;
	register struct pdma *pdp;
	register struct tty *tp;
	register int cntr;
	int s;

	if (cp->pmax_unit >= NDC)
		return (0);
	if (badaddr(cp->pmax_addr, 2))
		return (0);

	/*
	 * For a remote console, wait a while for previous output to
	 * complete.
	 */
	if (major(cn_tab.cn_dev) == DCDEV && cp->pmax_unit == 0 &&
		cn_tab.cn_screen == 0)
		DELAY(10000);

	/* reset chip */
	dcaddr = (dcregs *)cp->pmax_addr;
	dcaddr->dc_csr = CSR_CLR;
	MachEmptyWriteBuffer();
	while (dcaddr->dc_csr & CSR_CLR)
		;
	dcaddr->dc_csr = CSR_MSE | CSR_TIE | CSR_RIE;

	/* init pseudo DMA structures */
	pdp = &dcpdma[cp->pmax_unit * 4];
	tp = &dc_tty[cp->pmax_unit * 4];
	for (cntr = 0; cntr < 4; cntr++) {
		pdp->p_addr = (void *)dcaddr;
		pdp->p_arg = (int)tp;
		pdp->p_fcn = dcxint;
		tp->t_addr = (caddr_t)pdp;
		pdp++, tp++;
	}
	dcsoftCAR[cp->pmax_unit] = cp->pmax_flags | 0xB;

	if (dc_timer == 0) {
		dc_timer = 1;
		timeout(dcscan, (void *)0, hz);
	}

	/*
	 * Special handling for consoles.
	 */
	if (cp->pmax_unit == 0) {
		if (cn_tab.cn_screen) {
			s = spltty();
			dcaddr->dc_lpr = LPR_RXENAB | LPR_8_BIT_CHAR |
				LPR_B4800 | DCKBD_PORT;
			dcaddr->dc_lpr = LPR_RXENAB | LPR_B4800 | LPR_OPAR |
				LPR_PARENB | LPR_8_BIT_CHAR | DCMOUSE_PORT;
			MachEmptyWriteBuffer();
			DELAY(1000);
			KBDReset(makedev(DCDEV, DCKBD_PORT), dcPutc);
			MouseInit(makedev(DCDEV, DCMOUSE_PORT), dcPutc, dcGetc);
			splx(s);
		} else if (major(cn_tab.cn_dev) == DCDEV) {
			s = spltty();
			dcaddr->dc_lpr = LPR_RXENAB | LPR_8_BIT_CHAR |
				LPR_B9600 | minor(cn_tab.cn_dev);
			MachEmptyWriteBuffer();
			DELAY(1000);
			cn_tab.cn_disabled = 0;
			splx(s);
		}
	}
	printf("dc%d at nexus0 csr 0x%x priority %d\n",
		cp->pmax_unit, cp->pmax_addr, cp->pmax_pri);
	return (1);
}

dcopen(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct tty *tp;
	register int unit;
	int s, error = 0;

	unit = minor(dev);
	if (unit >= dc_cnt || dcpdma[unit].p_addr == (void *)0)
		return (ENXIO);
	tp = &dc_tty[unit];
	tp->t_addr = (caddr_t)&dcpdma[unit];
	tp->t_oproc = dcstart;
	tp->t_param = dcparam;
	tp->t_dev = dev;
	if ((tp->t_state & TS_ISOPEN) == 0) {
		tp->t_state |= TS_WOPEN;
		ttychars(tp);
#ifndef PORTSELECTOR
		if (tp->t_ispeed == 0) {
#endif
			tp->t_iflag = TTYDEF_IFLAG;
			tp->t_oflag = TTYDEF_OFLAG;
			tp->t_cflag = TTYDEF_CFLAG;
			tp->t_lflag = LFLAG;
			tp->t_ispeed = tp->t_ospeed = ISPEED;
#ifdef PORTSELECTOR
			tp->t_cflag |= HUPCL;
#else
		}
#endif
		(void) dcparam(tp, &tp->t_termios);
		ttsetwater(tp);
	} else if ((tp->t_state & TS_XCLUDE) && curproc->p_ucred->cr_uid != 0)
		return (EBUSY);
	(void) dcmctl(dev, DML_DTR, DMSET);
	s = spltty();
	while (!(flag & O_NONBLOCK) && !(tp->t_cflag & CLOCAL) &&
	       !(tp->t_state & TS_CARR_ON)) {
		tp->t_state |= TS_WOPEN;
		if (error = ttysleep(tp, (caddr_t)&tp->t_rawq, TTIPRI | PCATCH,
		    ttopen, 0))
			break;
	}
	splx(s);
	if (error)
		return (error);
	return ((*linesw[tp->t_line].l_open)(dev, tp));
}

/*ARGSUSED*/
dcclose(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct tty *tp;
	register int unit, bit;

	unit = minor(dev);
	tp = &dc_tty[unit];
	bit = 1 << ((unit & 03) + 8);
	if (dc_brk[unit >> 2] & bit) {
		dc_brk[unit >> 2] &= ~bit;
		ttyoutput(0, tp);
	}
	(*linesw[tp->t_line].l_close)(tp, flag);
	if ((tp->t_cflag & HUPCL) || (tp->t_state & TS_WOPEN) ||
	    !(tp->t_state & TS_ISOPEN))
		(void) dcmctl(dev, 0, DMSET);
	return (ttyclose(tp));
}

dcread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;

	tp = &dc_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}

dcwrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;

	tp = &dc_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}

/*ARGSUSED*/
dcioctl(dev, cmd, data, flag, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
	struct proc *p;
{
	register struct tty *tp;
	register int unit = minor(dev);
	register int dc = unit >> 2;
	int error;

	tp = &dc_tty[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag, p);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	switch (cmd) {

	case TIOCSBRK:
		dc_brk[dc] |= 1 << ((unit & 03) + 8);
		ttyoutput(0, tp);
		break;

	case TIOCCBRK:
		dc_brk[dc] &= ~(1 << ((unit & 03) + 8));
		ttyoutput(0, tp);
		break;

	case TIOCSDTR:
		(void) dcmctl(dev, DML_DTR|DML_RTS, DMBIS);
		break;

	case TIOCCDTR:
		(void) dcmctl(dev, DML_DTR|DML_RTS, DMBIC);
		break;

	case TIOCMSET:
		(void) dcmctl(dev, *(int *)data, DMSET);
		break;

	case TIOCMBIS:
		(void) dcmctl(dev, *(int *)data, DMBIS);
		break;

	case TIOCMBIC:
		(void) dcmctl(dev, *(int *)data, DMBIC);
		break;

	case TIOCMGET:
		*(int *)data = dcmctl(dev, 0, DMGET);
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

dcparam(tp, t)
	register struct tty *tp;
	register struct termios *t;
{
	register dcregs *dcaddr;
	register int lpr;
	register int cflag = t->c_cflag;
	int unit = minor(tp->t_dev);
	int ospeed = ttspeedtab(t->c_ospeed, dcspeedtab);

	/* check requested parameters */
        if (ospeed < 0 || (t->c_ispeed && t->c_ispeed != t->c_ospeed) ||
            (cflag & CSIZE) == CS5 || (cflag & CSIZE) == CS6 ||
	    (pmax_boardtype == DS_PMAX && t->c_ospeed == 19200))
                return (EINVAL);
        /* and copy to tty */
        tp->t_ispeed = t->c_ispeed;
        tp->t_ospeed = t->c_ospeed;
        tp->t_cflag = cflag;

	dcaddr = (dcregs *)dcpdma[unit].p_addr;

	/*
	 * Handle console cases specially.
	 */
	if (cn_tab.cn_screen) {
		if (unit == DCKBD_PORT) {
			dcaddr->dc_lpr = LPR_RXENAB | LPR_8_BIT_CHAR |
				LPR_B4800 | DCKBD_PORT;
			MachEmptyWriteBuffer();
			return (0);
		} else if (unit == DCMOUSE_PORT) {
			dcaddr->dc_lpr = LPR_RXENAB | LPR_B4800 | LPR_OPAR |
				LPR_PARENB | LPR_8_BIT_CHAR | DCMOUSE_PORT;
			MachEmptyWriteBuffer();
			return (0);
		}
	} else if (tp->t_dev == cn_tab.cn_dev) {
		dcaddr->dc_lpr = LPR_RXENAB | LPR_8_BIT_CHAR |
			LPR_B9600 | unit;
		MachEmptyWriteBuffer();
		return (0);
	}
	if (ospeed == 0) {
		(void) dcmctl(unit, 0, DMSET);	/* hang up line */
		return (0);
	}
	lpr = LPR_RXENAB | ospeed | (unit & 03);
	if ((cflag & CSIZE) == CS7)
		lpr |= LPR_7_BIT_CHAR;
	else
		lpr |= LPR_8_BIT_CHAR;
	if (cflag & PARENB)
		lpr |= LPR_PARENB;
	if (cflag & PARODD)
		lpr |= LPR_OPAR;
	if (cflag & CSTOPB)
		lpr |= LPR_2_STOP;
	dcaddr->dc_lpr = lpr;
	MachEmptyWriteBuffer();
	return (0);
}

/*
 * Check for interrupts from all devices.
 */
void
dcintr(unit)
	register int unit;
{
	register dcregs *dcaddr;
	register unsigned csr;

	unit <<= 2;
	dcaddr = (dcregs *)dcpdma[unit].p_addr;
	while ((csr = dcaddr->dc_csr) & (CSR_RDONE | CSR_TRDY)) {
		if (csr & CSR_RDONE)
			dcrint(unit);
		if (csr & CSR_TRDY)
			dcxint(&dc_tty[unit + ((csr >> 8) & 03)]);
	}
}

dcrint(unit)
	register int unit;
{
	register dcregs *dcaddr;
	register struct tty *tp;
	register int c, cc;
	register struct tty *tp0;
	int overrun = 0;

	dcaddr = (dcregs *)dcpdma[unit].p_addr;
	tp0 = &dc_tty[unit];
	while ((c = dcaddr->dc_rbuf) < 0) {	/* char present */
		cc = c & 0xff;
		tp = tp0 + ((c >> 8) & 03);
		if ((c & RBUF_OERR) && overrun == 0) {
			log(LOG_WARNING, "dc%d,%d: silo overflow\n", unit >> 2,
				(c >> 8) & 03);
			overrun = 1;
		}
		/* the keyboard requires special translation */
		if (tp == &dc_tty[DCKBD_PORT] && cn_tab.cn_screen) {
#ifdef KADB
			if (cc == LK_DO) {
				spl0();
				kdbpanic();
				return;
			}
#endif
#ifdef DEBUG
			debugChar = cc;
#endif
			if (dcDivertXInput) {
				(*dcDivertXInput)(cc);
				return;
			}
			if ((cc = kbdMapChar(cc)) < 0)
				return;
		} else if (tp == &dc_tty[DCMOUSE_PORT] && dcMouseButtons) {
			register MouseReport *mrp;
			static MouseReport currentRep;

			mrp = &currentRep;
			mrp->byteCount++;
			if (cc & MOUSE_START_FRAME) {
				/*
				 * The first mouse report byte (button state).
				 */
				mrp->state = cc;
				if (mrp->byteCount > 1)
					mrp->byteCount = 1;
			} else if (mrp->byteCount == 2) {
				/*
				 * The second mouse report byte (delta x).
				 */
				mrp->dx = cc;
			} else if (mrp->byteCount == 3) {
				/*
				 * The final mouse report byte (delta y).
				 */
				mrp->dy = cc;
				mrp->byteCount = 0;
				if (mrp->dx != 0 || mrp->dy != 0) {
					/*
					 * If the mouse moved,
					 * post a motion event.
					 */
					(*dcMouseEvent)(mrp);
				}
				(*dcMouseButtons)(mrp);
			}
			return;
		}
		if (!(tp->t_state & TS_ISOPEN)) {
			wakeup((caddr_t)&tp->t_rawq);
#ifdef PORTSELECTOR
			if (!(tp->t_state & TS_WOPEN))
#endif
				return;
		}
		if (c & RBUF_FERR)
			cc |= TTY_FE;
		if (c & RBUF_PERR)
			cc |= TTY_PE;
		(*linesw[tp->t_line].l_rint)(cc, tp);
	}
	DELAY(10);
}

void
dcxint(tp)
	register struct tty *tp;
{
	register struct pdma *dp;
	register dcregs *dcaddr;

	dp = (struct pdma *)tp->t_addr;
	if (dp->p_mem < dp->p_end) {
		dcaddr = (dcregs *)dp->p_addr;
		dcaddr->dc_tdr = dc_brk[(tp - dc_tty) >> 2] | *dp->p_mem++;
		MachEmptyWriteBuffer();
		DELAY(10);
		return;
	}
	tp->t_state &= ~TS_BUSY;
	if (tp->t_state & TS_FLUSH)
		tp->t_state &= ~TS_FLUSH;
	else {
		ndflush(&tp->t_outq, dp->p_mem-tp->t_outq.c_cf);
		dp->p_end = dp->p_mem = tp->t_outq.c_cf;
	}
	if (tp->t_line)
		(*linesw[tp->t_line].l_start)(tp);
	else
		dcstart(tp);
	if (tp->t_outq.c_cc == 0 || !(tp->t_state & TS_BUSY)) {
		((dcregs *)dp->p_addr)->dc_tcr &= ~(1 << (minor(tp->t_dev) & 03));
		MachEmptyWriteBuffer();
		DELAY(10);
	}
}

void
dcstart(tp)
	register struct tty *tp;
{
	register struct pdma *dp;
	register dcregs *dcaddr;
	register int cc;
	int s;

	dp = (struct pdma *)tp->t_addr;
	dcaddr = (dcregs *)dp->p_addr;
	s = spltty();
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;
	if (tp->t_outq.c_cc <= tp->t_lowat) {
		if (tp->t_state & TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		selwakeup(&tp->t_wsel);
	}
	if (tp->t_outq.c_cc == 0)
		goto out;
	/* handle console specially */
	if (tp == &dc_tty[DCKBD_PORT] && cn_tab.cn_screen) {
		while (tp->t_outq.c_cc > 0) {
			cc = getc(&tp->t_outq) & 0x7f;
			cnputc(cc);
		}
		/*
		 * After we flush the output queue we may need to wake
		 * up the process that made the output.
		 */
		if (tp->t_outq.c_cc <= tp->t_lowat) {
			if (tp->t_state & TS_ASLEEP) {
				tp->t_state &= ~TS_ASLEEP;
				wakeup((caddr_t)&tp->t_outq);
			}
			selwakeup(&tp->t_wsel);
		}
		goto out;
	}
	if (tp->t_flags & (RAW|LITOUT))
		cc = ndqb(&tp->t_outq, 0);
	else {
		cc = ndqb(&tp->t_outq, 0200);
		if (cc == 0) {
			cc = getc(&tp->t_outq);
			timeout(ttrstrt, (void *)tp, (cc & 0x7f) + 6);
			tp->t_state |= TS_TIMEOUT;
			goto out;
		}
	}
	tp->t_state |= TS_BUSY;
	dp->p_end = dp->p_mem = tp->t_outq.c_cf;
	dp->p_end += cc;
	dcaddr->dc_tcr |= 1 << (minor(tp->t_dev) & 03);
	MachEmptyWriteBuffer();
out:
	splx(s);
}

/*
 * Stop output on a line.
 */
/*ARGSUSED*/
dcstop(tp, flag)
	register struct tty *tp;
{
	register struct pdma *dp;
	register int s;

	dp = (struct pdma *)tp->t_addr;
	s = spltty();
	if (tp->t_state & TS_BUSY) {
		dp->p_end = dp->p_mem;
		if (!(tp->t_state & TS_TTSTOP))
			tp->t_state |= TS_FLUSH;
	}
	splx(s);
}

dcmctl(dev, bits, how)
	dev_t dev;
	int bits, how;
{
	register dcregs *dcaddr;
	register int unit, mbits;
	int b, s;
	register int msr;

	unit = minor(dev);
	b = 1 << (unit & 03);
	dcaddr = (dcregs *)dcpdma[unit].p_addr;
	s = spltty();
	/* only channel 2 has modem control (what about line 3?) */
	mbits = DML_DTR | DML_DSR | DML_CAR;
	switch (unit & 03) {
	case 2:
		mbits = 0;
		if (dcaddr->dc_tcr & TCR_DTR2)
			mbits |= DML_DTR;
		msr = dcaddr->dc_msr;
		if (msr & MSR_CD2)
			mbits |= DML_CAR;
		if (msr & MSR_DSR2) {
			if (pmax_boardtype == DS_PMAX)
				mbits |= DML_CAR | DML_DSR;
			else
				mbits |= DML_DSR;
		}
		break;

	case 3:
		if (pmax_boardtype != DS_PMAX) {
			mbits = 0;
			if (dcaddr->dc_tcr & TCR_DTR3)
				mbits |= DML_DTR;
			msr = dcaddr->dc_msr;
			if (msr & MSR_CD3)
				mbits |= DML_CAR;
			if (msr & MSR_DSR3)
				mbits |= DML_DSR;
		}
	}
	switch (how) {
	case DMSET:
		mbits = bits;
		break;

	case DMBIS:
		mbits |= bits;
		break;

	case DMBIC:
		mbits &= ~bits;
		break;

	case DMGET:
		(void) splx(s);
		return (mbits);
	}
	switch (unit & 03) {
	case 2:
		if (mbits & DML_DTR)
			dcaddr->dc_tcr |= TCR_DTR2;
		else
			dcaddr->dc_tcr &= ~TCR_DTR2;
		break;

	case 3:
		if (pmax_boardtype != DS_PMAX) {
			if (mbits & DML_DTR)
				dcaddr->dc_tcr |= TCR_DTR3;
			else
				dcaddr->dc_tcr &= ~TCR_DTR3;
		}
	}
	if ((mbits & DML_DTR) && (dcsoftCAR[unit >> 2] & b))
		dc_tty[unit].t_state |= TS_CARR_ON;
	(void) splx(s);
	return (mbits);
}

/*
 * This is called by timeout() periodically.
 * Check to see if modem status bits have changed.
 */
void
dcscan(arg)
	void *arg;
{
	register dcregs *dcaddr;
	register struct tty *tp;
	register int i, bit, car;
	int s;

	s = spltty();
	/* only channel 2 has modem control (what about line 3?) */
	dcaddr = (dcregs *)dcpdma[i = 2].p_addr;
	tp = &dc_tty[i];
	bit = TCR_DTR2;
	if (dcsoftCAR[i >> 2] & bit)
		car = 1;
	else
		car = dcaddr->dc_msr & MSR_DSR2;
	if (car) {
		/* carrier present */
		if (!(tp->t_state & TS_CARR_ON))
			(void)(*linesw[tp->t_line].l_modem)(tp, 1);
	} else if ((tp->t_state & TS_CARR_ON) &&
	    (*linesw[tp->t_line].l_modem)(tp, 0) == 0)
		dcaddr->dc_tcr &= ~bit;
	splx(s);
	timeout(dcscan, (void *)0, hz);
}

/*
 * ----------------------------------------------------------------------------
 *
 * dcGetc --
 *
 *	Read a character from a serial line.
 *
 * Results:
 *	A character read from the serial port.
 *
 * Side effects:
 *	None.
 *
 * ----------------------------------------------------------------------------
 */
int
dcGetc(dev)
	dev_t dev;
{
	register dcregs *dcaddr;
	register int c;
	int s;

	dcaddr = (dcregs *)dcpdma[minor(dev)].p_addr;
	if (!dcaddr)
		return (0);
	s = spltty();
	for (;;) {
		if (!(dcaddr->dc_csr & CSR_RDONE))
			continue;
		c = dcaddr->dc_rbuf;
		DELAY(10);
		if (((c >> 8) & 03) == (minor(dev) & 03))
			break;
	}
	splx(s);
	return (c & 0xff);
}

/*
 * Send a char on a port, non interrupt driven.
 */
void
dcPutc(dev, c)
	dev_t dev;
	int c;
{
	register dcregs *dcaddr;
	register u_short tcr;
	register int timeout;
	int s, line;

	s = spltty();

	dcaddr = (dcregs *)dcpdma[minor(dev)].p_addr;
	tcr = dcaddr->dc_tcr;
	dcaddr->dc_tcr = tcr | (1 << minor(dev));
	MachEmptyWriteBuffer();
	DELAY(10);
	while (1) {
		/*
		 * Wait for transmitter to be not busy.
		 */
		timeout = 1000000;
		while (!(dcaddr->dc_csr & CSR_TRDY) && timeout > 0)
			timeout--;
		if (timeout == 0) {
			printf("dcPutc: timeout waiting for CSR_TRDY\n");
			break;
		}
		line = (dcaddr->dc_csr >> 8) & 3;
		/*
		 * Check to be sure its the right port.
		 */
		if (line != minor(dev)) {
			tcr |= 1 << line;
			dcaddr->dc_tcr &= ~(1 << line);
			MachEmptyWriteBuffer();
			DELAY(10);
			continue;
		}
		/*
		 * Start sending the character.
		 */
		dcaddr->dc_tdr = dc_brk[0] | (c & 0xff);
		MachEmptyWriteBuffer();
		DELAY(10);
		/*
		 * Wait for character to be sent.
		 */
		while (1) {
			/*
			 * cc -O bug: this code produces and infinite loop!
			 * while (!(dcaddr->dc_csr & CSR_TRDY))
			 *	;
			 */
			timeout = 1000000;
			while (!(dcaddr->dc_csr & CSR_TRDY) && timeout > 0)
				timeout--;
			line = (dcaddr->dc_csr >> 8) & 3;
			if (line != minor(dev)) {
				tcr |= 1 << line;
				dcaddr->dc_tcr &= ~(1 << line);
				MachEmptyWriteBuffer();
				DELAY(10);
				continue;
			}
			dcaddr->dc_tcr &= ~(1 << minor(dev));
			MachEmptyWriteBuffer();
			DELAY(10);
			break;
		}
		break;
	}
	/*
	 * Enable interrupts for other lines which became ready.
	 */
	if (tcr & 0xF) {
		dcaddr->dc_tcr = tcr;
		MachEmptyWriteBuffer();
		DELAY(10);
	}

	splx(s);
}
#endif /* NDC */
