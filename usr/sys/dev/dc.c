#include "../h/param.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/systm.h"

/*
 * Base address of DC-11's. Minor device  i  is at
 * DCADDR + 10*i.
 */
#define	DCADDR	(struct device *)0174000

/*
 * Number of DC's for which table space is allocated.
 */
#define	NDC11	4

/*
 * Control bits in device registers
 */
#define	CDLEAD	01
#define	CARRIER	04
#define	SPEED1	010
#define	STOP1	0400
#define	RQSEND	01
#define	PARITY	040
#define	ERROR	0100000
#define	CTRANS	040000
#define	RINGIND	020000


struct	tty dc11[NDC11];

struct device {
	int dcrcsr;
	int dcrbuf;
	int dctcsr;
	int dctbuf;
};

/*
 * Input-side speed and control bit table.
 * Each DC11 has 4 speeds which correspond to the 4 non-zero entries.
 * The table index is the same as the speed-selector
 * number for the DH11.
 * Attempts to set the speed to a zero entry are ignored.
 */
int dcrstab[] = {
	0,		/* 0 baud */
	0,		/* 50 baud */
	0,		/* 75 baud */
	0,		/* 110 baud */
	01101,		/* 134.5 baud: 7b/ch, speed 0 */
	0111,		/* 150 baud: 8b/ch, speed 1 */
	0,		/* 200 baud */
	0121,		/* 300 baud: 8b/ch, speed 2 */
	0,		/* 600 baud */
	0131,		/* 1200 baud */
	0,		/* 1800 baud */
	0,		/* 2400 baud */
	0,		/* 4800 baud */
	0,		/* 9600 baud */
	0,		/* X0 */
	0,		/* X1 */
};

/*
 * Transmitter speed table
 */
int dctstab[] = {
	0,		/* 0 baud */
	0,		/* 50 baud */
	0,		/* 75 baud */
	0,		/* 110 baud */
	0501,		/* 134.5 baud: stop 1 */
	0511,		/* 150 baud */
	0,		/* 200 baud */
	0521,		/* 300 baud */
	0,		/* 600 baud */
	0531,		/* 1200 baud */
	0,		/* 1800 baud */
	0,		/* 2400 baud */
	0,		/* 4800 baud */
	0,		/* 9600 baud */
	0,		/* X0 */
	0,		/* X1 */
};

/*
 * Open a DC11, waiting until carrier is established.
 * Default initial conditions are set up on the first open.
 * t_state's CARR_ON bit is a pure copy of the hardware
 * CARRIER bit, and is only used to regularize
 * carrier tests in general tty routines.
 */
dcopen(dev, flag)
dev_t dev;
{
	register struct tty *tp;
	register struct device *addr;
	extern int klstart();
	int s;

	if (minor(dev) >= NDC11) {
		u.u_error = ENXIO;
		return;
	}
	tp = &dc11[minor(dev)];
	addr = DCADDR + minor(dev);
	tp->t_addr = (caddr_t)addr;
	tp->t_state |= WOPEN;
	s = spl5();
	addr->dcrcsr |= IENABLE|CDLEAD;
	if ((tp->t_state&ISOPEN) == 0) {
		tp->t_erase = CERASE;
		tp->t_kill = CKILL;
		addr->dcrcsr = IENABLE|CDLEAD|SPEED1;
		addr->dctcsr = IENABLE|SPEED1|STOP1|RQSEND;
		tp->t_state = ISOPEN | WOPEN;
		tp->t_flags = ODDP|EVENP|ECHO;
		tp->t_oproc = klstart;
	}
	if (addr->dcrcsr & CARRIER)
		tp->t_state |= CARR_ON;
	splx(s);
	while ((tp->t_state & CARR_ON) == 0)
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	ttyopen(dev, tp);
}

/*
 * Close a dc11
 */
dcclose(dev)
dev_t dev;
{
	register struct tty *tp;

	tp = &dc11[minor(dev)];
	if (tp->t_state&HUPCLS)
		((struct device *)(tp->t_addr))->dcrcsr &= ~CDLEAD;
	ttyclose(tp);
}

/*
 * Read a DC11
 */
dcread(dev)
dev_t dev;
{
	ttread(&dc11[minor(dev)]);
}

/*
 * Write a DC11
 */
dcwrite(dev)
dev_t dev;
{
	ttwrite(&dc11[minor(dev)]);
}

/*
 * DC11 transmitter interrupt.
 */
dcxint(dev)
dev_t dev;
{
	register struct tty *tp;

	tp = &dc11[minor(dev)];
	ttstart(tp);
	if (tp->t_outq.c_cc == 0 || tp->t_outq.c_cc == TTLOWAT)
		wakeup((caddr_t)&tp->t_outq);
}

/*
 * DC11 receiver interrupt.
 */
dcrint(dev)
dev_t dev;
{
	register struct tty *tp;
	register int c, csr;

	tp = &dc11[minor(dev)];
	c = ((struct device *)(tp->t_addr))->dcrbuf;
	/*
	 * If carrier is off, and an open is not in progress,
	 * knock down the CD lead to hang up the local dataset
	 * and signal a hangup.
	 */
	if (((csr = ((struct device *)(tp->t_addr))->dcrcsr) & CARRIER) == 0) {
		if ((tp->t_state&WOPEN) == 0) {
			((struct device *)(tp->t_addr))->dcrcsr &= ~CDLEAD;
			if (tp->t_state & CARR_ON)
				signal(tp->t_pgrp, SIGHUP);
			flushtty(tp);
		}
		tp->t_state &= ~CARR_ON;
		return;
	}
	if (csr&ERROR || (tp->t_state&ISOPEN)==0) {
		if (tp->t_state&WOPEN && csr&CARRIER)
			tp->t_state |= CARR_ON;
		wakeup((caddr_t)tp);
		return;
	}
	csr &= PARITY;
	if (csr&&(tp->t_flags&ODDP) || !csr&&(tp->t_flags&EVENP))
		ttyinput(c, tp);
}

/*
 * DC11 stty/gtty.
 * Perform general functions and set speeds.
 */
dcioctl(dev, cmd, addr, flag)
dev_t dev;
caddr_t addr;
{
	register struct tty *tp;
	register r;

	tp = &dc11[minor(dev)];
	if (ttioccom(cmd, &dc11[minor(dev)], addr, dev) == 0) {
		u.u_error = ENOTTY;
		return;
	}
	if (cmd == TIOCSETP) {
		r = dcrstab[tp->t_ispeed];
		if (r)
			((struct device *)(tp->t_addr))->dcrcsr = r;
		else
			((struct device *)(tp->t_addr))->dcrcsr &= ~CDLEAD;
		r = dctstab[tp->t_ospeed];
		((struct device *)(tp->t_addr))->dctcsr = r;
	}
}
