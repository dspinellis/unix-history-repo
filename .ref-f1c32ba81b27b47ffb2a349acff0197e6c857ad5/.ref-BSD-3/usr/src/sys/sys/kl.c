/*	kl.c	2.1	1/5/80	*/

#ifdef ERNIE
/*
 *   KL/DL-11 driver
 */
#include "../h/param.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/systm.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/uba.h"

/* base address */
#define	KLBASE	((struct device *)(UBA0_DEV+0176500))	/* kl and dl11-a */
#define	DLBASE	((struct device *)(UBA0_DEV+0175610))	/* dl-e */
#define	NKL11	8
#define	NDL11	0
#define	DSRDY	02
#define	RDRENB	01
#define	DLDELAY	4	/* Extra delay for DL's (double buff) */

#define	NL1	000400
#define	NL2	001000
#define	CR2	020000
#define	FF1	040000
#define	TAB1	002000

struct	tty kl_tty[NKL11+NDL11];
int	klstart();
int	ttrstrt();
char	partab[];

struct device {
	short	rcsr;
	short	rbuf;
	short	tcsr;
	short	tbuf;
};

/*ARGSUSED*/
klopen(dev, flag)
dev_t dev;
{
	register struct device *addr;
	register struct tty *tp;
	register d;

	d = minor(dev);
	if(d >= NKL11+NDL11) {
		u.u_error = ENXIO;
		return;
	}
	tp = &kl_tty[d];
	/*
	 * set up minor 0 thru NKL11-1 to address from KLBASE
	 * set up minor NKL11 on to address from DLBASE
	 */
	if(d<NKL11)
		addr = KLBASE + d;
	else
		addr = DLBASE + (d-NKL11);
	tp->t_addr = (caddr_t)addr;
	tp->t_oproc = klstart;
	if ((tp->t_state&ISOPEN) == 0) {
		tp->t_state = ISOPEN|CARR_ON;
		tp->t_flags = EVENP|LCASE|ECHO|XTABS|CRMOD|CR2;
		ttychars(tp);
	}
	addr->rcsr |= IENABLE|DSRDY|RDRENB;
	addr->tcsr |= IENABLE;
	(*linesw[tp->t_line].l_open)(dev, tp);
}

klclose(dev)
dev_t dev;
{
	register struct tty *tp;

	tp = &kl_tty[minor(dev)];
	(*linesw[tp->t_line].l_close)(tp);
	ttyclose(tp);
}

klread(dev)
dev_t dev;
{
	register struct tty *tp;

	tp = &kl_tty[minor(dev)];
	(*linesw[tp->t_line].l_read)(tp);
}

klwrite(dev)
dev_t dev;
{
	register struct tty *tp;

	tp = &kl_tty[minor(dev)];
	(*linesw[tp->t_line].l_write)(tp);
}

klxint(dev)
dev_t dev;
{
	register struct tty *tp;

	tp = &kl_tty[minor(dev)];
	if (tp->t_line)
		(*linesw[tp->t_line].l_start)(tp);
	else
		ttstart(tp);
	if ((tp->t_state&ASLEEP) && tp->t_outq.c_cc<=TTLOWAT)
		if (tp->t_chan)
			mcstart(tp->t_chan, (caddr_t)&tp->t_outq);
		else
			wakeup((caddr_t)&tp->t_outq);
}

klrint(dev)
dev_t dev;
{
	register int c;
	register struct device *addr;
	register struct tty *tp;

	tp = &kl_tty[minor(dev)];
	addr = (struct device *)tp->t_addr;
	c = addr->rbuf;
	addr->rcsr |= RDRENB;
	(*linesw[tp->t_line].l_rint)(c, tp);
}

/*ARGSUSED*/
klioctl(dev, cmd, addr, flag)
caddr_t addr;
dev_t dev;
{
	if (ttioccom(cmd, &kl_tty[minor(dev)], addr, dev)==0)
		u.u_error = ENOTTY;
}

klstart(tp)
register struct tty *tp;
{
	register c;
	register struct device *addr;

	addr = (struct device *)tp->t_addr;
	if ((addr->tcsr&DONE) == 0)
		return;
	if ((c=getc(&tp->t_outq)) >= 0) {
		if (tp->t_flags&RAW)
			addr->tbuf = c;
		else if (c<=0177)
			addr->tbuf = c | (partab[c]&0200);
		else {
			timeout(ttrstrt, (caddr_t)tp, (c&0177) + DLDELAY);
			tp->t_state |= TIMEOUT;
		}
	}
}
#endif
