/*
 *   KL/DL-11 driver
 */
#include "../h/param.h"
#include "../h/conf.h"
#include	"../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/systm.h"
#include "../h/uba.h"

/* base address */
#define	KLBASE	((struct device *)(UBA0_DEV+0176500))	/* kl and dl11-a */
#define	DLBASE	((struct device *)(UBA0_DEV+0175610))	/* dl-e */
#define	NKL11	1
#define	NDL11	0
#define DSRDY	02
#define	RDRENB	01
#define	DLDELAY	4	/* Extra delay for DL's (double buff) */

#define	NL1	000400
#define	NL2	001000
#define	CR2	020000
#define	FF1	040000
#define	TAB1	002000

struct	tty kl11[NKL11+NDL11];
int	klstart();
int	ttrstrt();
char	partab[];

struct device {
	short	rcsr;
	short	rbuf;
	short	tcsr;
	short	tbuf;
};

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
	tp = &kl11[d];
	/*
	 * set up minor 0 to address KLADDR
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
		tp->t_erase = CERASE;
		tp->t_kill = CKILL;
	}
	addr->rcsr |= IENABLE|DSRDY|RDRENB;
	addr->tcsr |= IENABLE;
	ttyopen(dev, tp);
}

klclose(dev)
dev_t dev;
{
	register struct tty *tp;

	tp = &kl11[minor(dev)];
	ttyclose(tp);
}

klread(dev)
dev_t dev;
{
	ttread(&kl11[minor(dev)]);
}

klwrite(dev)
dev_t dev;
{
	ttwrite(&kl11[minor(dev)]);
}

klxint(dev)
dev_t dev;
{
	register struct tty *tp;

	tp = &kl11[minor(dev)];
	ttstart(tp);
	if (tp->t_outq.c_cc == 0 || tp->t_outq.c_cc == TTLOWAT)
		wakeup((caddr_t)&tp->t_outq);
}

klrint(dev)
dev_t dev;
{
	register int c;
	register struct device *addr;
	register struct tty *tp;

	tp = &kl11[minor(dev)];
	addr = (struct device *)tp->t_addr;
	c = addr->rbuf;
	addr->rcsr |= RDRENB;
	ttyinput(c, tp);
}

klioctl(dev, cmd, addr, flag)
caddr_t addr;
dev_t dev;
{
	if (ttioccom(cmd, &kl11[minor(dev)], addr, dev)==0)
		u.u_error = ENOTTY;
}

klstart(tp)
register struct tty *tp;
{
	register c;
	register struct device *addr;

	addr = (struct device *)tp->t_addr;
	if((addr->tcsr&DONE) == 0)
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
