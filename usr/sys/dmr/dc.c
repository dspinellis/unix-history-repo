#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

/*
 *   DC-11 driver
 */
#include "../param.h"
#include "../conf.h"
#include "../user.h"
#include "../tty.h"
#include "../proc.h"

/* base address */
#define	DCADDR	0174000
#define	NDC11	14

/* Control bits */
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

struct dcregs {
	int dcrcsr;
	int dcrbuf;
	int dctcsr;
	int dctbuf;
};

int dcrstab[] {
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

int dctstab[] {
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

dcopen(dev, flag)
{
	register struct tty *rtp;
	register *addr;

	if (dev.d_minor >= NDC11) {
		u.u_error = ENXIO;
		return;
	}
	rtp = &dc11[dev.d_minor];
	rtp->t_addr = addr = DCADDR + dev.d_minor*8;
	rtp->t_state =| WOPEN;
	addr->dcrcsr =| IENABLE|CDLEAD;
	if ((rtp->t_state&ISOPEN) == 0) {
		rtp->t_erase = CERASE;
		rtp->t_kill = CKILL;
		addr->dcrcsr = IENABLE|CDLEAD|SPEED1;
		addr->dctcsr = IENABLE|SPEED1|STOP1|RQSEND;
		rtp->t_state = ISOPEN | WOPEN;
		rtp->t_flags = ODDP|EVENP|ECHO;
	}
	if (addr->dcrcsr & CARRIER)
		rtp->t_state =| CARR_ON;
	while ((rtp->t_state & CARR_ON) == 0)
		sleep(&rtp->t_rawq, TTIPRI);
	rtp->t_state =& ~WOPEN;
	if (u.u_procp->p_ttyp == 0)
		u.u_procp->p_ttyp = rtp;
}

dcclose(dev)
{
	register struct tty *tp;

	(tp = &dc11[dev.d_minor])->t_state = 0;
	if (tp->t_flags&HUPCL)
		tp->t_addr->dcrcsr =& ~CDLEAD;
	wflushtty(tp);
}

dcread(dev)
{
	register struct tty *tp;

	tp = &dc11[dev.d_minor];
	if ((tp->t_addr->dcrcsr&CARRIER) != 0)
		ttread(tp);
}

dcwrite(dev)
{
	register struct tty *tp;

	tp = &dc11[dev.d_minor];
	if ((tp->t_addr->dcrcsr & CARRIER) != 0)
		ttwrite(tp);
}

dcxint(dev)
{
	register struct tty *tp;

	ttstart(tp = &dc11[dev.d_minor]);
	if (tp->t_outq.c_cc == 0 || tp->t_outq.c_cc == TTLOWAT)
		wakeup(&tp->t_outq);
}

dcrint(dev)
{
	register struct tty *tp;
	register int c, csr;

	tp = &dc11[dev.d_minor];
	c = tp->t_addr->dcrbuf;
	if (((csr = tp->t_addr->dcrcsr) & CARRIER) == 0) {
		if ((tp->t_state&WOPEN) == 0) {
			tp->t_addr->dcrcsr =& ~CDLEAD;
			if (tp->t_state & CARR_ON)
				signal(tp, SIGHUP);
			flushtty(tp);
		}
		tp->t_state =& ~CARR_ON;
		return;
	}
	if (csr&ERROR || (tp->t_state&ISOPEN)==0) {
		if (tp->t_state&WOPEN && csr&CARRIER)
			tp->t_state =| CARR_ON;
		wakeup(tp);
		return;
	}
	csr =& PARITY;
	if (csr&&(tp->t_flags&ODDP) || !csr&&(tp->t_flags&EVENP))
		ttyinput(c, tp);
}

dcsgtty(dev, av)
int *av;
{
	register struct tty *tp;
	register r;

	tp = &dc11[dev.d_minor];
	if (ttystty(tp, av))
		return;
	if (r = dcrstab[tp->t_speeds.lobyte&017])
		tp->t_addr->dcrcsr = r;
	if (r = dctstab[tp->t_speeds.hibyte&017])
		tp->t_addr->dctcsr = r;
}
