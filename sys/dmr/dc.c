#
/*
 *   DC-11 driver
 */
#include "/sys/nsys/param.h"
#include "/sys/nsys/conf.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/tty.h"
#include "/sys/nsys/proc.h"

/* base address */
#define	DCADDR	0174000

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

struct {
	char	lbyte;
	char	hbyte;
};

int dcrstab[] {
	0,		/* 0 baud */
	0,		/* 75 baud */
	0,		/* 110 baud */
	01101,		/* 134.5 baud: 7b/ch, speed 0 */
	0111,		/* 150 baud: 8b/ch, speed 1 */
	0121,		/* 300 baud: 8b/ch, speed 2 */
	0,		/* 600 baud */
	0131,		/* 1200 baud */
	0,		/* 1800 baud */
	0,		/* 2400 baud */
	0,		/* 4800 baud */
	0,		/* 9600 baud */
	0,		/* X0 */
	0,		/* X1 */
	0,		/* X2 */
	0		/* X3 */
};

int dctstab[] {
	0,		/* 0 baud */
	0,		/* 75 baud */
	0,		/* 110 baud */
	0501,		/* 134.5 baud: stop 1 */
	0511,		/* 150 baud */
	0521,		/* 300 baud */
	0,		/* 600 baud */
	0531,		/* 1200 baud */
	0,		/* 1800 baud */
	0,		/* 2400 baud */
	0,		/* 4800 baud */
	0,		/* 9600 baud */
	0,		/* X0 */
	0,		/* X1 */
	0,		/* X2 */
	0		/* X3 */
};

dcopen(dev, flag)
{
	struct tty *tp;
	register struct tty *rtp;
	int *addr;

	if (dev.d_minor >= NDC11) {
		u.u_error = ENXIO;
		return;
	}
	rtp = tp = &dc11[dev.d_minor];
	rtp->t_addr = addr = DCADDR + dev.d_minor*8;
	rtp->t_state =| WOPEN;
	addr->dcrcsr =| IENABLE|CDLEAD;
	if ((rtp->t_state&ISOPEN) == 0) {
		rtp->t_quit = 034;		/* FS */
		rtp->t_intrup = 0177;		/* DEL */
		addr->dcrcsr = IENABLE|CDLEAD|SPEED1;
		addr->dctcsr = IENABLE|SPEED1|STOP1|RQSEND;
		rtp->t_state = ISOPEN | WOPEN;
		rtp->t_flags = ODDP|EVENP|ECHO;
	}
	while (((addr->dcrcsr) & CARRIER) == 0)
		sleep(&tp->t_rawq, TTIPRI);
	tp->t_state =& ~WOPEN;
	if (u.u_procp->p_ttyp == 0)
		u.u_procp->p_ttyp = tp;
}

dcclose(dev)
{
	register struct tty *tp;

	(tp = &dc11[dev.d_minor])->t_state = 0;
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
	struct tty *tp;

	ttstart(tp = &dc11[dev.d_minor]);
	if (tp->t_outq.c_cc <= TTLOWAT)
		wakeup(&tp->t_outq);
}

dcrint(dev)
{
	struct tty *tp;
	register struct tty *rtp;
	register int c, csr;

	tp = rtp = &dc11[dev.d_minor];
	c = rtp->t_addr->dcrbuf;
	if (((csr = rtp->t_addr->dcrcsr) & CARRIER) == 0) {
		if ((rtp->t_state&WOPEN) == 0) {
			rtp->t_addr->dcrcsr =& ~CDLEAD;
			signal(rtp, SIGHUP);
			flushtty(tp);
		}
		return;
	}
	if (csr&ERROR || (rtp->t_state&ISOPEN)==0) {
		wakeup(rtp);
		return;
	}
	csr =& PARITY;
	if (csr&&(rtp->t_flags&ODDP) || !csr&&(rtp->t_flags&EVENP))
		ttyinput(c, rtp);
}

dcsgtty(dev, v)
int *v;
{
	register struct tty *tp;
	struct tty *atp;
	register r;

	tp = &dc11[dev.d_minor];
	if (v) {
		v[0] = tp->t_speeds;
		v[1] = 0;
		v[2] = tp->t_flags;
		return;
	}
	atp = tp;
	wflushtty(tp);
	tp = atp;
	tp->t_speeds = u.u_arg[0];
	tp->t_flags = u.u_arg[2];
	if (r = dcrstab[tp->t_speeds.lbyte&017])
		tp->t_addr->dcrcsr = r;
	if (r = dctstab[tp->t_speeds.hbyte&017])
		tp->t_addr->dctcsr = r;
}
