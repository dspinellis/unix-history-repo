/*
 *	DM-BB driver
 */
#include "../h/param.h"
#include "../h/tty.h"
#include "../h/conf.h"

#define	DMADDR	((struct device *)0170500)

struct	tty dh11[];
int	ndh11;		/* Set by dh.c to number of lines */

#define	DONE	0200
#define	SCENABL	040
#define	CLSCAN	01000
#define	TURNON	03	/* CD lead, line enable */
#define	SECX	010	/* secondary xmit */
#define	RQS	04	/* request to send */
#define	TURNOFF	1	/* line enable only */
#define	CARRIER	0100
#define	CLS	040	/* clear to send */
#define	SECR	020	/* secondary receive */

struct device
{
	int	dmcsr;
	int	dmlstat;
	int	junk[2];
};

#define	B1200	9
#define	B300	7

/*
 * Turn on the line associated with the (DH) device dev.
 */
dmopen(dev)
{
	register struct tty *tp;
	register struct device *addr;
	register d;

	d = minor(dev);
	tp = &dh11[d];
	addr = DMADDR;
	addr += d>>4;
	spl5();
	addr->dmcsr = d&017;
	addr->dmlstat = TURNON;
	if (addr->dmlstat&CARRIER) {
		tp->t_state |= CARR_ON;
	}
	addr->dmcsr = IENABLE|SCENABL;
	while ((tp->t_state&CARR_ON)==0)
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	addr->dmcsr = d&017;
	if (addr->dmlstat&SECR) {
		tp->t_ispeed = B1200;
		tp->t_ospeed = B1200;
		dhparam(dev);
	}
	addr->dmcsr = IENABLE|SCENABL;
	spl0();
}

/*
 * Dump control bits into the DM registers.
 */
dmctl(dev, bits)
{
	register struct device *addr;
	register d, s;

	d = minor(dev);
	addr = DMADDR;
	addr += d>>4;
	s = spl5();
	addr->dmcsr = d&017;
	addr->dmlstat = bits;
	addr->dmcsr = IENABLE|SCENABL;
	splx(s);
}

/*
 * DM11 interrupt.
 * Mainly, deal with carrier transitions.
 */
dmint(dev)
{
	register struct tty *tp;
	register struct device *addr;
	register d;

	d = minor(dev);
	addr = DMADDR;
	addr += d;
	if (addr->dmcsr&DONE) {
		tp = &dh11[(d<<4)+(addr->dmcsr&017)];
		if (tp < &dh11[ndh11]) {
			wakeup((caddr_t)&tp->t_rawq);
			if ((addr->dmlstat&CARRIER)==0) {
				if ((tp->t_state&WOPEN)==0) {
					signal(tp->t_pgrp, SIGHUP);
					addr->dmlstat = 0;
					flushtty(tp);
				}
				tp->t_state &= ~CARR_ON;
			} else {
				tp->t_state |= CARR_ON;
			}
		}
		addr->dmcsr = IENABLE|SCENABL;
	}
}
