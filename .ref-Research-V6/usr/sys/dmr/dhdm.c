#
/*
 */

/*
 *	DM-BB driver
 */
#include "../param.h"
#include "../tty.h"
#include "../conf.h"

#define	DMADDR	0170500

struct	tty dh11[];
int	ndh11;		/* Set by dh.c to number of lines */

#define	DONE	0200
#define	SCENABL	040
#define	CLSCAN	01000
#define	TURNON	07	/* RQ send, CD lead, line enable */
#define	TURNOFF	1	/* line enable only */
#define	CARRIER	0100

struct dmregs {
	int	dmcsr;
	int	dmlstat;
};

/*
 * Turn on the line associated with the (DH) device dev.
 */
dmopen(dev)
{
	register struct tty *tp;

	tp = &dh11[dev.d_minor];
	DMADDR->dmcsr = dev.d_minor;
	DMADDR->dmlstat = TURNON;
	if (DMADDR->dmlstat&CARRIER)
		tp->t_state =| CARR_ON;
	DMADDR->dmcsr = IENABLE|SCENABL;
	spl5();
	while ((tp->t_state&CARR_ON)==0)
		sleep(&tp->t_rawq, TTIPRI);
	spl0();
}

/*
 * If a DH line has the HUPCL mode,
 * turn off carrier when it is closed.
 */
dmclose(dev)
{
	register struct tty *tp;

	tp = &dh11[dev.d_minor];
	if (tp->t_flags&HUPCL) {
		DMADDR->dmcsr = dev.d_minor;
		DMADDR->dmlstat = TURNOFF;
		DMADDR->dmcsr = IENABLE|SCENABL;
	}
}

/*
 * DM11 interrupt.
 * Mainly, deal with carrier transitions.
 */
dmint()
{
	register struct tty *tp;

	if (DMADDR->dmcsr&DONE) {
		tp = &dh11[DMADDR->dmcsr&017];
		if (tp < &dh11[ndh11]) {
			wakeup(tp);
			if ((DMADDR->dmlstat&CARRIER)==0) {
				if ((tp->t_state&WOPEN)==0) {
					signal(tp, SIGHUP);
					DMADDR->dmlstat = 0;
					flushtty(tp);
				}
				tp->t_state =& ~CARR_ON;
			} else
				tp->t_state =| CARR_ON;
		}
		DMADDR->dmcsr = IENABLE|SCENABL;
	}
}
