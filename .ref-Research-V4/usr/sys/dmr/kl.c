#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

/*
 *   KL-11 driver
 */
#include "../param.h"
#include "../conf.h"
#include "../user.h"
#include "../tty.h"
#include "../proc.h"

/* base address */
#define	KLADDR	0177560
#define	KLBASE	0176500
#define	NKL11	4
#define DSRDY	02
#define	RDRENB	01

struct	tty kl11[NKL11];

struct klregs {
	int klrcsr;
	int klrbuf;
	int kltcsr;
	int kltbuf;
}

klopen(dev, flag)
{
	register *addr;
	register struct tty *tp;

	if(dev.d_minor >= NKL11) {
		u.u_error = ENXIO;
		return;
	}
	tp = &kl11[dev.d_minor];
	tp->t_quit = 034;		/* ctl shf l */
	tp->t_intrup = 0177;		/* DEL */
	if (u.u_procp->p_ttyp == 0)
		u.u_procp->p_ttyp = tp;
	addr = KLADDR;
	if(dev.d_minor)
		addr = KLBASE-8 + 8*dev.d_minor;
	tp->t_addr = addr;
	tp->t_flags = XTABS|LCASE|ECHO|CRMOD;
	tp->t_state = CARR_ON;
	addr->klrcsr =| IENABLE|DSRDY|RDRENB;
	addr->kltcsr =| IENABLE;
}

klclose(dev)
{
	wflushtty(&kl11[dev.d_minor]);
}

klread(dev)
{
	ttread(&kl11[dev.d_minor]);
}

klwrite(dev)
{
	ttwrite(&kl11[dev.d_minor]);
}

klxint(dev)
{
	register struct tty *tp;

	tp = &kl11[dev.d_minor];
	ttstart(tp);
	if (tp->t_outq.c_cc == 0 || tp->t_outq.c_cc == TTLOWAT)
		wakeup(&tp->t_outq);
}

klrint(dev)
{
	register int c, *addr;
	register struct tty *tp;

	tp = &kl11[dev.d_minor];
	addr = tp->t_addr;
	c = addr->klrbuf;
	addr->klrcsr =| RDRENB;
	if ((c&0177)==0)
		addr->kltbuf = c;	/* hardware botch */
	ttyinput(c, tp);
}

klsgtty(dev, v)
int *v;
{
	register struct tty *tp;

	tp = &kl11[dev.d_minor];
	if (v)
		v[2] = tp->t_flags;
	else {
		wflushtty(tp);
		tp->t_flags = u.u_arg[2];
	}
}
