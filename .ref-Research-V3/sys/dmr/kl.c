#
/*
 *   KL-11 driver
 */
#include "/sys/nsys/param.h"
#include "/sys/nsys/conf.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/tty.h"
#include "/sys/nsys/proc.h"

/* base address */
#define	KLADDR	0177560
#define	RDRENB	01

/* For now, only 1 KL allowed */
struct	tty kl11;

struct klregs {
	int klrcsr;
	int klrbuf;
	int kltcsr;
	int kltbuf;
}

klopen(dev, flag)
{

	kl11.t_quit = 0175;		/* DEC's "altmode" */
	kl11.t_intrup = 0177;		/* DEL */
	if (u.u_procp->p_ttyp == 0)
		u.u_procp->p_ttyp = &kl11;
	kl11.t_addr = KLADDR;
	kl11.t_flags = XTABS|LCASE|ECHO|CRMOD;
	KLADDR->klrcsr =| IENABLE|RDRENB;
	KLADDR->kltcsr =| IENABLE;
}

klclose(dev)
{
	wflushtty(&kl11);
}

klread(dev)
{
	ttread(&kl11);
}

klwrite(dev)
{
	ttwrite(&kl11);
}

klxint(dev)
{
	ttstart(&kl11);
	if (kl11.t_outq.c_cc <= TTLOWAT)
		wakeup(&kl11.t_outq);
}

klrint(dev)
{
	register int c;

	c = KLADDR->klrbuf;
	KLADDR->klrcsr =| RDRENB;
	ttyinput(c, &kl11);
}

klsgtty(dev, v)
int *v;
{
	if (v)
		v[2] = kl11.t_flags;
	else {
		wflushtty(&kl11);
		kl11.t_flags = u.u_arg[2];
	}
}
