#
/*
 * GP DR11C driver used for C/A/T
 */

#include "/sys/nsys/param.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/tty.h"

#define	CATADDR	0167750
#define	PCAT	9

struct {
	int	catlock;
	struct	clist	oq;
} cat;

struct {
	int	catcsr;
	int	catbuf;
};

ctopen(dev)
{
	if (cat.catlock==0) {
		cat.catlock++;
		CATADDR->catcsr =| IENABLE;
	} else
		u.u_error = ENXIO;
}

ctclose(dev)
{
	cat.catlock = 0;
}

ctwrite(dev)
{
	int c;
	extern lbolt;

	spl5();
	while (cpass(&c) >= 0) {
		while (cat.oq.c_cc > 60)
			sleep(&cat.oq, PCAT);
		while (putc(c, &cat.oq) < 0)
			sleep(&lbolt, PCAT);
		catintr();
	}
	spl0();
}

catintr()
{
	register int c;

	if (CATADDR->catcsr&DONE && (c=getc(&cat.oq))>=0) {
		CATADDR->catbuf = c;
		if (cat.oq.c_cc==0 || cat.oq.c_cc==15)
			wakeup(&cat.oq);
	}
}
