#
/*
 */

/*
 * GP DR11C driver used for C/A/T
 */

#include "../param.h"
#include "../user.h"
#include "../tty.h"

#define	CATADDR	0167750
#define	PCAT	9
#define	CATHIWAT	60
#define	CATLOWAT	15

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

ctclose()
{
	cat.catlock = 0;
}

ctwrite(dev)
{
	register c;
	extern lbolt;

	while ((c=cpass()) >= 0) {
		spl5();
		while (cat.oq.c_cc > CATHIWAT)
			sleep(&cat.oq, PCAT);
		while (putc(c, &cat.oq) < 0)
			sleep(&lbolt, PCAT);
		catintr();
		spl0();
	}
}

catintr()
{
	register int c;

	if (CATADDR->catcsr&DONE && (c=getc(&cat.oq))>=0) {
		CATADDR->catbuf = c;
		if (cat.oq.c_cc==0 || cat.oq.c_cc==CATLOWAT)
			wakeup(&cat.oq);
	} else {
		if (cat.catlock==0)
			CATADDR->catcsr = 0;
	}
}
