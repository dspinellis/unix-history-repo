#
/*
 */

/*
 * GP DR11C driver used for C/A/T
 */

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"

#define	CATADDR	((struct catdev *)0167770)
#define	PCAT	(PZERO+9)
#define	CATHIWAT	100
#define	CATLOWAT	30

struct {
	int	catlock;
	struct	clist	oq;
} cat;

struct catdev {
	int	catcsr;
	int	catbuf;
};

ctopen(dev)
{
	if (cat.catlock==0) {
		cat.catlock++;
		CATADDR->catcsr |= IENABLE;
	} else
		u.u_error = ENXIO;
}

ctclose()
{
	cat.catlock = 0;
	catintr();
}

ctwrite(dev)
{
	register c;
	extern lbolt;

	while ((c=cpass()) >= 0) {
		spl5();
		while (cat.oq.c_cc > CATHIWAT)
			sleep((caddr_t)&cat.oq, PCAT);
		while (putc(c, &cat.oq) < 0)
			sleep((caddr_t)&lbolt, PCAT);
		catintr();
		spl0();
	}
}

catintr()
{
	register int c;

	if (CATADDR->catcsr&DONE) {
		if ((c = getc(&cat.oq)) >= 0) {
			CATADDR->catbuf = c;
			if (cat.oq.c_cc==0 || cat.oq.c_cc==CATLOWAT)
				wakeup((caddr_t)&cat.oq);
		} else {
			if (cat.catlock==0)
				CATADDR->catcsr = 0;
		}
	}
}
