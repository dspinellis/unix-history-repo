/*	ct.c	4.3	81/03/09	*/

#include "cat.h"
#if NCT > 0
/*
 * GP DR11C driver used for C/A/T
 */

#include "../h/param.h"
#include "../h/tty.h"
#include "../h/pte.h"
#include "../h/map.h"
#include "../h/uba.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"

#define	PCAT	(PZERO+9)
#define	CATHIWAT	100
#define	CATLOWAT	30

struct {
	int	catlock;
	struct	clist	oq;
} cat;

struct device {
	short	catcsr;
	short	catbuf;
};

int ctintr();

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
	ctintr();
}

ctwrite(dev)
{
	register c;
	extern lbolt;

	while ((c=cpass()) >= 0) {
		(void) spl5();
		while (cat.oq.c_cc > CATHIWAT)
			sleep((caddr_t)&cat.oq, PCAT);
		while (putc(c, &cat.oq) < 0)
			sleep((caddr_t)&lbolt, PCAT);
		ctintr();
		(void) spl0();
	}
}

ctintr()
{
	register int c;

	if (CATADDR->catcsr&DONE) {
		if ((c = getc(&cat.oq)) >= 0) {
#if MH135A
			c |= (c & 01) << 8;	/* for dr11c bug */
#endif		
			CATADDR->catbuf = c;
			if (cat.oq.c_cc==0 || cat.oq.c_cc==CATLOWAT)
				wakeup(&cat.oq);
		} else {
			if (cat.catlock==0)
				CATADDR->catcsr = 0;
		}
	}

}
#endif
