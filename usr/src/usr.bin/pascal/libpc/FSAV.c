/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FSAV.c 1.1 %G%";

#include "h00vars.h"

struct formalrtn *
FSAV(entryaddr, cbn, frtn) 
	long (*entryaddr)();
	long cbn;
	register struct formalrtn *frtn;
{
	register struct display *dp;
	register struct display *ds;
	struct display *limit;

	frtn->entryaddr = entryaddr;
	frtn->cbn = cbn;
	limit = &frtn->disp[frtn->cbn];
	for (dp = &_disply[1], ds = &frtn->disp[0]; ds < limit; )
		*ds++ = *dp++;
	return frtn;
}
