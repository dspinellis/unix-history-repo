/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FSAV.c 1.3 4/1/81";

#include "h00vars.h"

struct formalrtn *
FSAV(entryaddr, cbn, frtn) 
	long (*entryaddr)();
	long cbn;
	register struct formalrtn *frtn;
{
	frtn->fentryaddr = entryaddr;
	frtn->fbn = cbn;
	blkcpy(frtn->fbn * sizeof(struct display),
	       &_disply[1], &frtn->fdisp[0]);
	return frtn;
}
