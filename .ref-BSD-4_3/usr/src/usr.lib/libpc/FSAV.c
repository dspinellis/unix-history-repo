/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FSAV.c 1.4 11/12/82";

#include "h00vars.h"

struct formalrtn *
FSAV(entryaddr, cbn, frtn) 
	long (*entryaddr)();
	long cbn;
	register struct formalrtn *frtn;
{
	frtn->fentryaddr = entryaddr;
	frtn->fbn = cbn;
	blkcpy(&_disply[1], &frtn->fdisp[0],
		frtn->fbn * sizeof(struct display));
	return frtn;
}
