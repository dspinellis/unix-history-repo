/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FSAV.c 1.2 %G%";

#include "h00vars.h"

struct formalrtn *
FSAV(entryaddr, cbn, frtn) 
	long (*entryaddr)();
	long cbn;
	register struct formalrtn *frtn;
{
	frtn->entryaddr = entryaddr;
	frtn->cbn = cbn;
	blkcpy(frtn->cbn * sizeof(struct display),
	       &_disply[1], &frtn->disp[0]);
	return frtn;
}
