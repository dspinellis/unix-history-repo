/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FRTN.c 1.3 %G%";

#include "h00vars.h"

FRTN(frtn)
	register struct formalrtn *frtn;
{
	blkcpy(frtn->cbn * sizeof(struct display),
	       &frtn->disp[frtn->cbn], &_disply[1]);
}
