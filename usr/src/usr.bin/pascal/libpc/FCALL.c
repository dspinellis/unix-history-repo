/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FCALL.c 1.2 %G%";

#include "h00vars.h"

long *
FCALL(frtn)
	register struct formalrtn *frtn;
{
	blkcpy(frtn->cbn * sizeof(struct display),
	       &_disply[1], &frtn->disp[frtn->cbn]);
	blkcpy(frtn->cbn * sizeof(struct display),
	       &frtn->disp[0], &_disply[1]);
	return (long *)(frtn->entryaddr);
}
