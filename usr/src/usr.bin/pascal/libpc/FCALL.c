/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FCALL.c 1.3 %G%";

#include "h00vars.h"

FCALL(save, frtn)
	char *save;
	register struct formalrtn *frtn;
{
	blkcpy(frtn->fbn * sizeof(struct display), &_disply[1], save);
	blkcpy(frtn->fbn * sizeof(struct display), &frtn->fdisp[0],
		&_disply[1]);
}
