/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FCALL.c 1.4 11/12/82";

#include "h00vars.h"

FCALL(save, frtn)
	char *save;
	register struct formalrtn *frtn;
{
	blkcpy(&_disply[1], save, frtn->fbn * sizeof(struct display));
	blkcpy(&frtn->fdisp[0], &_disply[1],
		frtn->fbn * sizeof(struct display));
}
