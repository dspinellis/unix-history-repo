/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FCALL.c 1.4 6/8/81";

#include "h00vars.h"

FCALL(frtn)
	register struct formalrtn *frtn;
{
	blkcpy(frtn->fbn * sizeof(struct display),
		&_disply[1], &frtn->fdisp[frtn->fbn]);
	blkcpy(frtn->fbn * sizeof(struct display),
		&frtn->fdisp[0], &_disply[1]);
}
