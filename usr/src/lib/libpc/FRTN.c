/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FRTN.c 1.5 6/8/81";

#include "h00vars.h"

FRTN(frtn)
	register struct formalrtn *frtn;
{
	blkcpy(frtn->fbn * sizeof(struct display),
		frtn->fdisp[frtn->fbn], &_disply[1]);
}
