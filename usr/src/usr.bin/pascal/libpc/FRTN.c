/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FRTN.c 1.2 %G%";

#include "h00vars.h"

FRTN(frtn)
	register struct formalrtn *frtn;
{
	register struct display *dp;
	register struct display *ds;
	struct display *limit;

	limit = &frtn->disp[2 * frtn->cbn];
	for (ds = &frtn->disp[frtn->cbn], dp = &_disply[1]; ds < limit; )
		*dp++ = *ds++;
}
