/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FCALL.c 1.1 10/29/80";

#include "h00vars.h"

FCALL(frtn)
	register struct formalrtn *frtn;
{
	register struct display *dp;
	register struct display *ds;
	struct display *limit;

	limit = &frtn->disp[2 * frtn->cbn];
	for (dp = &_disply[1], ds = &frtn->disp[frtn->cbn]; ds < limit; )
		*ds++ = *dp++;
	limit = &frtn->disp[frtn->cbn];
	for (ds = &frtn->disp[0], dp = &_disply[1]; ds < limit; )
		*dp++ = *ds++;
	return (long)(frtn->entryaddr);
}
