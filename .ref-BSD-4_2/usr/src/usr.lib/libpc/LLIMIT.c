/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)LLIMIT.c 1.3 6/10/81";

#include "h00vars.h"

LLIMIT(curfile, limit)

	register struct iorec	*curfile;
	long			limit;
{
	if (limit <= 0)
		limit = 0x7fffffff;
	curfile->llimit = limit;
	if (curfile->lcount >= curfile->llimit) {
		ERROR("%s: Line limit exceeded\n", curfile->pfname);
		return;
	}
}
