/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)LLIMIT.c 1.1 %G%";

#include "h00vars.h"
#include "h01errs.h"

LLIMIT(curfile, limit)

	register struct iorec	*curfile;
	int			limit;
{
	if (limit <= 0)
		limit = 0x7fffffff;
	curfile->llimit = limit;
	if (curfile->lcount >= curfile->llimit) {
		ERROR(ELLIMIT, curfile->pfname);
		return;
	}
}
