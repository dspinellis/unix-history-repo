/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)PFLUSH.c 1.2 1/21/83";

#include "h00vars.h"

/*
 * insure that a usable image is in the buffer window
 */
PFLUSH()
{
	register struct iorec	*next;

	for (next = _fchain.fchain; next != FILNIL; next = next->fchain) {
		if ((next->funit & (FDEF | FREAD)) != 0)
			continue;
		if (next->fbuf != 0)
			fflush(next->fbuf);
	}
}
