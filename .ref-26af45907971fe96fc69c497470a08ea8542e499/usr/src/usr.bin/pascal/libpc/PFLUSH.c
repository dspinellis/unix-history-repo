/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)PFLUSH.c	1.3 (Berkeley) %G%";
#endif /* not lint */

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
