/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)WRITES.c	1.5 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

WRITES(curfile, d1, d2, d3, d4)

	register struct iorec	*curfile;
	FILE			*d1;
	int			d2, d3;
	char			*d4;
{
	if (curfile->funit & FREAD) {
		ERROR("%s: Attempt to write, but open for reading\n",
			curfile->pfname);
		return;
	}
	fwrite(d1, d2, d3, d4);
	if (ferror(curfile->fbuf)) {
		PERROR("Could not write to ", curfile->pfname);
		return;
	}
}
