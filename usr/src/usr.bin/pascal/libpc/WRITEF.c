/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)WRITEF.c	1.5 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

WRITEF(curfile, d1, d2, d3, d4, d5, d6, d7, d8)

	register struct iorec	*curfile;
	FILE			*d1;
	char			*d2;
	int			d3, d4, d5, d6, d7, d8;
{
	if (curfile->funit & FREAD) {
		ERROR("%s: Attempt to write, but open for reading\n",
			curfile->pfname);
		return;
	}
	fprintf(d1, d2, d3, d4, d5, d6, d7, d8);
	if (ferror(curfile->fbuf)) {
		PERROR("Could not write to ", curfile->pfname);
		return;
	}
}
