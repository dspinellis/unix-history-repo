/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)WRITEC.c	1.5 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

WRITEC(curfile, d1, d2)

	register struct iorec	*curfile;
	char			d1;
	FILE			*d2;
{
	if (curfile->funit & FREAD) {
		ERROR("%s: Attempt to write, but open for reading\n",
			curfile->pfname);
		return;
	}
	fputc(d1, d2);
	if (ferror(curfile->fbuf)) {
		PERROR("Could not write to ", curfile->pfname);
		return;
	}
}
