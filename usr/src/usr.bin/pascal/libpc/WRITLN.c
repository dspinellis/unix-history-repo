/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)WRITLN.c	1.3 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

WRITLN(curfile)

	register struct iorec	*curfile;
{
	if (curfile->funit & FREAD) {
		ERROR("%s: Attempt to write, but open for reading\n",
			curfile->pfname);
		return;
	}
	if (++curfile->lcount >= curfile->llimit) {
		ERROR("%s: Line limit exceeded\n", curfile->pfname);
		return;
	}
	fputc('\n', curfile->fbuf);
	if (ferror(curfile->fbuf)) {
		PERROR("Could not write to ", curfile->pfname);
		return;
	}
}
