/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)PUT.c	1.4 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

PUT(curfile)

	register struct iorec	*curfile;
{
	if (curfile->funit & FREAD) {
		ERROR("%s: Attempt to write, but open for reading\n",
			curfile->pfname);
		return;
	}
	fwrite(curfile->fileptr, (int)curfile->fsize, 1, curfile->fbuf);
	if (ferror(curfile->fbuf)) {
		PERROR("Could not write to ", curfile->pfname);
		return;
	}
}
