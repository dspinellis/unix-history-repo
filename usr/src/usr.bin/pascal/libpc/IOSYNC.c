/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)IOSYNC.c	1.6 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

/*
 * insure that a usable image is in the buffer window
 */
IOSYNC(curfile)

	register struct iorec	*curfile;
{
	char			*limit, *ptr;

	if (curfile->funit & FWRITE) {
		ERROR("%s: Attempt to read, but open for writing\n",
			curfile->pfname);
		return;
	}
	if ((curfile->funit & SYNC) == 0) {
		return;
	}
	if (curfile->funit & EOFF) {
		ERROR("%s: Tried to read past end of file\n", curfile->pfname);
		return;
	}
	curfile->funit &= ~SYNC;
	if (curfile->funit & SPEOLN) {
		curfile->funit &= ~(SPEOLN|EOLN);
		curfile->funit |= EOFF;
		return;
	}
	fread(curfile->fileptr, (int)curfile->fsize, 1, curfile->fbuf);
	if (ferror(curfile->fbuf)) {
		ERROR("%s: Tried to read past end of file\n", curfile->pfname);
		return;
	}
	if (feof(curfile->fbuf)) {
		if (curfile->funit & FTEXT) {
			*curfile->fileptr = ' ';
			if (curfile->funit & EOLN) {
				curfile->funit &= ~EOLN;
				curfile->funit |= EOFF;
				return;
			}
			curfile->funit |= (SPEOLN|EOLN);
			return;
		}
		curfile->funit |= EOFF;
		limit = &curfile->fileptr[curfile->fsize];
		for (ptr = curfile->fileptr; ptr < limit; )
			*ptr++ = 0;
		return;
	}
	if (curfile->funit & FTEXT) {
		if (*curfile->fileptr == '\n') {
			curfile->funit |= EOLN;
			*curfile->fileptr = ' ';
			return;
		}
		curfile->funit &= ~EOLN;
	}
}
