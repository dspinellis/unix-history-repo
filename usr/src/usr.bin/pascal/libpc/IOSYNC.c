/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)IOSYNC.c 1.3 %G%";

#include "h00vars.h"
#include "h01errs.h"

/*
 * insure that a usable image is in the buffer window
 */
IOSYNC(curfile)

	register struct iorec	*curfile;
{
	register short		unit = curfile->funit;
	char			*limit, *ptr;

	if (unit & FWRITE) {
		ERROR(EREADIT, curfile->pfname);
		return;
	}
	if ((unit & SYNC) == 0) {
		return;
	}
	if (unit & EOFF) {
		ERROR(EPASTEOF, curfile->pfname);
		return;
	}
	unit &= ~SYNC;
	fread(curfile->fileptr, curfile->fsize, 1, curfile->fbuf);
	if (ferror(curfile->fbuf)) {
		ERROR(EPASTEOF, curfile->pfname);
		return;
	}
	if (feof(curfile->fbuf)) {
		curfile->funit = unit | EOFF;
		if (unit & FTEXT) {
			*curfile->fileptr = ' ';
			return;
		}
		limit = &curfile->fileptr[curfile->fsize];
		for (ptr = curfile->fileptr; ptr < limit; )
			*ptr++ = 0;
		return;
	}
	if (unit & FTEXT) {
		if (*curfile->fileptr == '\n') {
			unit |= EOLN;
			*curfile->fileptr = ' ';
		} else {
			unit &= ~EOLN;
		}
	}
	curfile->funit = unit;
}
