/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)READC.c 1.3 %G%";

#include "h00vars.h"

char
READC(curfile)

	register struct iorec	*curfile;
{
	char			data;

	if (curfile->funit & FWRITE) {
		ERROR("%s: Attempt to read, but open for writing\n",
			curfile->pfname);
	}
	IOSYNC(curfile);
	if (curfile->funit & EOFF) {
		ERROR("%s: Tried to read past end of file\n", curfile->pfname);
	}
	curfile->funit |= SYNC;
	return *curfile->fileptr;
}
