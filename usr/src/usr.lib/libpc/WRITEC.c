/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)WRITEC.c 1.4 6/10/81";

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
