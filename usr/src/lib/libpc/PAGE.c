/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)PAGE.c 1.2 6/10/81";

#include "h00vars.h"

PAGE(curfile)

	register struct iorec	*curfile;
{
	if (curfile->funit & FREAD) {
		ERROR("%s: Attempt to write, but open for reading\n",
			curfile->pfname);
		return;
	}
	fputc('', curfile->fbuf);
	if (ferror(curfile->fbuf)) {
		PERROR("Could not write to ", curfile->pfname);
		return;
	}
}
