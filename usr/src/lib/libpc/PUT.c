/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)PUT.c 1.3 6/10/81";

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
