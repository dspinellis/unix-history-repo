/* Copyright (c) 1989 Regents of the University of California */

static char sccsid[] = "@(#)VWRITEF.c 1.2 %G%";

#include "h00vars.h"

#include <stdarg.h>

VWRITEF(curfile, file, format, args)

	register struct iorec	*curfile;
	FILE			*file;
	char 			*format;
	va_list			args;
{

	if (curfile->funit & FREAD) {
		ERROR("%s: Attempt to write, but open for reading\n",
			curfile->pfname);
		return;
	}
	vfprintf(file, format, args);
	if (ferror(curfile->fbuf)) {
		PERROR("Could not write to ", curfile->pfname);
		return;
	}
}
