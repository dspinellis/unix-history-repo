/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)READ4.c 1.8 10/28/83";

#include "h00vars.h"
#include <errno.h>
extern int errno;

long
READ4(curfile)

	register struct iorec	*curfile;
{
	long			data;
	int			retval;

	if (curfile->funit & FWRITE) {
		ERROR("%s: Attempt to read, but open for writing\n",
			curfile->pfname);
		return;
	}
	UNSYNC(curfile);
	errno = 0;
	retval = fscanf(curfile->fbuf, "%ld", &data);
	if (retval == EOF) {
		ERROR("%s: Tried to read past end of file\n", curfile->pfname);
		return;
	}
	if (retval == 0) {
		ERROR("%s: Bad data found on integer read\n", curfile->pfname);
		return;
	}
	if (errno == ERANGE) {
		ERROR("%s: Overflow on integer read\n", curfile->pfname);
		return;
	}
	if (errno != 0) {
		PERROR("Error encountered on integer read ", curfile->pfname);
		return;
	}
	return data;
}
