/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)APPEND.c 1.2 6/10/81";

#include "h00vars.h"

APPEND(filep)

	register struct iorec	*filep;
{
	filep = GETNAME (filep, 0, 0, 0);
	filep->fbuf = fopen(filep->fname, "a");
	if (filep->fbuf == NULL) {
		PERROR("Could not open ", filep->pfname);
		return;
	}
	filep->funit |= (EOFF | FWRITE);
	if (filep->fblk > PREDEF) {
		setbuf(filep->fbuf, &filep->buf[0]);
	}
}
