/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)APPEND.c 1.1 10/31/80";

#include "h00vars.h"
#include "h01errs.h"

APPEND(filep)

	register struct iorec	*filep;
{
	filep = GETNAME (filep, 0, 0, 0);
	filep->fbuf = fopen(filep->fname, "a");
	if (filep->fbuf == NULL) {
		ERROR(EOPEN, filep->pfname);
		return;
	}
	filep->funit |= (EOFF | FWRITE);
	if (filep->fblk > PREDEF) {
		setbuf(filep->fbuf, &filep->buf[0]);
	}
}
