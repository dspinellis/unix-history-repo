/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)REWRITE.c 1.2 %G%";

#include "h00vars.h"
#include "h01errs.h"

REWRITE(filep, name, maxnamlen, datasize)

	register struct iorec	*filep;
	char			*name;
	long			maxnamlen;
	long			datasize;
{
	filep = GETNAME (filep, name, maxnamlen, datasize);
	filep->fbuf = fopen(filep->fname, "w");
	if (filep->fbuf == NULL) {
		ERROR(ECREATE, filep->pfname);
		return;
	}
	filep->funit |= (EOFF | FWRITE);
	if (filep->fblk > PREDEF) {
		setbuf(filep->fbuf, &filep->buf[0]);
	}
}
