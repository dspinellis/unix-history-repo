/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RESET.c 1.1 %G%";

#include "h00vars.h"
#include "h01errs.h"

RESET(filep, name, maxnamlen, datasize)

	register struct iorec	*filep;
	char			*name;
	int			maxnamlen;
	int			datasize;
{
	if (name == NULL && filep == INPUT && filep->fname[0] == '\0') {
		if (rewind(filep->fbuf)) {
			ERROR(ESEEK, filep->pfname);
			return;
		}
		filep->funit &= ~(EOFF | EOLN);
		filep->funit |= SYNC;
		return;
	}
	filep = GETNAME(filep, name, maxnamlen, datasize);
	filep->fbuf = fopen(filep->fname, "r");
	if (filep->fbuf == NULL) {
		if (filep->funit & TEMP) {
			filep->funit |= (EOFF | SYNC | FREAD);
			return;
		}
		ERROR(EOPEN, filep->pfname);
		return;
	}
	filep->funit |= (SYNC | FREAD);
	if (filep->fblk > PREDEF) {
		setbuf(filep->fbuf, &filep->buf[0]);
	}
}
