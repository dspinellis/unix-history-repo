/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)RESET.c 1.5 %G%";

#include "h00vars.h"

RESET(filep, name, maxnamlen, datasize)

	register struct iorec	*filep;
	char			*name;
	long			maxnamlen;
	long			datasize;
{
	if (name == NULL && filep == INPUT && filep->fname[0] == '\0') {
		if (fseek(filep->fbuf, (long)0, 0)) {
			PERROR("Could not reset ", filep->pfname);
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
		PERROR("Could not open ", filep->pfname);
		return;
	}
	filep->funit |= (SYNC | FREAD);
	if (filep->funit & FTEXT)
		filep->funit |= EOLN;
	if (filep->fblk > PREDEF) {
		setbuf(filep->fbuf, &filep->buf[0]);
	}
}
