/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)REWRITE.c	1.4 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

REWRITE(filep, name, maxnamlen, datasize)

	register struct iorec	*filep;
	char			*name;
	long			maxnamlen;
	long			datasize;
{
	filep = GETNAME (filep, name, maxnamlen, datasize);
	filep->fbuf = fopen(filep->fname, "w");
	if (filep->fbuf == NULL) {
		PERROR("Could not create ",filep->pfname);
		return;
	}
	filep->funit |= (EOFF | FWRITE);
	if (filep->fblk > PREDEF) {
		setbuf(filep->fbuf, &filep->buf[0]);
	}
}
