/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)RESET.c	1.9 (Berkeley) 4/9/90";
#endif /* not lint */

#include "h00vars.h"

RESET(filep, name, maxnamlen, datasize)

	register struct iorec	*filep;
	char			*name;
	long			maxnamlen;
	long			datasize;
{
	if (name == NULL && filep == INPUT && filep->fname[0] == '\0') {
		if (fseek(filep->fbuf, (long)0, 0) == -1) {
			PERROR("Could not reset ", filep->pfname);
			return;
		}
		filep->funit &= ~EOFF;
		filep->funit |= (SYNC | EOLN);
		return;
	}
	filep = GETNAME(filep, name, maxnamlen, datasize);
	filep->fbuf = fopen(filep->fname, "r");
	if (filep->fbuf == NULL) {
		/*
		 * This allows unnamed temp files to be opened even if
		 * they have not been rewritten yet. We decided to remove
		 * this feature since the standard requires that files be
		 * defined before being reset.
		 *
		if (filep->funit & TEMP) {
			filep->funit |= (EOFF | SYNC | FREAD);
			return;
		}
		 */
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
