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
static char sccsid[] = "@(#)READ4.c	1.10 (Berkeley) 4/9/90";
#endif /* not lint */

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
	}
	UNSYNC(curfile);
	errno = 0;
	retval = fscanf(curfile->fbuf, "%ld", &data);
	if (retval == EOF) {
		ERROR("%s: Tried to read past end of file\n", curfile->pfname);
	}
	if (retval == 0) {
		ERROR("%s: Bad data found on integer read\n", curfile->pfname);
	}
	if (errno == ERANGE) {
		ERROR("%s: Overflow on integer read\n", curfile->pfname);
	}
	if (errno != 0) {
		PERROR("Error encountered on integer read ", curfile->pfname);
	}
	return data;
}
