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
static char sccsid[] = "@(#)READE.c	1.8 (Berkeley) 4/9/90";
#endif /* not lint */

#include "h00vars.h"

long
READE(curfile, name)

	register struct iorec	*curfile;
	char			*name;
{
	register short	*sptr;
	register int	len;
	register int	nextlen;
	register int	cnt;
	char		*cp;
	char		namebuf[NAMSIZ];
	int		retval;

	if (curfile->funit & FWRITE) {
		ERROR("%s: Attempt to read, but open for writing\n",
			curfile->pfname);
	}
	UNSYNC(curfile);
	retval = fscanf(curfile->fbuf,
	    "%*[ \t\n]%74[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789]",
	    namebuf);
	if (retval == EOF) {
		ERROR("%s: Tried to read past end of file\n", curfile->pfname);
	}
	if (retval == 0)
		goto ename;
	for (len = 0; len < NAMSIZ && namebuf[len]; len++)
		/* void */;
	len++;
	sptr = (short *)name;
	cnt = *sptr++;
	cp = name + sizeof (short) + *sptr;
	do	{
		nextlen = *sptr++;
		nextlen = *sptr - nextlen;
		if (nextlen == len && RELEQ(len, namebuf, cp)) {
			return *((short *) name) - cnt;
		}
		cp += (int)nextlen;
	} while (--cnt);
ename:
	ERROR("Unknown name \"%s\" found on enumerated type read\n", namebuf);
	return 0;
}
