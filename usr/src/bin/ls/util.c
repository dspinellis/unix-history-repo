/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)util.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>

/**
 * prablelen --
 *	act like strlen, but also translate non-printing chars to '?'
 */
prablelen(cp)
	register char *cp;
{
	register int len;

	for (len = 0; *cp; ++len, ++cp)
		if (!isprint(*cp))
			*cp = '?';
	return(len);
}

char
*emalloc(size)
	u_int size;
{
	char *retval, *malloc();

	if (!(retval = malloc(size)))
		nomem();
	return(retval);
}

nomem()
{
	(void)fprintf(stderr, "ls: out of memory.\n");
	exit(1);
}

usage()
{
	(void)fprintf(stderr, "usage: ls [-1ACFLRacdfgilqrstu] [file ...]\n");
	exit(1);
}
