/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
%sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)util.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>

prcopy(src, dest, len)
	register char *src, *dest;
	register int len;
{
	register int ch;

	while(len--) {
		ch = *src++;
		*dest++ = isprint(ch) ? ch : '?';
	}
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
	(void)fprintf(stderr, "usage: ls [-1ACFLRTacdfgiklqrstu] [file ...]\n");
	exit(1);
}
