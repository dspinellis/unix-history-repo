/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)getarg_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * return a specified command line argument
 *
 * calling sequence:
 *	character*20 arg
 *	call getarg(k, arg)
 * where:
 *	arg will receive the kth unix command argument
*/

getarg_(n, s, ls)
long int *n;
register char *s;
long int ls;
{
extern int xargc;
extern char **xargv;
register char *t;
register int i;

if(*n>=0 && *n<xargc)
	t = xargv[*n];
else
	t = "";
for(i = 0; i<ls && *t!='\0' ; ++i)
	*s++ = *t++;
for( ; i<ls ; ++i)
	*s++ = ' ';
}
