/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)lnblnk_.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*
 * find last occurrence of a non-blank character in string
 *
 * calling sequence:
 *	character*(*) string
 *	indx = lnblnk (string)
 * where:
 *	indx will be the index of the last occurence
 *	of a non-blank character in string, or zero if not found.
 */

long lnblnk_(str, slen)
char *str; long slen;
{
	register char *p = str + slen;

	while (--p >= str && *p == ' ' ) ;
	return((long)(++p - str));
}
