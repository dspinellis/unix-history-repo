/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)lnblnk_.c	5.3 (Berkeley) 4/12/91";
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
