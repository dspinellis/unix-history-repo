/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)link_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * make a link to an existing file
 *
 * calling sequence:
 *	ierror = link(name1, name2)
 * where:
 *	name1 is the pathname of an existing file
 *	name2 is a pathname to be linked to file name1
 *	ierror will be 0 if successful; a system error code otherwise.
 */

#include "../libI77/f_errno.h"
#include <sys/param.h>
#ifndef	MAXPATHLEN
#define MAXPATHLEN	128
#endif

long link_(name1, name2, n1len, n2len)
char *name1, *name2;
long n1len, n2len;
{
	char buf1[MAXPATHLEN];
	char buf2[MAXPATHLEN];

	if (n1len >= sizeof buf1 || n2len >= sizeof buf2)
		return((long)(errno=F_ERARG));
	g_char(name1, n1len, buf1);
	g_char(name2, n2len, buf2);
	if (buf1[0] == '\0' || buf2[0] == '\0')
		return((long)(errno=F_ERARG));
	if (link(buf1, buf2) != 0)
		return((long)errno);
	return(0L);
}
