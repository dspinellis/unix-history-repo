/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)symlnk_.c	5.2 (Berkeley) 4/12/91";
#endif /* not lint */

/*
 * make a symbolic link to a file
 *
 * calling sequence:
 *	ierror = symlnk(name1, name2)
 * where:
 *	name1 is the pathname of an existing file
 *	name2 is a pathname that will become a symbolic link to name1
 *	ierror will be 0 if successful; a system error code otherwise.
 */

#include <sys/param.h>
#include "../libI77/f_errno.h"

#ifndef	MAXPATHLEN
#define	MAXPATHLEN	128
#endif

long symlnk_(name1, name2, n1len, n2len)
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
	if (symlink(buf1, buf2) != 0)
		return((long)errno);
	return(0L);
}
