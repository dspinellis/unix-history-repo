/*-
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)getwd.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "uucp.h"

/*
 *	get working directory
 *
 *	return codes  0 = FAIL
 *		      wkdir = SUCCES
 */

char *
getwd(wkdir)
register char *wkdir;
{
	register FILE *fp;
	extern FILE *rpopen();
	extern int rpclose();
	register char *c;

	*wkdir = '\0';
	if ((fp = rpopen("PATH=/bin:/usr/bin;pwd 2>&-", "r")) == NULL)
		return 0;
	if (fgets(wkdir, 100, fp) == NULL) {
		rpclose(fp);
		return 0;
	}
	if (*(c = wkdir + strlen(wkdir) - 1) == '\n')
		*c = '\0';
	rpclose(fp);
	return wkdir;
}
