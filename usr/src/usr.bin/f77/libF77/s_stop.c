/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)s_stop.c	5.2 (Berkeley) 4/12/91";
#endif /* not lint */

#include <stdio.h>

s_stop(s, n)
char *s;
long int n;
{
int i;

if(n > 0)
	{
	fprintf(stderr, "STOP: ");
	for(i = 0; i<n ; i++)
		putc(*s++, stderr);
	putc('\n', stderr);
	}
f_exit();
_cleanup();
exit(0);
}
