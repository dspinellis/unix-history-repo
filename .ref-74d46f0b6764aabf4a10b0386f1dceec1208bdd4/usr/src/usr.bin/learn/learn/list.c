/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)list.c	4.4 (Berkeley) %G%";
#endif /* not lint */

#include "stdio.h"
#include "lrnref.h"
#include "signal.h"

int istop;

list(r)
char *r;
{
	void stop(), intrpt();
	FILE *ft;
	char s[100];

	if (r==0)
		return;
	istop = 1;
	signal(SIGINT, stop);
	ft = fopen(r, "r");
	if (ft != NULL) {
		while (fgets(s, 100, ft) && istop)
			fputs(s, stdout);
		fclose(ft);
	}
	signal(SIGINT, intrpt);
}

void
stop()
{
	istop=0;
}
