/*-
 * Copyright (c) 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)start.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "stdio.h"
#include "lrnref.h"
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>

lstart(lesson)
char *lesson;
{
	register struct dirent *ep;
	int c, n;
	char where [100];
	DIR *dp;

	if ((dp = opendir(".")) == NULL) {	/* clean up play directory */
		perror("Start:  play directory");
		wrapup(1);
	}
	while ((ep = readdir(dp)) != NULL) {
		if (ep->d_ino == 0)
			continue;
		n = ep->d_namlen;
		if (n >= 2 && ep->d_name[n-2] == '.' && ep->d_name[n-1] == 'c')
			continue;
		c = ep->d_name[0];
		if (c>='a' && c<= 'z')
			unlink(ep->d_name);
	}
	(void) closedir(dp);
	if (ask)
		return;
	sprintf(where, "%s/%s/L%s", direct, sname, lesson);
	if (access(where, R_OK)==0)	/* there is a file */
		return;
	perror(where);
	fprintf(stderr, "Start:  no lesson %s\n",lesson);
	wrapup(1);
}

fcopy(new,old)
char *new, *old;
{
	char b[BUFSIZ];
	int n, fn, fo;
	fn = creat(new, 0666);
	fo = open(old,0);
	if (fo<0) return;
	if (fn<0) return;
	while ( (n=read(fo, b, BUFSIZ)) > 0)
		write(fn, b, n);
	close(fn);
	close(fo);
}
