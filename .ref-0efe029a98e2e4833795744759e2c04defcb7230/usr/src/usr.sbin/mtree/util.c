/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)util.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char *
rlink(name)
	char *name;
{
	extern char path[];
	int len;
	static char lbuf[MAXPATHLEN];

	len = readlink(name, lbuf, sizeof(lbuf));
	if (len == -1) {
		(void)fprintf(stderr, "mtree: %s: %s.\n",
		    path + 2, strerror(errno));
		exit(1);
	}
	lbuf[len] = '\0';
	return(lbuf);
}

char *
emalloc(size)
	int size;
{
	char *p;

	/* NOSTRICT */
	if (!(p = malloc((u_int)size)))
		nomem();
	bzero(p, size);
	return(p);
}

nomem()
{
	(void)fprintf(stderr, "mtree: %s.\n", strerror(ENOMEM));
	exit(1);
}
