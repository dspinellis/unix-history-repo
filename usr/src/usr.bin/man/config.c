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
static char sccsid[] = "@(#)config.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "pathnames.h"

extern char *progname;

/*
 * config --
 *	read in the configuration file, convert it to a colon separated
 *	path.
 */
char *
config()
{
	register char *p;
	u_int buflen;
	int len;
	char *buf, *endp, line[256], *realloc();

	if (!freopen(_PATH_MANCONF, "r", stdin)) {
		(void)fprintf(stderr, "%s: no configuration file %s.\n",
		    progname, _PATH_MANCONF);
		exit(1);
	}
	buflen = 0;
	buf = endp = p = NULL;
	while (fgets(line, sizeof(line), stdin)) {
		if (!index(line, '\n')) {
			(void)fprintf(stderr, "%s: config line too long.\n",
			    progname);
			exit(1);
		}
		len = strcspn(line, " \t\n");
		if (!len || *line == '#')
			continue;
		if (!p || p + len + 2 >= endp) {
			if (!(buf = realloc(buf, buflen += 1024)))
				enomem();
			if (!p)
				p = buf;
			endp = buf + buflen;
		}
		bcopy(line, p, len);
		p += len;
		*p++ = ':';
	}
	if (!buf)
		return((char *)NULL);
	*--p = '\0';
	return(buf);
}

enomem()
{
	extern int errno;
	char *strerror();

	(void)fprintf(stderr, "%s: %s\n", strerror(ENOMEM), progname);
	exit(1);
}
