/*-
 * Copyright (c) 1992, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)fmt.c	8.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/resource.h>

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vis.h>

#include "ps.h"

static char *cmdpart __P((char *));
static char *shquote __P((char **));

/*
 * XXX
 * This is a stub until marc does the real one.
 */
static char *
shquote(argv)
	char **argv;
{
	char **p, *dst, *src;
	static char buf[4096];		/* XXX */

	if (*argv == 0) {
		buf[0] = 0;
		return (buf);
	}
	dst = buf;
	for (p = argv; (src = *p++) != 0; ) {
		if (*src == 0)
			continue;
		strvis(dst, src, VIS_NL | VIS_CSTYLE);
		while (*dst)
			dst++;
		*dst++ = ' ';
	}
	if (dst != buf)
		--dst;
	*dst = '\0';
	return (buf);
}

static char *
cmdpart(arg0)
	char *arg0;
{
	char *cp;

	return ((cp = strrchr(arg0, '/')) != NULL ? cp + 1 : arg0);
}

char *
fmt_argv(argv, cmd, maxlen)
	char **argv;
	char *cmd;
	int maxlen;
{
	int len;
	char *ap, *cp;

	if (argv == 0 || argv[0] == 0) {
		if (cmd == NULL)
			return ("");
		ap = NULL;
		len = maxlen + 3;
	} else {
		ap = shquote(argv);
		len = strlen(ap) + maxlen + 4;
	}
	if ((cp = malloc(len)) == NULL)
		return (NULL);
	if (ap == NULL)
		sprintf(cp, "(%.*s)", maxlen, cmd);
	else if (strncmp(cmdpart(argv[0]), cmd, maxlen) != 0)
		sprintf(cp, "%s (%.*s)", ap, maxlen, cmd);
	else
		(void) strcpy(cp, ap);
	return (cp);
}
