/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fts.h>
#include "find.h"

void
newsyntax(argc, argvp)
	int argc;
	char ***argvp;
{
	extern int optind;
	extern char *optarg;
	int ch;
	char **argv, **cur;

	cur = argv = *argvp;
	while ((ch = getopt(argc, argv, "df:rsx")) != EOF)
		switch(ch) {
		case 'd':
			isdepth = 1;
			break;
		case 'f':
			*cur++ = optarg;
			break;
		case 'r':
			isrelative = 1;
			break;
		case 's':
			ftsoptions &= ~FTS_PHYSICAL;
			ftsoptions |= FTS_LOGICAL;
			break;
		case 'x':
			ftsoptions &= ~FTS_NOSTAT;
			ftsoptions |= FTS_XDEV;
			break;
		case '?':
		default:
			usage();
		}

	*argvp += optind;
	if (cur == argv) {
		if (!**argvp)
			usage();
		*cur++ = **argvp;
		++*argvp;
	}
	*cur = NULL;
}
