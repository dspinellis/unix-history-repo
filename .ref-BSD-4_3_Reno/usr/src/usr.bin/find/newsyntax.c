/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)newsyntax.c	5.2 (Berkeley) 5/22/90";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fts.h>
#include "find.h"

newsyntax(argc, argvp)
	int argc;
	char ***argvp;
{
	extern char *optarg;
	extern int depth, optind;
	int ch;
	char **argv, **cur;

	cur = argv = *argvp;
	while ((ch = getopt(argc, argv, "df:sx")) != EOF)
		switch(ch) {
		case 'd':
			depth = 1;
			break;
		case 'f':
			*cur++ = optarg;
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
