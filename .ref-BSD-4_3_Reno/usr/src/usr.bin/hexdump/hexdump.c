/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)hexdump.c	5.5 (Berkeley) 6/1/90";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include "hexdump.h"

FS *fshead;				/* head of format strings */
int blocksize;				/* data block size */
int exitval;				/* final exit value */
int length = -1;			/* max bytes to read */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno;
	register FS *tfs;
	char *p, *rindex();

	if (!(p = rindex(argv[0], 'o')) || strcmp(p, "od"))
		newsyntax(argc, &argv);
	else
		oldsyntax(argc, &argv);

	/* figure out the data block size */
	for (blocksize = 0, tfs = fshead; tfs; tfs = tfs->nextfs) {
		tfs->bcnt = size(tfs);
		if (blocksize < tfs->bcnt)
			blocksize = tfs->bcnt;
	}
	/* rewrite the rules, do syntax checking */
	for (tfs = fshead; tfs; tfs = tfs->nextfs)
		rewrite(tfs);

	(void)next(argv);
	display();
	exit(exitval);
}
