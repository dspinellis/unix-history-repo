/*
 * Copyright (c) 1986 Regents of the University of California.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1986 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)20b.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	register int bsize, cc, want;
	register char *base, *current;
	char *alloca();

	if (argc > 1) {
		bsize = atoi(argv[1]);
		if (bsize <= 0) {
			fputs("20b: bad block size.\n", stderr);
			exit(-1);
		}
	}
	else
		bsize = 20 * 512;
	base = alloca(bsize);
	for (cc = bsize; cc > 0;) {
		current = base;
		for (want = bsize; want > 0 && cc > 0; want -= cc) {
			if ((cc = read(0, current, want)) < 0)
				return(-1);
			current += cc;
		}
		want = bsize - want;
		if (want && write(1, base, want) != want)
			return(-1);
	}
	return(0);
}
