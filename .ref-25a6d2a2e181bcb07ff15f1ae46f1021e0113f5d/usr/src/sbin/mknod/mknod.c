/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kevin Fall.
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
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mknod.c	4.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno;
	u_short mode;
	char *strerror();

	if (argc != 5) {
		(void)fprintf(stderr,
		    "usage: mknod name [b | c] major minor\n");
		exit(1);
	}

	mode = 0666;
	if (argv[2][0] == 'c')
		mode |= S_IFCHR;
	else if (argv[2][0] == 'b')
		mode |= S_IFBLK;
	else {
		(void)fprintf(stderr,
		    "mknod: node must be type 'b' or 'c'.\n");
		exit(1);
	}

	if (mknod(argv[1], mode, makedev(atoi(argv[3]), atoi(argv[4]))) < 0) {
		(void)fprintf(stderr,
		    "mknod: %s: %s\n", argv[1], strerror(errno));
		exit(1);
	}
	exit(0);
}
