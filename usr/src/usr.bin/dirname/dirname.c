/*
 * Copyright (c) 1987 Regents of the University of California.
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
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)dirname.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	char *p, *rindex();

	if (argc != 2) {
		fprintf(stderr, "usage: dirname path\n");
		exit(1);
	}
	if (p = rindex(*++argv, '/'))
		if (p > *argv)
			*p = '\0';
		else
			*++p = '\0';
	printf("%s\n", *argv);
	exit(0);
}
