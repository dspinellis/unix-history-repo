/*
 * Copyright (c) 1983 The Regents of the University of California.
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
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rmdir.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * Remove directory
 */
#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	int errors;

	if (argc < 2) {
		fprintf(stderr, "usage: rmdir directory ...\n");
		exit(1);
	}
	for (errors = 0; *++argv;)
		if (rmdir(*argv) < 0) {
			fprintf(stderr, "rmdir: ");
			perror(*argv);
			errors = 1;
		}
	exit(errors);
}
