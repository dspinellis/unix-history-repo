/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)dirname.c	5.1 (Berkeley) %G%";
#endif /* not lint */

main(argc, argv)
	int argc;
	char **argv;
{
	char *p, *rindex();

	p = rindex(*++argv, '/');
	if (p && p > *argv)
		*p = '\0';
	puts(*argv);
	exit(0);
}
