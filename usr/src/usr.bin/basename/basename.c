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
static char sccsid[] = "@(#)basename.c	4.4 (Berkeley) %G%";
#endif /* not lint */

main(argc, argv)
	int argc;
	char **argv;
{
	register char *p, *t;
	char *base;

	if (argc <= 1) {	/* backward compatible */
		putchar('\n');
		exit(1);
	}
	for (p = base = *++argv; *p;)
		if (*p++ == '/')
			base = p;
	if (argc > 2) {		/* original version allows any # of args */
		for (t = *++argv; *t; ++t);
		do {
			if (t == *argv) {
				*p = '\0';
				break;
			}
		} while (p >= base && *--t == *--p);
	}
	puts(base);
	exit(0);
}
