/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)echo.c	8.1 (Berkeley) %G%
 */

/*
 * Echo command.
 */

#define main echocmd

#include "bltin.h"

/* #define eflag 1 */

main(argc, argv)  char **argv; {
	register char **ap;
	register char *p;
	register char c;
	int count;
	int nflag = 0;
#ifndef eflag
	int eflag = 0;
#endif

	ap = argv;
	if (argc)
		ap++;
	if ((p = *ap) != NULL) {
		if (equal(p, "-n")) {
			nflag++;
			ap++;
		} else if (equal(p, "-e")) {
#ifndef eflag
			eflag++;
#endif
			ap++;
		}
	}
	while ((p = *ap++) != NULL) {
		while ((c = *p++) != '\0') {
			if (c == '\\' && eflag) {
				switch (*p++) {
				case 'b':  c = '\b';  break;
				case 'c':  return 0;		/* exit */
				case 'f':  c = '\f';  break;
				case 'n':  c = '\n';  break;
				case 'r':  c = '\r';  break;
				case 't':  c = '\t';  break;
				case 'v':  c = '\v';  break;
				case '\\':  break;		/* c = '\\' */
				case '0':
					c = 0;
					count = 3;
					while (--count >= 0 && (unsigned)(*p - '0') < 8)
						c = (c << 3) + (*p++ - '0');
					break;
				default:
					p--;
					break;
				}
			}
			putchar(c);
		}
		if (*ap)
			putchar(' ');
	}
	if (! nflag)
		putchar('\n');
	return 0;
}
