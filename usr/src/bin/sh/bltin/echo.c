/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)echo.c	5.1 (Berkeley) %G%
 */

/*
 * Echo command.
 */

#define main echocmd

#include "bltin.h"

#define EOF (-1)

main(argc, argv)  
	char **argv; 
{
	register char *p;
	register char c;
	int count;
	int nflag = 0;
	int eflag = 0;
	extern char *optarg;
	extern int optind, opterr;
	int ch;

	opterr = 0;
	while ((ch = getopt(argc, argv, "ne")) != EOF)
		switch((char)ch) {
		case 'n':
			nflag++;
			break;
		case 'e':
			eflag++;
			break;
		case '?':
		default:
			error("usage: %s [-ne] [arg]...", *argv);
			return 0;
		}
	argc -= optind;
	argv += optind;

	if (!eflag) {
		while (p = *argv++) {
			while (*p) {
				putchar(*p);
				p++;
			}
			if (*argv) putchar(' ');
		}
	} else {
		while (p = *argv++) {
			while (c = *p++) {
				if (c == '\\') {
					switch (*p++) {
					case 'b':  c = '\b';  break;
					case 'c':  return 0;	/* exit */
					case 'f':  c = '\f';  break;
					case 'n':  c = '\n';  break;
					case 'r':  c = '\r';  break;
					case 't':  c = '\t';  break;
					case 'v':  c = '\v';  break;
					case '\\':  break;	/* c = '\\' */
					case '0':	/* should be [0-7] */
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
			if (*argv) putchar(' ');
		}
	}
	if (!nflag)
		putchar('\n');
	return 0;
}

#ifndef SHELL
void
error(f, a)
{
	_doprnt(f, &a, stderr);
}
#endif
