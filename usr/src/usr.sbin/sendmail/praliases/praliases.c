/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
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
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)praliases.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sendmail.h>

typedef struct {
	char *dptr;
	int dsize;
} datum;


main(argc, argv)
	char **argv;
{
	extern char *optarg;
	extern int optind;
	static char *filename = "/usr/lib/aliases";
	datum content, key, firstkey(), nextkey(), fetch();
	int ch;

	while ((ch = getopt(argc, argv, "f:")) != EOF)
		switch((char)ch) {
		case 'f':
			filename = optarg;
			break;
		case '?':
		default:
			fputs("usage: praliases [-f file]\n", stderr);
			exit(EX_USAGE);
		}
	argc -= optind;
	argv += optind;

	if (dbminit(filename) < 0)
		exit(EX_OSFILE);
	if (!argc)
		for (key = firstkey(); key.dptr; key = nextkey(key)) {
			content = fetch(key);
			printf("%s:%s\n", key.dptr, content.dptr);
		}
	else for (; *argv; ++argv) {
		key.dptr = *argv;
		key.dsize = strlen(*argv) + 1;
		content = fetch(key);
		if (!content.dptr)
			printf("%s: No such key\n", key.dptr);
		else
			printf("%s:%s\n", key.dptr, content.dptr);
	}
	exit(EX_OK);
}
