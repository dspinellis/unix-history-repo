/*
 * Copyright (c) 1988 Regents of the University of California.
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
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)praliases.c	5.3 (Berkeley) %G%";
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
