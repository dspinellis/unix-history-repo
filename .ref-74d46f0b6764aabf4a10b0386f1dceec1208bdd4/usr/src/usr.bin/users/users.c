/*
 * Copyright (c) 1980, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)users.c	5.12 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <utmp.h>
#include <stdio.h>

#define	MAXUSERS	200

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	register int cnt, ncnt;
	struct utmp utmp;
	char names[MAXUSERS][UT_NAMESIZE];
	int ch, scmp();

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch(ch) {
		case '?':
		default:
			(void)fprintf(stderr, "usage: users\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;

	if (!freopen(_PATH_UTMP, "r", stdin)) {
		(void)fprintf(stderr, "users: can't open %s.\n", _PATH_UTMP);
		exit(1);
	}
	for (ncnt = 0;
	    fread((char *)&utmp, sizeof(utmp), 1, stdin) == 1;)
		if (*utmp.ut_name) {
			if (ncnt == MAXUSERS) {
				(void)fprintf(stderr,
				    "users: too many users.\n");
				break;
			}
			(void)strncpy(names[ncnt], utmp.ut_name, UT_NAMESIZE);
			++ncnt;
		}

	if (ncnt) {
		qsort(names, ncnt, UT_NAMESIZE, scmp);
		(void)printf("%.*s", UT_NAMESIZE, names[0]);
		for (cnt = 1; cnt < ncnt; ++cnt)
			if (strncmp(names[cnt], names[cnt - 1], UT_NAMESIZE))
				(void)printf(" %.*s", UT_NAMESIZE, names[cnt]);
		(void)printf("\n");
	}
	exit(0);
}

scmp(p, q)
	char *p, *q;
{
	return(strncmp(p, q, UT_NAMESIZE));
}
