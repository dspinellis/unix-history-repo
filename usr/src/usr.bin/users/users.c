/*
 * Copyright (c) 1980, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)users.c	5.9 (Berkeley) 6/1/90";
#endif /* not lint */

#include <sys/types.h>
#include <errno.h>
#include <utmp.h>
#include <stdio.h>

#define	MAXUSERS	200

main()
{
	register int cnt, ncnt;
	struct utmp utmp;
	char names[MAXUSERS][UT_NAMESIZE];
	int scmp();

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
		(void)printf("%s", names[0]);
		for (cnt = 1; cnt < ncnt; ++cnt) {
			while (cnt < ncnt - 1 &&
			    !strncmp(names[cnt], names[cnt + 1], UT_NAMESIZE))
				++cnt;
			(void)printf(" %.*s", UT_NAMESIZE, names[cnt]);
		}
		(void)printf("\n");
	}
	exit(0);
}

scmp(p, q)
	char *p, *q;
{
	return(strcmp(p, q));
}
