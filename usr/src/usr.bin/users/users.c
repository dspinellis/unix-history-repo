/*
 * Copyright (c) 1980, 1987 Regents of the University of California.
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
"@(#) Copyright (c) 1980, 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)users.c	5.8 (Berkeley) %G%";
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
