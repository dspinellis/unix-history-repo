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
static char sccsid[] = "@(#)users.c	5.7 (Berkeley) %G%";
#endif /* not lint */

/*
 * users
 */
#include <sys/types.h>
#include <utmp.h>
#include <stdio.h>

#define NMAX		sizeof(utmp.ut_name)
#define MAXUSERS	200

static struct utmp utmp;		/* read structure */
static int ncnt;			/* count of names */
static char *names[MAXUSERS];		/* names table */

main()
{
	register FILE *fp;		/* file pointer */

	if (!(fp = fopen(_PATH_UTMP, "r"))) {
		(void)fprintf(stderr, "users: can't open %s.\n", _PATH_UTMP);
		exit(1);
	}
	while (fread((char *)&utmp, sizeof(utmp), 1, fp) == 1)
		if (*utmp.ut_name) {
			if (++ncnt > MAXUSERS) {
				ncnt = MAXUSERS;
				fputs("users: too many users.\n", stderr);
				break;
			}
			nsave();
		}
	summary();
	exit(0);
}

nsave()
{
	static char **namp = names;	/* pointer to names table */
	char *calloc();

	if (!(*namp = calloc((u_int)(NMAX + 1), sizeof(char)))) {
		fputs("users: malloc error.\n", stderr);
		exit(1);
	}
	bcopy(utmp.ut_name, *namp++, NMAX);
}

summary()
{
	register char **p;
	int scmp();

	if (!ncnt)
		return;
	qsort((char *)names, ncnt, sizeof(names[0]), scmp);
	fputs(names[0], stdout);
	for (p = &names[1]; --ncnt; ++p) {
		putchar(' ');
		fputs(*p, stdout);
	}
	putchar('\n');
}

scmp(p, q)
	char **p, **q;
{
	return(strcmp(*p, *q));
}
