/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)users.c	5.3 (Berkeley) %G%";
#endif not lint

/*
 * users
 */
#include <sys/types.h>
#include <utmp.h>
#include <stdio.h>

#define ERREXIT		1
#define OKEXIT		0
#define NMAX		sizeof(utmp.ut_name)
#define MAXUSERS	200

static struct utmp	utmp;		/* read structure */
static int	ncnt;			/* count of names */
static char	*names[MAXUSERS],	/* names table */
		**namp;			/* pointer to names table */

main(argc,argv)
int	argc;
char	**argv;
{
	register FILE	*fp;		/* file pointer */
	char	*fname;

	if (argc > 2) {
		fputs("usage: users [ utmp_file ]\n",stderr);
		exit(ERREXIT);
	}
	fname = argc == 2 ? argv[1] : "/etc/utmp";
	if (!(fp = fopen(fname,"r"))) {
		perror(fname);
		exit(ERREXIT);
	}
	namp = names;
	while (fread((char *)&utmp,sizeof(utmp),1,fp) == 1)
		if (*utmp.ut_name) {
			if (++ncnt > MAXUSERS) {
				ncnt = MAXUSERS;
				fputs("users: too many users.\n",stderr);
				break;
			}
			nsave();
		}
	if (ncnt)
		summary();
	exit(OKEXIT);
}

nsave()
{
	char	*calloc();

	if (!(*namp = calloc((u_int)(NMAX + 1),sizeof(char)))) {
		fputs("users: malloc error.\n",stderr);
		exit(ERREXIT);
	}
	bcopy(utmp.ut_name,*namp++,NMAX);
}

summary()
{
	register char	**p,
			**q;
	int	scmp();

	qsort((char *)names,ncnt,sizeof(names[0]),scmp);
	for (p = names;p < namp;p = q) {
		if (p != names)
			putchar(' ');
		fputs(*p,stdout);
		for (q = p + 1;q < namp && !strcmp(*q,*p);++q);
	}
	putchar('\n');
}

scmp(p,q)
char	**p,
	**q;
{
	return(strcmp(*p,*q));
}
