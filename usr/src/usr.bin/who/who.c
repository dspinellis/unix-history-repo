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
static char sccsid[] = "@(#)who.c	5.7 (Berkeley) %G%";
#endif not lint

/*
 * who
 */

#include <sys/types.h>
#include <utmp.h>
#include <pwd.h>
#include <stdio.h>
#include <ctype.h>
#include "pathnames.h"

#define NMAX sizeof(utmp.ut_name)
#define LMAX sizeof(utmp.ut_line)
#define	HMAX sizeof(utmp.ut_host)

struct	utmp utmp;
struct	passwd *pw;
struct	passwd *getpwuid();

char	*ttyname(), *rindex(), *ctime(), *strcpy();

main(argc, argv)
	int argc;
	char **argv;
{
	register char *tp, *s;
	register FILE *fi;

	s = _PATH_UTMP;
	if(argc == 2)
		s = argv[1];
	if (argc == 3) {
		tp = ttyname(0);
		if (tp)
			tp = rindex(tp, '/') + 1;
		else {	/* no tty - use best guess from passwd file */
			(void)strcpy(utmp.ut_line, "tty??");
			guess();
			exit(0);
		}
	}
	if (!(fi = fopen(s, "r"))) {
		fprintf(stderr, "who: cannot read %s.\n", s);
		exit(1);
	}
	while (fread((char *)&utmp, sizeof(utmp), 1, fi) == 1) {
		if (argc == 3) {
			if (strcmp(utmp.ut_line, tp))
				continue;
			if (!utmp.ut_name[0])
				guess();
			else
				putline();
			exit(0);
		}
		if (utmp.ut_name[0] == '\0' && argc == 1)
			continue;
		putline();
	}
	if (argc == 3) {
		strncpy(utmp.ut_line, tp, sizeof(utmp.ut_line));
		guess();
	}
	exit(0);
}

putline()
{
	register char *cbuf;

	printf("%-*.*s %-*.*s",
		NMAX, NMAX, utmp.ut_name,
		LMAX, LMAX, utmp.ut_line);
	cbuf = ctime(&utmp.ut_time);
	printf("%.12s", cbuf+4);
	if (utmp.ut_host[0])
		printf("\t(%.*s)", HMAX, utmp.ut_host);
	putchar('\n');
}

guess()
{
	pw = getpwuid(getuid());
	strncpy(utmp.ut_name, pw ? pw->pw_name : "?", NMAX);
	time(&utmp.ut_time);
	putline();
}
