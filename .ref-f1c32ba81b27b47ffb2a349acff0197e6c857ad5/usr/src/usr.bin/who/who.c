/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)who.c	5.11 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <sys/time.h>
#include <pwd.h>
#include <utmp.h>
#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	register char *p;
	struct utmp usr;
	struct passwd *pw;
	FILE *ufp, *file();
	char *t, *rindex(), *strcpy(), *strncpy(), *ttyname();
	time_t time();

	switch (argc) {
	case 1:					/* who */
		ufp = file(_PATH_UTMP);
		/* only entries with both name and line fields */
		while (fread((char *)&usr, sizeof(usr), 1, ufp) == 1)
			if (*usr.ut_name && *usr.ut_line)
				output(&usr);
		break;
	case 2:					/* who utmp_file */
		ufp = file(argv[1]);
		/* all entries */
		while (fread((char *)&usr, sizeof(usr), 1, ufp) == 1)
			output(&usr);
		break;
	case 3:					/* who am i */
		ufp = file(_PATH_UTMP);

		/* search through the utmp and find an entry for this tty */
		if (p = ttyname(0)) {
			/* strip any directory component */
			if (t = rindex(p, '/'))
				p = t + 1;
			while (fread((char *)&usr, sizeof(usr), 1, ufp) == 1)
				if (usr.ut_name && !strcmp(usr.ut_line, p)) {
					output(&usr);
					exit(0);
				}
			/* well, at least we know what the tty is */
			(void)strncpy(usr.ut_line, p, UT_LINESIZE);
		} else
			(void)strcpy(usr.ut_line, "tty??");
		pw = getpwuid(getuid());
		(void)strncpy(usr.ut_name, pw ? pw->pw_name : "?", UT_NAMESIZE);
		(void)time(&usr.ut_time);
		*usr.ut_host = '\0';
		output(&usr);
		break;
	default:
		(void)fprintf(stderr, "usage: who [ file ]\n       who am i\n");
		exit(1);
	}
	exit(0);
}

output(up)
	struct utmp *up;
{
	char *ctime();

	(void)printf("%-*.*s %-*.*s", UT_NAMESIZE, UT_NAMESIZE, up->ut_name,
	    UT_LINESIZE, UT_LINESIZE, up->ut_line);
	(void)printf("%.12s", ctime(&up->ut_time) + 4);
	if (*up->ut_host)
		printf("\t(%.*s)", UT_HOSTSIZE, up->ut_host);
	(void)putchar('\n');
}

FILE *
file(name)
	char *name;
{
	extern int errno;
	FILE *ufp;
	char *strerror();

	if (!(ufp = fopen(name, "r"))) {
		(void)fprintf(stderr, "who: %s: %s.\n", name, strerror(errno));
		exit(1);
	}
	return(ufp);
}
