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
static char sccsid[] = "@(#)who.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * who
 */

#include <sys/param.h>
#include <utmp.h>
#include <pwd.h>
#include <stdio.h>
#include <strings.h>
#include <ctype.h>

#define NMAX	sizeof(utmp.ut_name)
#define LMAX	sizeof(utmp.ut_line)
#define HMAX	sizeof(utmp.ut_host)

static struct utmp	utmp;		/* read buffer */

main(argc,argv)
int	argc;
char	**argv;
{
	register FILE	*fp;			/* utmp file pointer */
	register char	*tp,			/* tty name */
			*fname;			/* utmp file name */
	struct passwd	*pw,			/* user passwd structure */
			*getpwuid();
	char	hostname[MAXHOSTNAMELEN],	/* host name */
		*ttyname();
	uid_t	getuid();
	long	time();

	switch(argc) {
		case 2:
			fname = argv[1];
			break;
		case 3:
			if (!(tp = ttyname(0))) {
				/*
				 * no tty -- use best guess from passwd file.
				 * next line is a kludge, but as of now getuid
				 * returns a "uid_t" and getpwuid takes an int.
				 */
				pw = getpwuid((int)getuid());
				strncpy(utmp.ut_name,pw ? pw->pw_name : "?",NMAX);
				strcpy(utmp.ut_line,"tty??");
				time(&utmp.ut_time);
				putline();
				exit(0);
			}
			tp = rindex(tp,'/') + 1;
			if (gethostname(hostname,sizeof(hostname)) == -1) {
				perror("gethostname");
				exit(1);
			}
		case 1:
			fname = "/etc/utmp";
			break;
		default:
			fputs("usage: who [ utmp_file ]\nor who am i\n",stderr);
			exit(1);
	}
	if (!(fp = fopen(fname,"r"))) {
		perror(fname);
		exit(1);
	}
	while (fread((char *)&utmp,sizeof(utmp),1,fp) == 1)
		if (argc == 3) {
			if (!strcmp(utmp.ut_line,tp)) {
				printf("%s!",hostname);
				putline();
				exit(0);
			}
		}
		else if (argc != 1 || *utmp.ut_name)
			putline();
}

putline()
{
	register char	*cbuf;
	char	*ctime();

	cbuf = ctime(&utmp.ut_time) + 4;
	printf("%-*.*s %-*.*s%.12s",NMAX,NMAX,utmp.ut_name,LMAX,LMAX,utmp.ut_line,cbuf);
	if (*utmp.ut_host)
		printf("\t(%.*s)",HMAX,utmp.ut_host);
	putchar('\n');
}
