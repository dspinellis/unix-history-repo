/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)kill.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <signal.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

static char *signals[] = {
	"hup", "int", "quit", "ill", "trap", "iot",		/*  1 - 6  */
	"emt", "fpe", "kill", "bus", "segv", "sys",		/*  7 - 12 */
	"pipe", "alrm",  "term", "urg", "stop", "tstp",		/* 13 - 18 */
	"cont", "chld", "ttin", "ttou", "io", "xcpu",		/* 19 - 24 */
	"xfsz", "vtalrm", "prof", "winch", "29", "usr1",	/* 25 - 30 */
	"usr2", NULL,						/* 31 - 32 */
};

main(argc, argv)
	int argc;
	char **argv;
{
	register int errors, numsig, pid;
	register char **p;
	char *ep;

	if (argc < 2)
		usage();

	if (!strcmp(*++argv, "-l")) {
		printsig(stdout);
		exit(0);
	}

	numsig = SIGTERM;
	if (**argv == '-') {
		++*argv;
		if (isalpha(**argv)) {
			if (!strncasecmp(*argv, "sig", 3))
				*argv += 3;
			for (p = signals;; ++p) {
				if (!*p)
					nosig(*argv);
				if (!strcasecmp(*p, *argv)) {
					numsig = p - signals + 1;
					break;
				}
			}
		} else if (isdigit(**argv)) {
			numsig = strtol(*argv, &ep, 10);
			if (!*argv || *ep) {
				(void)fprintf(stderr,
				    "kill: illegal signal number %s\n", *argv);
				exit(1);
			}
			if (numsig <= 0 || numsig > NSIG)
				nosig(*argv);
		} else
			nosig(*argv);
		++argv;
	}

	if (!*argv)
		usage();

	for (errors = 0; *argv; ++argv) {
		pid = strtol(*argv, &ep, 10);
		if (!*argv || *ep) {
			(void)fprintf(stderr,
			    "kill: illegal process id %s\n", *argv);
			continue;
		}
		if (kill(atoi(*argv), numsig) == -1) {
			(void)fprintf(stderr,
			    "kill: %s: %s\n", *argv, strerror(errno));
			errors = 1;
		}
	}
	exit(errors);
}

nosig(name)
	char *name;
{
	(void)fprintf(stderr,
	    "kill: unknown signal %s; valid signals:\n", name);
	printsig(stderr);
	exit(1);
}

printsig(fp)
	FILE *fp;
{
	register char **p;

	for (p = signals; *p; ++p) {
		(void)fprintf(fp, "%s ", *p);
		if ((p - signals) == NSIG / 2 - 1)
			(void)fprintf(fp, "\n");
	}
	(void)fprintf(fp, "\n");
}

usage()
{
	(void)fprintf(stderr, "usage: kill [-l] [-sig] pid ...\n");
	exit(1);
}
