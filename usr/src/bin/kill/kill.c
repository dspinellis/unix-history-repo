/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1988, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)kill.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <ctype.h>
#include <err.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void nosig __P((char *));
void printsig __P((FILE *));
void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register int errors, numsig, pid;
	register const char *const *p;
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
			for (numsig = NSIG, p = sys_signame + 1; --numsig; ++p)
				if (!strcasecmp(*p, *argv)) {
					numsig = p - sys_signame;
					break;
				}
			if (!numsig)
				nosig(*argv);
		} else if (isdigit(**argv)) {
			numsig = strtol(*argv, &ep, 10);
			if (!*argv || *ep)
				errx(1, "illegal signal number: %s", *argv);
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
			warnx("illegal process id: %s", *argv);
			errors = 1;
		} else if (kill(pid, numsig) == -1) {
			warn("%s", *argv);
			errors = 1;
		}
	}
	exit(errors);
}

void
nosig(name)
	char *name;
{
	(void)fprintf(stderr,
	    "kill: unknown signal %s; valid signals:\n", name);
	printsig(stderr);
	exit(1);
}

void
printsig(fp)
	FILE *fp;
{
	register int cnt;
	register const char *const *p;

	for (cnt = NSIG, p = sys_signame + 1; --cnt; ++p) {
		(void)fprintf(fp, "%s ", *p);
		if (cnt == NSIG / 2)
			(void)fprintf(fp, "\n");
	}
	(void)fprintf(fp, "\n");
}

void
usage()
{
	(void)fprintf(stderr, "usage: kill [-l] [-sig] pid ...\n");
	exit(1);
}
