/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * John B. Roll Jr.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)xargs.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <limits.h>
#include "pathnames.h"

int fflag, tflag;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	extern char *optarg;
	register int ch;
	register char *p, *bp, *endbp, **bxp, **endxp, **xp;
	int cnt, indouble, insingle, nargs, nline;
	char *start, **xargs;

	nargs = 1024;				/* random value */
	nline = ARG_MAX - 2048;			/* max POSIX.2 value */

	while ((ch = getopt(argc, argv, "fn:s:t")) != EOF)
		switch(ch) {
		case 'f':
			fflag = 1;
			break;
		case 'n':
			if ((nargs = atoi(optarg)) <= 0) {
				(void)fprintf(stderr,
				    "xargs: illegal argument count.\n");
				exit(1);
			}
			break;
		case 's':
			if ((nline = atoi(optarg)) <= 0) {
				(void)fprintf(stderr,
				    "xargs: illegal command length.\n");
				exit(1);
			}
			break;
		case 't':
			tflag = 1;
			break;
		case '?':
		default:
			usage();
	}
	argc -= optind;
	argv += optind;

	/*
	 * Allocate for the utility and arguments passed to xarg, the pointers
	 * to the arguments read from stdin and the trailing NULL.  Allocate
	 * for the arguments read from stdin.
	 */
	if (!(xargs = malloc((u_int)(nargs + argc + 2) * sizeof(char **))) ||
	    !(bp = malloc((u_int)nline + 1))) {
		(void)fprintf(stderr, "xargs: %s.\n", strerror(errno));
		exit(1);
	}

	/*
	 * Use the user's name for the utility as argv[0], just like the
	 * shell.  Echo is the default.  Set up pointers for the user's
	 * arguments.
	 */
	xp = xargs + 1;
	if (!*argv)
		*xp++ = _PATH_ECHO;
	else {
		*xp++ = *argv;
		while (*++argv)
			*xp++ = *argv;
	}

	/* Set up the pointers into the buffer and the arguments */
	*(endxp = (bxp = xp) + nargs) = NULL;
	endbp = (start = p = bp) + nline;

	insingle = indouble = 0;
	for (;;)
		switch(ch = getchar()) {
		case EOF:
			/* Nothing to display. */
			if (p == bp)
				exit(0);

			/* Nothing since last arg end. */
			if (start == p) {
				*xp = NULL;
				run(xargs[0], xargs);
				exit(0);
			}
			goto addarg;
		case ' ':
		case '\t':
			if (insingle || indouble)
				goto addch;
			goto addarg;
		case '\n':
			if (start == p)			/* Empty line. */
				continue;
addarg:			if (insingle || indouble) {
				(void)fprintf(stderr,
				   "xargs: unterminated quote\n");
				exit(1);
			}
			*xp++ = start;
			*p++ = '\0';
			if (xp == endxp || p == endbp || ch == EOF) {
				*xp = NULL;
				run(xargs[0], xargs);
				if (ch == EOF)
					exit(0);
				p = bp;
				xp = bxp;
			}
			start = p;
			break;
		case '\'':
			if (indouble)
				goto addch;
			insingle = !insingle;
			break;
		case '"':
			if (insingle)
				goto addch;
			indouble = !indouble;
			break;
		case '\\':
			if ((ch = getchar()) == EOF) {
				(void)fprintf(stderr,
				    "xargs: backslash at EOF\n");
				exit(1);
			}
			/* FALLTHROUGH */
		default:
addch:			if (p != endbp) {
				*p++ = ch;
				continue;
			}
			if (bxp == xp) {
				(void)fprintf(stderr,
				    "xargs: argument too large.\n");
				exit(1);
			}
			*xp = NULL;
			run(xargs[0], xargs);
			cnt = endbp - start;
			bcopy(start, bp, cnt);
			p = (start = bp) + cnt;
			*p++ = ch;
			xp = bxp;
			break;
		}
	/* NOTREACHED */
}

run(prog, argv)
	char *prog, **argv;
{
	int noinvoke, status;
	pid_t pid;
	char **p;

	if (tflag) {
		(void)fprintf(stderr, "%s", *argv);
		for (p = argv + 1; *p; ++p)
			(void)fprintf(stderr, " %s", *p);
		(void)fprintf(stderr, "\n");
		(void)fflush(stderr);
	}
	noinvoke = 0;
	switch(pid = vfork()) {
	case -1:
		(void)fprintf(stderr,
		    "xargs: vfork: %s.\n", strerror(errno));
		exit(1);
	case 0:
		execvp(prog, argv);
		(void)fprintf(stderr,
		    "xargs: %s: %s.\n", prog, strerror(errno));
		noinvoke = 1;
		_exit(1);
	}
	pid = waitpid(pid, &status, 0);
	if (pid == -1) {
		(void)fprintf(stderr,
		    "xargs: waitpid: %s.\n", strerror(errno));
		exit(1);
	}
	/*
	 * If we couldn't invoke the utility or the utility didn't exit
	 * properly, quit with 127.
	 * Otherwise, if not specified otherwise, and the utility exits
	 * non-zero, exit with that value.
	 */
	if (noinvoke || !WIFEXITED(status) || WIFSIGNALED(status))
		exit(127);
	if (!fflag && WEXITSTATUS(status))
		exit(WEXITSTATUS(status));
}

usage()
{
	(void)fprintf(stderr,
	    "xargs: [-ft] [-n number] [-s size] [utility [argument ...]]\n");
	exit(1);
}
