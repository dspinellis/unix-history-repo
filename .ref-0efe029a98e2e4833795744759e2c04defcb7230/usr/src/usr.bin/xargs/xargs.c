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
static char sccsid[] = "@(#)xargs.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include "pathnames.h"

#define	DEF_ARGC	255

int tflag;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno, optind;
	extern char *optarg;
	register int ch;
	register char *p, *bp, *endbp, **bxp, **endxp, **xp;
	int cnt, indouble, insingle, nargs, nline;
	char *mark, *prog, **xargs, *malloc();

	nargs = DEF_ARGC;
	nline = _POSIX2_LINE_MAX;

	while ((ch = getopt(argc, argv, "n:s:t")) != EOF)
		switch(ch) {
		case 'n':
			if ((nargs = atoi(optarg)) <= 0) {
				(void)fprintf(stderr,
				    "xargs: bad argument count.\n");
				exit(1);
			}
			break;
		case 's':
			if ((nline = atoi(optarg)) <= 0) {
				(void)fprintf(stderr,
				    "xargs: bad command length.\n");
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

	/* room for the command, leftover arguments and trailing NULL */
	if (!(xargs =
	    (char **)malloc((u_int)(nargs + argc + 2) * sizeof(char **))))
		enomem();

	if (!(bp = malloc((u_int)nline + 1)))
		enomem();

	xp = xargs + 1;
	if (!*argv)
		prog = _PATH_ECHO;
	else {
		prog = *argv;
		while (*++argv)
			*xp++ = *argv;
	}

	if (xargs[0] = rindex(prog, '/'))
		++xargs[0];
	else
		xargs[0] = prog;

	/* set up the pointers into the buffer and the arguments */
	*(endxp = (bxp = xp) + nargs) = NULL;
	endbp = (mark = p = bp) + nline;

	insingle = indouble = 0;
	for (;;)
		switch(ch = getchar()) {
		case EOF:
			if (p == bp)		/* nothing to display */
				exit(0);
			if (mark == p) {	/* nothing since last arg end */
				run(prog, xargs);
				exit(0);
			}
			goto addarg;
		case ' ':
		case '\t':
			if (insingle || indouble)
				goto addch;
			goto addarg;
		case '\n':
			if (mark == p)			/* empty line */
				continue;
addarg:			*xp++ = mark;
			*p++ = '\0';
			if (xp == endxp || p >= endbp || ch == EOF) {
				if (insingle || indouble) {
					(void)fprintf(stderr,
					   "xargs: unterminated quote.\n");
					exit(1);
				}
				run(prog, xargs);
				if (ch == EOF)
					exit(0);
				p = bp;
				xp = bxp;
			}
			mark = p;
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
			if ((ch = getchar()) == EOF)
				ch = '\\';
			if (ch == '\n') {
				(void)fprintf(stderr,
				    "xargs: newline may not be escaped.\n");
				exit(1);
			}
			/* FALLTHROUGH */
		default:
addch:			if (p != endbp) {
				*p++ = ch;
				continue;
			}
			if (xp == bxp) {
				(void)fprintf(stderr,
				    "xargs: argument too large.\n");
				exit(1);
			}
			*xp = NULL;
			run(prog, xargs);
			cnt = endbp - mark;
			bcopy(mark, bp, cnt);
			p = (mark = bp) + cnt;
			*p++ = ch;
			xp = bxp;
			break;
		}
	/* NOTREACHED */
}

run(prog, argv)
	char *prog, **argv;
{
	union wait pstat;
	pid_t pid, waitpid();
	char **p;

	if (tflag) {
		(void)fprintf(stderr, "%s", *argv);
		for (p = argv + 1; *p; ++p)
			(void)fprintf(stderr, " %s", *p);
		(void)fprintf(stderr, "\n");
		(void)fflush(stderr);
	}
	switch(pid = vfork()) {
	case -1:
		(void)fprintf(stderr,
		   "xargs: vfork: %s.\n", strerror(errno));
		exit(1);
	case 0:
		execvp(prog, argv);
		(void)fprintf(stderr,
		   "xargs: %s: %s.\n", prog, strerror(errno));
		_exit(1);
	}
	pid = waitpid(pid, &pstat, 0);
	if (pid == -1) {
		(void)fprintf(stderr,
		   "xargs: waitpid: %s.\n", strerror(errno));
		exit(1);
	}
	if (pstat.w_status)
		exit(1);
}

enomem()
{
	(void)fprintf(stderr, "xargs: %s.\n", strerror(ENOMEM));
	exit(1);
}

usage()
{
	(void)fprintf(stderr,
	    "xargs: [-t] [-n number] [-s size] [utility [argument ...]]\n");
	exit(1);
}
