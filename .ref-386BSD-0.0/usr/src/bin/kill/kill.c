/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)kill.c	5.3 (Berkeley) 7/1/91";
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
	"xfsz", "vtalrm", "prof", "winch", "info", "usr1",	/* 25 - 30 */
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
		if (kill(pid, numsig) == -1) {
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
