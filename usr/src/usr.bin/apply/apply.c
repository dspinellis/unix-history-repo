/*-
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)apply.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void	usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int ch, clen, debug, error, n, nargs;
	char *c, *cmd, magic, *p;

	debug = 0;
	magic = '%';		/* default magic char is `%' */
	nargs = 1;		/* default to a single arg */

	while ((ch = getopt(argc, argv, "a:d0123456789")) != EOF) {
		switch (ch) {
			case 'a':
				magic = *optarg;
				break;

			case 'd':
				debug = 1;
				break;

			case '0': case '1': case '2':
			case '3': case '4': case '5':
			case '6': case '7': case '8':
			case '9':
				nargs = optopt - '0';
				break;

			default:
				usage();
				break;
		}
	}

	argc -= optind;
	argv += optind;

	if (argc < 2)
		usage();

	/*
	 * The command to run is argv[0], and the args are argv[1..]
	 */
	cmd = argv[0];

	/*
	 * Look for %digit references, remembering the
	 * largest value found.
	 */
	n = 0;
	for (p = cmd; (p = strchr(p, magic)) != NULL; p++) {
		p++;
		switch (*p) {
		case '1': case '2': case '3':
		case '4': case '5': case '6':
		case '7': case '8': case '9':
			if ((*p - '0') > n)
				n = *p - '0';
			break;
		}
	}

	/*
	 * If there were any %digit references, then
	 * simply use those, otherwise build a new command
	 * string with sufficient %digit references at
	 * the end to consume (nargs) arguments each time
	 * round the loop.
	 */
	c = malloc(sizeof("exec") + strlen(cmd) + 3 * nargs + 1);
	if (c == NULL)
		err(1, NULL);
		
	if (n > 0) {
		(void)sprintf(c, "exec %s", cmd);
		nargs = n;
	} else {
		int i;

		p = c;
		p += sprintf(c, "exec %s", cmd);
		for (i = 1; i <= nargs; i++)
			p += sprintf(p, " %c%d", magic, i);
	}
	cmd = c;

	/*
	 * (argc) and (argv) are still offset by one to make it
	 * simpler to expand %digit references.
	 * At the end of the loop check for (argc) equals 1 means
	 * that all the (argv) has been consumed.
	 */
	c = 0;
	clen = 0;
	error = 0;

	while (argc > nargs) {
		int i, l;
		char *c, *q;

		/* find tentative value for command length */
		l = strlen(cmd);
		for (i = 0; i < nargs; i++)
			l += strlen(argv[i]);

		/* ensure enough space to build the command */
		if (l > clen) {
			clen = l;
			c = realloc(c, clen);
			if (c == NULL)
				err(1, NULL);
		}

		/* expand all command references */
		for (p = cmd, q = c; *p; p++) {
			if (*p != magic) {
				*q++ = *p;
				continue;
			}
			p++;

			switch (*p) {
			case '1': case '2': case '3':
			case '4': case '5': case '6':
			case '7': case '8': case '9':
				strcpy(q, argv[*p - '0']);
				q += strlen(q);
				break;

			default:
				*q++ = magic;
				*q++ = *p;
				break;
			}
		}

		/* terminate the command string */
		*q = '\0';

		/* run the command */
		if (debug) {
			puts(c);
		} else {
			if (system(c))
				error = 1;
		}

		/*
		 * bump the arg vector.  if -0 was on the command
		 * line, then still need to skip one argument.
		 */
		if (nargs) {
			argc -= nargs;
			argv += nargs;
		} else {
			--argc;
			argv++;
		}
	}

	if (argc != 1)
		errx(1, "expecting argument%s after \"%s\"",
			(nargs-argc) ? "s" : "", argv[argc-1]);
	
	exit(1);
}

void
usage()
{

	(void)fprintf(stderr, "usage: apply [-an] [-0..9] cmd args ...\n");
	exit(1);
}
