/*
 * Copyright (c) 1983, 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1992 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkdir.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int exitval;

void build __P((char *));
void usage __P((void));
void err __P((const char *, ...));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int ch, pflag;

	pflag = 0;
	while ((ch = getopt(argc, argv, "p")) != EOF)
		switch(ch) {
		case 'p':
			pflag = 1;
			break;
		case '?':
		default:
			usage();
		}

	if (!*(argv += optind))
		usage();

	for (; *argv; ++argv)
		if (pflag)
			build(*argv);
		else if (mkdir(*argv, S_IRWXU | S_IRWXG | S_IRWXO) < 0)
			err("%s: %s", *argv, strerror(errno));
	exit(exitval);
}

void
build(path)
	char *path;
{
	register char *p;
	struct stat sb;
	int create, savech;

	p = path;
	if (*p)				/* Skip leading '/'. */
		++p;
	for (create = 0;; ++p)
		if (!*p || *p == '/') {
			savech = *p;
			*p = '\0';
			if (stat(path, &sb)) {
				if (errno != ENOENT || mkdir(path,
				    S_IRWXU | S_IRWXG | S_IRWXO) < 0) {
					err("%s: %s", path, strerror(errno));
					return;
				}
				create = 1;
			}
			if (!(*p = savech))
				break;
		}
	if (!create)
		err("%s: %s", path, strerror(EEXIST));
}

void
usage()
{
	(void)fprintf(stderr, "usage: mkdir [-p] directory ...\n");
	exit(1);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(const char *fmt, ...)
#else
err(fmt, va_alist)
	char *fmt;
	va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "mkdir: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exitval = 1;
}
