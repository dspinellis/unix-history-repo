/*
 * Copyright (c) 1983, 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1983, 1992, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkdir.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int	build __P((char *));
void	usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int ch, exitval, pflag;

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

	for (exitval = 0; *argv; ++argv)
		if (pflag)
			exitval |= build(*argv);
		else if (mkdir(*argv, S_IRWXU | S_IRWXG | S_IRWXO) < 0) {
			warn("%s", *argv);
			exitval = 1;
		}
	exit(exitval);
}

int
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
					warn("%s", path);
					return (1);
				}
				create = 1;
			}
			if (!(*p = savech))
				break;
		}
	if (!create) {
		warnx("%s: %s", path, strerror(EEXIST));
		return (1);
	}
	return (0);
}

void
usage()
{
	(void)fprintf(stderr, "usage: mkdir [-p] directory ...\n");
	exit (1);
}
