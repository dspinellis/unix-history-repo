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
static char sccsid[] = "@(#)mkdir.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int	build __P((char *));
void	usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int ch, exitval, oct, omode, pflag;
	mode_t *set;
	char *ep, *mode;

	pflag = 0;
	mode = NULL;
	while ((ch = getopt(argc, argv, "m:p")) != EOF)
		switch(ch) {
		case 'p':
			pflag = 1;
			break;
		case 'm':
			mode = optarg;
			break;
		case '?':
		default:
			usage();
		}

	argc -= optind;
	argv += optind;
	if (argv[0] == NULL)
		usage();

	if (mode == NULL) {
		omode = S_IRWXU | S_IRWXG | S_IRWXO;
		oct = 1;
	} else if (*mode >= '0' && *mode <= '7') {
		omode = (int)strtol(mode, &ep, 8);
		if (omode < 0 || *ep)
			errx(1, "invalid file mode: %s", mode);
		oct = 1;
	} else {
		if ((set = setmode(mode)) == NULL)
			errx(1, "invalid file mode: %s", mode);
		oct = 0;
	}

	for (exitval = 0; *argv != NULL; ++argv) {
		if (pflag && build(*argv)) {
			exitval = 1;
			continue;
		}
		if (mkdir(*argv, oct ?
		    omode : getmode(set, S_IRWXU | S_IRWXG | S_IRWXO)) < 0) {
			warn("%s", *argv);
			exitval = 1;
		}
	}
	exit(exitval);
}

int
build(path)
	char *path;
{
	struct stat sb;
	mode_t numask, oumask;
	int first;
	char *p;

	p = path;
	if (p[0] == '/')		/* Skip leading '/'. */
		++p;
	for (first = 1;; ++p) {
		if (p[0] == '\0' || p[0] == '/' && p[1] == '\0')
			break;
		if (p[0] != '/')
			continue;
		*p = '\0';
		if (first) {
			/*
			 * POSIX 1003.2:
			 * For each dir operand that does not name an existing
			 * directory, effects equivalent to those cased by the
			 * following command shall occcur:
			 *
			 * mkdir -p -m $(umask -S),u+wx $(dirname dir) &&
			 *    mkdir [-m mode] dir
			 *
			 * We change the user's umask and then restore it,
			 * instead of doing chmod's.
			 */
			oumask = umask(0);
			numask = oumask & ~(S_IWUSR | S_IXUSR);
			(void)umask(numask);
			first = 0;
		}
		if (stat(path, &sb)) {
			if (errno != ENOENT ||
			    mkdir(path, S_IRWXU | S_IRWXG | S_IRWXO) < 0) {
				warn("%s", path);
				return (1);
			}
		}
		*p = '/';
	}
	if (!first)
		(void)umask(oumask);
	return (0);
}

void
usage()
{
	(void)fprintf(stderr, "usage: mkdir [-p] [-m mode] directory ...\n");
	exit (1);
}
