/*
 * Copyright (c) 1987, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1987, 1993, 1994\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ln.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int	dirflag;			/* Undocumented directory flag. */
int	fflag;				/* Unlink existing files. */
int	sflag;				/* Symbolic, not hard, link. */
					/* System link call. */
int (*linkf) __P((const char *, const char *));

int	linkit __P((char *, char *, int));
void	usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern int optind;
	struct stat sb;
	int ch, exitval;
	char *sourcedir;

	while ((ch = getopt(argc, argv, "Ffs")) != EOF)
		switch (ch) {
		case 'F':
			dirflag = 1;	/* XXX: deliberately undocumented. */
			break;
		case 'f':
			fflag = 1;
			break;
		case 's':
			sflag = 1;
			break;
		case '?':
		default:
			usage();
		}

	argv += optind;
	argc -= optind;

	linkf = sflag ? symlink : link;

	switch(argc) {
	case 0:
		usage();
	case 1:				/* ln target */
		exit(linkit(argv[0], ".", 1));
	case 2:				/* ln target source */
		exit(linkit(argv[0], argv[1], 0));
	}
					/* ln target1 target2 directory */
	sourcedir = argv[argc - 1];
	if (stat(sourcedir, &sb))
		err(1, "%s", sourcedir);
	if (!S_ISDIR(sb.st_mode))
		usage();
	for (exitval = 0; *argv != sourcedir; ++argv)
		exitval |= linkit(*argv, sourcedir, 1);
	exit(exitval);
}

int
linkit(target, source, isdir)
	char *target, *source;
	int isdir;
{
	struct stat sb;
	int exists;
	char *p, path[MAXPATHLEN];

	if (!sflag) {
		/* If target doesn't exist, quit now. */
		if (stat(target, &sb)) {
			warn("%s", target);
			return (1);
		}
		/* Only symbolic links to directories, unless -F option used. */
		if (!dirflag && (sb.st_mode & S_IFMT) == S_IFDIR) {
			warnx("%s: is a directory", target);
			return (1);
		}
	}

	/* If the source is a directory, append the target's name. */
	if (isdir || (exists = !stat(source, &sb)) && S_ISDIR(sb.st_mode)) {
		if ((p = strrchr(target, '/')) == NULL)
			p = target;
		else
			++p;
		(void)snprintf(path, sizeof(path), "%s/%s", source, p);
		source = path;
		exists = !stat(source, &sb);
	} else
		exists = !stat(source, &sb);

	/*
	 * If the file exists, and -f was specified, unlink it.
	 * Attempt the link.
	 */
	if (fflag && exists && unlink(source) || (*linkf)(target, source)) {
		warn("%s", source);
		return (1);
	}
	return (0);
}

void
usage()
{
	(void)fprintf(stderr,
	    "usage:\tln [-fs] file1 file2\n\tln [-fs] file ... directory\n");
	exit(1);
}
