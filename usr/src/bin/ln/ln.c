/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ln.c	4.14 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

static int	dirflag,			/* undocumented force flag */
		sflag,				/* symbolic, not hard, link */
		(*linkf)();			/* system link call */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	struct stat buf;
	int ch, exitval, link(), symlink();
	char *sourcedir;

	while ((ch = getopt(argc, argv, "Fs")) != EOF)
		switch((char)ch) {
		case 'F':
			dirflag = 1;
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
	default:			/* ln target1 target2 directory */
		sourcedir = argv[argc - 1];
		if (stat(sourcedir, &buf)) {
			(void)fprintf(stderr,
			    "ln: %s: %s\n", sourcedir, strerror(errno));
			exit(1);
		}
		if (!S_ISDIR(buf.st_mode))
			usage();
		for (exitval = 0; *argv != sourcedir; ++argv)
			exitval |= linkit(*argv, sourcedir, 1);
		exit(exitval);
	}
	/* NOTREACHED */
}

static
linkit(target, source, isdir)
	char *target, *source;
	int isdir;
{
	struct stat buf;
	char path[MAXPATHLEN], *cp;

	if (!sflag) {
		/* if target doesn't exist, quit now */
		if (stat(target, &buf)) {
			(void)fprintf(stderr,
			    "ln: %s: %s\n", target, strerror(errno));
			return(1);
		}
		/* only symbolic links to directories, unless -F option used */
		if (!dirflag && (buf.st_mode & S_IFMT) == S_IFDIR) {
			(void)printf("ln: %s is a directory.\n", target);
			return(1);
		}
	}

	/* if the source is a directory, append the target's name */
	if (isdir || !stat(source, &buf) && (buf.st_mode & S_IFMT) == S_IFDIR) {
		if (!(cp = rindex(target, '/')))
			cp = target;
		else
			++cp;
		(void)sprintf(path, "%s/%s", source, cp);
		source = path;
	}

	if ((*linkf)(target, source)) {
		(void)fprintf(stderr, "ln: %s: %s\n", source, strerror(errno));
		return(1);
	}
	return(0);
}

static
usage()
{
	(void)fprintf(stderr,
	    "usage:\tln [-s] file1 file2\n\tln [-s] file ... directory\n");
	exit(1);
}
