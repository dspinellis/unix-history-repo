/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef	lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif	/* !lint */

#ifndef	lint
static char sccsid[] = "@(#)ln.c	4.9 (Berkeley) %G%";
#endif	/* !lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <stdio.h>
#include <errno.h>

static int	fflag,				/* undocumented force flag */
		sflag,				/* symbolic, not hard, link */
		(*linkf)();			/* system link call */

main(argc, argv)
	int	argc;
	char	**argv;
{
	extern int	optind;
	struct stat	buf;
	int	ch, exitval, link(), symlink();
	char	*sourcedir;

	while ((ch = getopt(argc, argv, "fs")) != EOF)
		switch((char)ch) {
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
	default:			/* ln target1 target2 directory */
		sourcedir = argv[argc - 1];
		if (stat(sourcedir, &buf)) {
			perror(sourcedir);
			exit(1);
		}
		if ((buf.st_mode & S_IFMT) != S_IFDIR)
			usage();
		for (exitval = 0; *argv != sourcedir; ++argv)
			exitval |= linkit(*argv, sourcedir, 1);
		exit(exitval);
	}
	/*NOTREACHED*/
}

static
linkit(target, source, isdir)
	char	*target, *source;
	int	isdir;
{
	extern int	errno;
	struct stat	buf;
	char	path[MAXPATHLEN],
		*cp, *rindex(), *strcpy();

	if (!sflag) {
		/* if target doesn't exist, quit now */
		if (stat(target, &buf)) {
			perror(target);
			return(1);
		}
		/* only symbolic links to directories, unless -f option used */
		if (!fflag && (buf.st_mode & S_IFMT) == S_IFDIR) {
			printf("%s is a directory.\n", target);
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
		perror(source);
		return(1);
	}
	return(0);
}

static
usage()
{
	fputs("usage:\tln [-s] targetname [sourcename]\n\tln [-s] targetname1 targetname2 [... targetnameN] sourcedirectory\n", stderr);
	exit(1);
}
