/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)chflags.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <fts.h>
#include <stdio.h>
#include <string.h>

extern int errno;
int retval;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	register FTS *fts;
	register FTSENT *p;
	register int oct, oflags;
	register char *flags;
	void *set, *setflags();
	struct stat sb;
	int ch, fflag, rflag;

	fflag = rflag = 0;
	while ((ch = getopt(argc, argv, "Rf")) != EOF)
		switch((char)ch) {
		case 'R':
			rflag = 1;
			break;
		case 'f':		/* no longer documented */
			fflag = 1;
			break;
		case '?':
		default:
			usage();
		}
	argv += optind;
	argc -= optind;

	if (argc < 2)
		usage();

	flags = *argv;
	if (*flags >= '0' && *flags <= '7') {
		oflags = (int)strtol(flags, (char **)NULL, 8);
		oct = 1;
	} else {
		if (!(set = setflags(flags))) {
			(void)fprintf(stderr, "chflags: invalid flags.\n");
			exit(1);
		}
		oct = 0;
	}

	retval = 0;
	if (rflag) {
		if (!(fts = fts_open(++argv,
		    oct ? FTS_NOSTAT|FTS_PHYSICAL : FTS_PHYSICAL, 0))) {
			(void)fprintf(stderr, "chflags: %s.\n",
			    strerror(errno));
			exit(1);
		}
		while (p = fts_read(fts))
			switch(p->fts_info) {
			case FTS_D:
				break;
			case FTS_DNR:
			case FTS_ERR:
			case FTS_NS:
				(void)fprintf(stderr, "chflags: %s: %s.\n",
				    p->fts_path, strerror(errno));
				exit(1);
			default:
				if (chflags(p->fts_accpath, oct ? oflags :
				    getflags(set, p->fts_statb.st_flags)) &&
				    !fflag)
					error(p->fts_path);
				break;
			}
		exit(retval);
	}
	if (oct) {
		while (*++argv)
			if (chflags(*argv, oflags) && !fflag)
				error(*argv);
	} else
		while (*++argv)
			if ((lstat(*argv, &sb) ||
			    chflags(*argv, getflags(set, sb.st_flags))) &&
			    !fflag)
				error(*argv);
	exit(retval);
}

error(name)
	char *name;
{
	(void)fprintf(stderr, "chflags: %s: %s.\n", name, strerror(errno));
	retval = 1;
}

usage()
{
	(void)fprintf(stderr, "Usage: chflags [-R] flags file ...\n");
	exit(1);
}

/*
 * These are analogous to the setmode/getmode routines in the C library.
 */
struct cmdset {
	long	clrbits;
	long	setbits;
} cmds;

void *
setflags(cp)
	char *cp;
{
	register struct cmdset *fset;
	register char *arg;

	fset = &cmds;
	fset->clrbits = 0;
	fset->setbits = 0;
	while (cp) {
		while ((arg = strsep(&cp, ",")) != NULL && *arg == '\0')
			/* void */;
		if (!strcasecmp(arg, "dump"))
			fset->clrbits |= NODUMP;
		else if (!strcasecmp(arg, "nodump"))
			fset->setbits |= NODUMP;
		else
			return (NULL);
	}
	return (fset);
}

getflags(fset, oflags)
	register struct cmdset *fset;
	register int oflags;
{

	oflags &= ~fset->clrbits;
	oflags |= fset->setbits;
	return (oflags);
}
