/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1992 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)chflags.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <fts.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct cmdset {
	long clrbits;
	long setbits;
} CMDS;
CMDS cmds;
int retval;

void	 err __P((const char *, ...));
void	 error __P((char *));
int	 getflags __P((CMDS *, int));
void	*setflags __P((char *));
void	 usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register FTS *ftsp;
	register FTSENT *p;
	register long oflags;
	register int oct;
	register char *flags;
	struct stat sb;
	void *set;
	int ch, rflag, hflag, Hflag;
	char *ep;
	int fts_options;

	rflag = hflag = Hflag = 0;
	fts_options = FTS_PHYSICAL;
	while ((ch = getopt(argc, argv, "HRh")) != EOF)
		switch((char)ch) {
		case 'H':
			Hflag = 1;
			fts_options |= FTS_COMFOLLOW;
			break;
		case 'R':
			rflag = 1;
			break;
		case 'h':
			hflag = 1;
			fts_options &= ~FTS_PHYSICAL;
			fts_options |= FTS_LOGICAL;
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
		oflags = (int)strtol(flags, &ep, 8);
                if (oflags < 0 || *ep)
                        err("invalid flags: %s", flags);
                oct = 1;
	} else {
		if ((set = setflags(flags)) == NULL)
                        err("invalid flags: %s", flags);
		oct = 0;
	}

	retval = 0;
	if (oct)
		fts_options |= FTS_NOSTAT;
	if (!(ftsp = fts_open(++argv, fts_options , 0)))
		err("%s", strerror(errno));
	while (p = fts_read(ftsp))
		switch(p->fts_info) {
		case FTS_D:
			if (!rflag)
				fts_set(ftsp, p, FTS_SKIP);
			break;
		case FTS_DNR:
		case FTS_ERR:
		case FTS_NS:
			err("%s: %s", p->fts_path, strerror(errno));
		default:
                        if (p->fts_info == FTS_SL &&
                            !(hflag ||
                            (Hflag && p->fts_level == FTS_ROOTLEVEL)))
				continue;
			if (chflags(p->fts_accpath, oct ? oflags :
			    getflags(set, p->fts_statp->st_flags)))
				error(p->fts_path);
			break;
		}
	exit(retval);
}

/*
 * These are analogous to the setmode/getmode routines in the C library.
 */
void *
setflags(cp)
	char *cp;
{
	register CMDS *fset;
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

int
getflags(fset, oflags)
	register CMDS *fset;
	register int oflags;
{

	oflags &= ~fset->clrbits;
	oflags |= fset->setbits;
	return (oflags);
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
	(void)fprintf(stderr, "chflags: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exit(1);
	/* NOTREACHED */
}

void
error(name)
	char *name;
{
	(void)fprintf(stderr, "chflags: %s: %s.\n", name, strerror(errno));
	retval = 1;
}

void
usage()
{
	(void)fprintf(stderr, "usage: chflags [-HRh] flags file ...\n");
	exit(1);
}
