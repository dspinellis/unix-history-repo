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
static char sccsid[] = "@(#)chmod.c	5.21 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <fts.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int retval;

void err __P((const char *, ...));
void error __P((char *));
void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register FTS *fts;
	register FTSENT *p;
	register int oct, omode;
	struct stat sb;
	mode_t *set;
	int ch, fflag, rflag;
	char *ep, *mode;

	fflag = rflag = 0;
	while ((ch = getopt(argc, argv, "Rfrwx")) != EOF)
		switch((char)ch) {
		case 'R':
			rflag = 1;
			break;
		case 'f':		/* no longer documented */
			fflag = 1;
			break;
		case 'r':		/* "-[rwx]" are valid file modes */
		case 'w':
		case 'x':
			--optind;
			goto done;
		case '?':
		default:
			usage();
		}
done:	argv += optind;
	argc -= optind;

	if (argc < 2)
		usage();

	mode = *argv;
	if (*mode >= '0' && *mode <= '7') {
		omode = (int)strtol(mode, &ep, 8);
		if (omode < 0 || *ep)
			err("invalid file mode: %s", mode);
		oct = 1;
	} else {
		if (!(set = setmode(mode)))
			err("invalid file mode: %s", mode);
		oct = 0;
	}

	retval = 0;
	if (rflag) {
		if ((fts = fts_open(++argv,
		    oct ? FTS_NOSTAT|FTS_PHYSICAL : FTS_PHYSICAL, 0)) == NULL)
			err("%s", strerror(errno));
		while (p = fts_read(fts))
			switch(p->fts_info) {
			case FTS_D:
				break;
			case FTS_DNR:
			case FTS_ERR:
			case FTS_NS:
				err("%s: %s", p->fts_path, strerror(errno));
			default:
				if (chmod(p->fts_accpath, oct ? omode :
				    getmode(set, p->fts_statp->st_mode)) &&
				    !fflag)
					error(p->fts_path);
				break;
			}
		exit(retval);
	}
	if (oct) {
		while (*++argv)
			if (chmod(*argv, omode) && !fflag)
				error(*argv);
	} else
		while (*++argv)
			if ((lstat(*argv, &sb) ||
			    chmod(*argv, getmode(set, sb.st_mode))) && !fflag)
				error(*argv);
	exit(retval);
}

void
error(name)
	char *name;
{
	(void)fprintf(stderr, "chmod: %s: %s\n", name, strerror(errno));
	retval = 1;
}

void
usage()
{
	(void)fprintf(stderr, "usage: chmod [-R] mode file ...\n");
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
	(void)fprintf(stderr, "chmod: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exit(1);
	/* NOTREACHED */
}
