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
static char sccsid[] = "@(#)chflags.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <err.h>
#include <fts.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

u_long	string_to_flags __P((char **, u_long *, u_long *));
void	usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register FTS *ftsp;
	register FTSENT *p;
	register int oct;
	u_long clear, set;
	int ch, fts_options, retval, rflag, hflag, Hflag;
	char *flags, *ep;

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
		set = (int)strtol(flags, &ep, 8);
                if (set < 0 || *ep)
                        errx(1, "invalid flags: %s", flags);
		fts_options |= FTS_NOSTAT;
                oct = 1;
	} else {
		if (string_to_flags(&flags, &set, &clear))
                        errx(1, "invalid flag: %s", flags);
		clear = ~clear;
		oct = 0;
	}

	if ((ftsp = fts_open(++argv, fts_options , 0)) == NULL)
		err(1, "");

	for (retval = 0; p = fts_read(ftsp);)
		switch(p->fts_info) {
		case FTS_D:
			if (!rflag)
				fts_set(ftsp, p, FTS_SKIP);
			break;
		case FTS_DNR:
		case FTS_ERR:
		case FTS_NS:
			err(1, "%s", p->fts_path);
		default:
                        if (p->fts_info == FTS_SL &&
                            !(hflag ||
                            (Hflag && p->fts_level == FTS_ROOTLEVEL)))
				continue;
			if (oct) {
				if (!chflags(p->fts_accpath, set))
					break;
			} else {
				p->fts_statp->st_flags |= set;
				p->fts_statp->st_flags &= clear;
				if (!chflags(p->fts_accpath,
				    p->fts_statp->st_flags))
					break;
			}
			warn("%s", p->fts_path);
			retval = 1;
			break;
		}
	exit(retval);
}

void
usage()
{
	(void)fprintf(stderr, "usage: chflags [-HRh] flags file ...\n");
	exit(1);
}
