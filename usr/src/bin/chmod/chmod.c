/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1989, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)chmod.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <err.h>
#include <errno.h>
#include <fts.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register FTS *ftsp;
	register FTSENT *p;
	register int oct, omode;
	mode_t *set;
	int ch, fflag, rflag, hflag, Hflag;
	int fts_options, retval;
	char *ep, *mode;

	fts_options = FTS_PHYSICAL;
	fflag = rflag = hflag = Hflag = 0;
	while ((ch = getopt(argc, argv, "HRfhrwx")) != EOF)
		switch((char)ch) {
		case 'H':
			Hflag = 1;
			fts_options |= FTS_COMFOLLOW;
			break;
		case 'R':
			rflag = 1;
			break;
		case 'f':		/* no longer documented */
			fflag = 1;
			break;
		case 'h':
			hflag = 1;
			fts_options &= ~FTS_PHYSICAL;
			fts_options |= FTS_LOGICAL;
			break;
		/*
		 * "-[rwx]" are valid mode commands.  If they are the entire
		 * argument, getopt has moved past them, so decrement optind.
		 * Regardless, we're done argument processing.
		 */
		case 'r':
			if (!strcmp(argv[optind - 1], "-r"))
				--optind;
			goto done;
		case 'w':
			if (!strcmp(argv[optind - 1], "-w"))
				--optind;
			goto done;
		case 'x':
			if (!strcmp(argv[optind - 1], "-x"))
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
			errx(1, "invalid file mode: %s", mode);
		oct = 1;
	} else {
		if ((set = setmode(mode)) == NULL)
			errx(1, "invalid file mode: %s", mode);
		oct = 0;
	}

	retval = 0;
	if ((ftsp = fts_open(++argv, fts_options, 0)) == NULL)
		err(1, "");
	while (p = fts_read(ftsp))
		switch(p->fts_info) {
		case FTS_D:
			if (!rflag)
				fts_set(ftsp, p, FTS_SKIP);
		case FTS_SL:
		case FTS_SLNONE:
			break;
		case FTS_DNR:
		case FTS_ERR:
		case FTS_NS:
			err(1, "%s", p->fts_path);
		default:	
			if (p->fts_info == FTS_SL && !(hflag || 
			    (Hflag && p->fts_level == FTS_ROOTLEVEL)))
				continue;
			if (chmod(p->fts_accpath, oct ? omode :
			    getmode(set, p->fts_statp->st_mode)) && !fflag) {
				warn(p->fts_path);
				retval = 1;
			}
			break;
		}
	exit(retval);
}

void
usage()
{
	(void)fprintf(stderr, "usage: chmod [-HRh] mode file ...\n");
	exit(1);
}
