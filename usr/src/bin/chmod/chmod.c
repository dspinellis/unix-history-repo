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
static char sccsid[] = "@(#)chmod.c	5.17 (Berkeley) %G%";
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
	register int oct, omode;
	register char *mode;
	mode_t *set, *setmode();
	struct stat sb;
	int ch, fflag, rflag;

	fflag = rflag = 0;
	while ((ch = getopt(argc, argv, "Rfrwx")) != EOF)
		switch((char)ch) {
		case 'R':
			rflag++;
			break;
		case 'f':
			fflag++;
			break;
		/* "-[rwx]" are valid file modes */
		case 'r':
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
		omode = (int)strtol(mode, (char **)NULL, 8);
		oct = 1;
	} else {
		if (!(set = setmode(mode))) {
			(void)fprintf(stderr, "chmod: invalid file mode.\n");
			exit(1);
		}
		oct = 0;
	}

	retval = 0;
	if (rflag) {
		if (!(fts = fts_open(++argv,
		    oct ? FTS_NOSTAT|FTS_PHYSICAL : FTS_PHYSICAL, 0))) {
			(void)fprintf(stderr, "chmod: %s.\n", strerror(errno));
			exit(1);
		}
		while (p = fts_read(fts))
			switch(p->fts_info) {
			case FTS_DNR:
				(void)fprintf(stderr,
				    "chmod: %s: unable to read.\n",
				    p->fts_path);
				break;
			case FTS_DNX:
				(void)fprintf(stderr,
				    "chmod: %s: unable to search.\n",
				    p->fts_path);
				break;
			case FTS_D:
			case FTS_DC:
				break;
			case FTS_ERR:
				(void)fprintf(stderr, "chmod: %s: %s.\n",
				    p->fts_path, strerror(errno));
				exit(1);
			case FTS_NS:
				(void)fprintf(stderr,
				    "chmod: %s: unable to stat.\n",
				    p->fts_path);
				break;
			default:
				if (chmod(p->fts_accpath, oct ? omode :
				    getmode(set, p->fts_statb.st_mode)) &&
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

error(name)
	char *name;
{
	(void)fprintf(stderr, "chmod: %s: %s.\n", name, strerror(errno));
	retval = 1;
}

usage()
{
	(void)fprintf(stderr, "chmod: chmod [-fR] mode file ...\n");
	exit(1);
}
