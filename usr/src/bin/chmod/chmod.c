/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)chmod.c	5.13 (Berkeley) %G%";
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
		if (!(fts = ftsopen(++argv,
		    (oct ? FTS_NOSTAT : 0)|FTS_MULTIPLE|FTS_PHYSICAL, 0))) {
			(void)fprintf(stderr, "chmod: %s.\n", strerror(errno));
			exit(1);
		}
		while (p = ftsread(fts)) {
			if (p->fts_info == FTS_D)
				continue;
			if (p->fts_info == FTS_ERR) {
				if (!fflag)
					error(p->fts_path);
				continue;
			}
			if (chmod(p->fts_accpath, oct ?
			    omode : getmode(set, p->fts_statb.st_mode)) &&
			    !fflag)
				error(p->fts_path);
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
