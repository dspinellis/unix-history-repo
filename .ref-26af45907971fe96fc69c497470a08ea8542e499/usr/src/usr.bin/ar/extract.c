/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Hugh Smith at The University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)extract.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <dirent.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "archive.h"
#include "extern.h"

extern CHDR chdr;			/* converted header */
extern char *archive;			/* archive name */

/*
 * extract --
 *	Extract files from the named archive - if member names given only
 *	extract those members otherwise extract all members.  If 'o' option
 *	selected modify date of newly created file to be same as archive
 *	members date otherwise date is time of extraction.  Does not modify
 *	archive.
 */
extract(argv)
	char **argv;
{
	register int afd, all, tfd;
	struct timeval tv[2];
	struct stat sb;
	CF cf;
	int eval;
	char *file;

	eval = 0;
	tv[0].tv_usec = tv[1].tv_usec = 0;

	afd = open_archive(O_RDONLY);

	/* Read from an archive, write to disk; pad on read. */
	SETCF(afd, archive, 0, 0, RPAD);
	for (all = !*argv; get_arobj(afd);) {
		if (all)
			file = chdr.name;
		else if (!(file = files(argv))) {
			skip_arobj(afd);
			continue;
		}

		if (options & AR_U && !stat(file, &sb) &&
		    sb.st_mtime > chdr.date)
			continue;

		if ((tfd = open(file, O_WRONLY|O_CREAT|O_TRUNC, S_IWUSR)) < 0) {
			(void)fprintf(stderr, "ar: %s: %s.\n",
			    file, strerror(errno));
			skip_arobj(afd);
			eval = 1;
			continue;
		}

		if (options & AR_V)
			(void)printf("x - %s\n", file);

		cf.wfd = tfd;
		cf.wname = file;
		copy_ar(&cf, chdr.size);

		if (fchmod(tfd, (short)chdr.mode)) {
			(void)fprintf(stderr, "ar: %s: chmod: %s\n",
			    file, strerror(errno));
			eval = 1;
		}
		if (options & AR_O) {
			tv[0].tv_sec = tv[1].tv_sec = chdr.date;
			if (utimes(file, tv)) {
				(void)fprintf(stderr, "ar: %s: utimes: %s\n",
				    file, strerror(errno));
				eval = 1;
			}
		}
		(void)close(tfd);
		if (!all && !*argv)
			break;
	}
	close_archive(afd);

	if (*argv) {
		orphans(argv);
		return(1);
	}
	return(0);
}	
