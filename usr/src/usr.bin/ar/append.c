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
static char sccsid[] = "@(#)append.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <stdio.h>
#include <string.h>
#include "archive.h"
#include "extern.h"

extern char *archive;			/* archive name */

/*
 * append --
 *	Append files to the archive - modifies original archive or creates
 *	a new archive if named archive does not exist.
 */
append(argv)
	char **argv;
{
	register int fd, afd;
	register char *file;
	struct stat sb;
	CF cf;
	int eval;

	afd = open_archive(O_CREAT|O_RDWR);
	if (lseek(afd, (off_t)0, SEEK_END) == (off_t)-1)
		error(archive);

	/* Read from disk, write to an archive; pad on write. */
	SETCF(0, 0, afd, archive, WPAD);
	for (eval = 0; file = *argv++;) {
		if ((fd = open(file, O_RDONLY)) < 0) {
			(void)fprintf(stderr,
			    "ar: %s: %s.\n", file, strerror(errno));
			eval = 1;
			continue;
		}
		if (options & AR_V)
			(void)printf("q - %s\n", file);
		cf.rfd = fd;
		cf.rname = file;
		put_arobj(&cf, &sb);
		(void)close(fd);
	}
	close_archive(afd);
	return(eval);	
}
