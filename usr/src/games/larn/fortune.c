/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)fortune.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/* fortune.c		 Larn is copyrighted 1986 by Noah Morgan. */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include "header.h"
#include "pathnames.h"

/*
 * function to return a random fortune from the fortune file
 */

char *
fortune()
{
	static int fd = -1;	/* true if we have load the fortune info */
	static int nlines;	/* # lines in fortune database */
	register int tmp;
	register char *p;
	char *base, **flines;
	struct stat sb;

	if (fd != -1)
		return (flines[random() % nlines]);

	if ((fd = open(_PATH_FORTS, O_RDONLY)) < 0)
		return (NULL);

	/* Find out how big fortune file is and get memory for it. */
	if ((fstat(fd, &sb) < 0) ||
	    ((base = malloc(1 + sb.st_size)) == NULL)) {
		(void)close(fd);
		goto bad;
	}

	/* Read in the entire fortune file. */
	if (read(fd, base, sb.st_size) != sb.st_size) {
		free(base);
		goto bad;
	}
	base[sb.st_size] = '\0';	/* Final NULL termination. */

	(void)close(fd);


	/*
	 * Count up all the lines (and NULL terminate) to know memory
	 * needs.
	 */
	for (p = base; p < base + sb.st_size; p++)
		if (*p == '\n') {
			*p = '\0';
			++nlines;
		}

	if (nlines <= 0) {
		free(base);
		goto bad;
	}

	/* Get memory for array of pointers to each fortune. */
	if ((flines = malloc(nlines * sizeof(char *))) == NULL) {
		free(base);
		goto bad;
	}

	/* Now assign each pointer to a line. */
	for (p = base, tmp = 0; tmp < nlines; ++tmp) {
		flines[tmp] = p;
		while (*p++);
	}

	return (flines[random() % nlines]);

bad:	fd = -1;
	return (NULL);
}
