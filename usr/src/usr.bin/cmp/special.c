/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)special.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "extern.h"

void
c_special(fd1, file1, skip1, fd2, file2, skip2)
	int fd1, fd2;
	char *file1, *file2;
	register off_t skip1, skip2;
{
	register int ch1, ch2;
	register off_t byte, line;
	FILE *fp1, *fp2;
	int dfound;

	if ((fp1 = fdopen(fd1, "r")) == NULL)
		err("%s: %s", file1, strerror(errno));
	if ((fp2 = fdopen(fd2, "r")) == NULL)
		err("%s: %s", file1, strerror(errno));

	while (skip1--)
		if (getc(fp1) == EOF)
			goto eof;
	while (skip2--)
		if (getc(fp2) == EOF)
			goto eof;

	dfound = 0;
	for (byte = line = 1;; ++byte) {
		ch1 = getc(fp1);
		ch2 = getc(fp2);
		if (ch1 == EOF || ch2 == EOF)
			break;
		if (ch1 != ch2)
			if (lflag) {
				dfound = 1;
				(void)printf("%6qd %3o %3o\n", byte, ch1, ch2);
			} else
				diffmsg(file1, file2, byte, line);
				/* NOTREACHED */
		if (ch1 == '\n')
			++line;
	}

eof:	if (ferror(fp1))
		err("%s: %s", file1, strerror(errno));
	if (ferror(fp2))
		err("%s: %s", file2, strerror(errno));
	if (feof(fp1)) {
		if (!feof(fp2))
			eofmsg(file1);
	} else
		if (feof(fp2))
			eofmsg(file2);
	if (dfound)
		exit(1);
}
