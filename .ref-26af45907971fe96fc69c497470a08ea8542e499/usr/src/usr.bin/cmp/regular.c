/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)regular.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include <limits.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "extern.h"

void
c_regular(fd1, file1, skip1, len1, fd2, file2, skip2, len2)
	int fd1, fd2;
	char *file1, *file2;
	off_t skip1, len1, skip2, len2;
{
	register u_char ch, *p1, *p2;
	register off_t byte, length, line;
	int dfound;

	if (sflag && len1 != len2)
		exit(1);

	if (skip1 > len1)
		eofmsg(file1);
	len1 -= skip1;
	if (skip2 > len2)
		eofmsg(file2);
	len2 -= skip2;

	length = MIN(len1, len2);
	if (length > SIZE_T_MAX)
		return (c_special(fd1, file1, skip1, fd2, file2, skip2));

	if ((p1 = (u_char *)mmap(NULL,
	    (size_t)length, PROT_READ, 0, fd1, skip1)) == (u_char *)-1)
		err("%s: %s", file1, strerror(errno));
	if ((p2 = (u_char *)mmap(NULL,
	    (size_t)length, PROT_READ, 0, fd2, skip2)) == (u_char *)-1)
		err("%s: %s", file2, strerror(errno));

	dfound = 0;
	for (byte = line = 1; length--; ++p1, ++p2, ++byte) {
		if ((ch = *p1) != *p2)
			if (lflag) {
				dfound = 1;
				(void)printf("%6qd %3o %3o\n", byte, ch, *p2);
			} else
				diffmsg(file1, file2, byte, line);
				/* NOTREACHED */
		if (ch == '\n')
			++line;
	}

	if (len1 != len2)
		eofmsg (len1 > len2 ? file2 : file1);
	if (dfound)
		exit(1);
}
