/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)ls.c	7.1 (Berkeley) %G%
 */

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#undef KERNEL
#include "../h/dir.h"
#include "saio.h"

char line[100];

main()
{
	int i;

	while (1) {
		printf("\nDirectory: ");
		gets(line);
		if ((i = open(line, 0)) < 0)
			printf("Cant open directory %s\n", line);
		else {
			ls(i);
			close(i);
		}
	}
}

ls(io)
register io;
{
	char buf[DIRBLKSIZ];
	register struct direct *dp;
	register off;

	while (read(io, buf, DIRBLKSIZ) > 0) {
		for (off = 0 ; off < DIRBLKSIZ ; off += dp->d_reclen) {
			dp = (struct direct *)(off + (int) buf);
			if (dp->d_ino == 0)
				break;
			printf("%d\t", dp->d_ino);
			printf("%s\n", dp->d_name);
		}
	}
}
