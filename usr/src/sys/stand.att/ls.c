/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ls.c	6.2 (Berkeley) %G%
 */

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/ino.h"
#include "../h/dir.h"
#include "saio.h"

char line[100];

main()
{
	int i;

	printf("ls\n");
	do  {
		printf(": "); gets(line);
		i = open(line, 0);
	} while (i < 0);

	ls(i);
}

ls(io)
register io;
{
	struct direct d;
	register i;

	while (read(io, (char *)&d, sizeof d) == sizeof d) {
		if (d.d_ino == 0)
			continue;
		printf("%d\t", d.d_ino);
		for (i=0; i<DIRSIZ; i++) {
			if (d.d_name[i] == 0)
				break;
			printf("%c", d.d_name[i]);
		}
		printf("\n");
	}
}
