/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tape.c	7.1 (Berkeley) %G%
 */

/*
 * tape.c -- operation commands for TAPE unit.
 * by A.Fujita, APR-14-1992
 */

#include <sys/param.h>
#include <luna68k/stand/status.h>

dev_t  rst0 = 0x0000;
dev_t nrst0 = 0x0004;

u_char buff[512];

int
tape(argc, argv)
	int   argc;
	char *argv[];
{
	int size, count;
	u_long *p = (u_long *) buff;

	if (!strcmp(argv[1], "read")) {
		count = 0;
		while ((size = stread(rst0, buff, 512)) == 512)
			count++;
		printf("tape: size  = %d\n", size);
		printf("tape: count = %d\n", count);
	} else if (!strcmp(argv[1], "write")) {
		for (count = 0; count < 500; count++) {
			if ((size = stwrite(rst0, buff, 512)) != 512)
				break;
		}
		printf("tape: size  = %d\n", size);
		printf("tape: count = %d\n", count);
	} else if (!strcmp(argv[1], "rewind")) {
		st_rewind(rst0);
	} else if (!strcmp(argv[1], "weof")) {
		st_write_EOF(rst0);
	} else if (!strcmp(argv[1], "skip")) {
		st_skip(rst0);
	} else {
		return(ST_ERROR);
	}

	return(ST_NORMAL);
}
