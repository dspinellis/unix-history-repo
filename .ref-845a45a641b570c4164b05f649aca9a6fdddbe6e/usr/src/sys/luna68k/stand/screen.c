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
 *	@(#)screen.c	7.1 (Berkeley) %G%
 */

/*
 * screen.c -- screen handler
 * by A.Fujita, Jun-17-1992
 */

#include <sys/param.h>
#include <luna68k/stand/status.h>

int
screen(argc, argv)
	int   argc;
	char *argv[];
{
	int i, j, flag;
	register char *p;
	short hcnt, vcnt;

	if (!strcmp(argv[1], "clear")) {
		bmdclear();
	} else if (!strcmp(argv[1], "adjust")) {
		hcnt = vcnt = 0;

		flag = 0;
		for (p = argv[2] ; *p != '\0'; p++) {
			if (*p == '-')
				flag++;
			else
				hcnt = (hcnt * 10) + (*p - 0x30);
		}
		if (flag)
			hcnt = -1 * hcnt;

		flag = 0;
		for (p = argv[3] ; *p != '\0'; p++) {
			if (*p == '-')
				flag++;
			else
				vcnt = (vcnt * 10) + (*p - 0x30);
		}
		if (flag)
			vcnt = -1 * vcnt;

		bmdadjust(hcnt, vcnt);
	} else if (!strcmp(argv[1], "number")) {
		for (j = 0; j < 50; j++)
			for (i = 0; i < 10; i++)
				bmdputc( 0x30 + i );

	} else if (!strcmp(argv[1], "alpha")) {
		for (j = 0; j < 26; j++) {
			for (i = 0; i < 90; i++) {
				bmdputc(0x41 + j);
			}
			bmdputc(0x0D);
			bmdputc(0x0A);
		}
	}

	return(ST_NORMAL);
}
