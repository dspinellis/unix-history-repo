/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)flcopy.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

#include <sys/file.h>
#include "pathnames.h"

int	floppydes;
char	*flopname = _PATH_FLOPPY;
long	dsize = 77 * 26 * 128;
int	hflag;
int	rflag;

main(argc, argv)
	register char **argv;
{
	static char buff[512];
	register long count;
	register startad = -26 * 128;
	register int n, file;
	register char *cp;

	while ((cp = *++argv), --argc > 0) {
		while (*cp) {
			switch(*cp++) {

			case '-':
				continue;

			case 'h':
				hflag++;
				printf("Halftime!\n");
				if ((file = open("floppy", 0)) < 0) {
					printf("can't open \"floppy\"\n");
					exit(1);
				}
				continue;

			case 'f':
				if (argc < 1) {
					printf(
					    "flcopy: -f: missing file name\n");
					exit(1);
				}
				flopname = *++argv;
				argc--;
				break;

			case 't':
				if (*cp >= '0' && *cp <= '9')
					dsize = atoi(cp);
				else if (argc > 1) {
					dsize = atoi(*++argv);
					argc--;
				} else
					dsize = 77;
				if (dsize <= 0 || dsize > 77) {
					printf("Bad number of tracks\n");
					exit(2);
				}
				dsize *= 26 * 128;
				continue;

			case 'r':
				rflag++;
			}
			break;
		}
	}
	if (!hflag) {
		file = open("floppy", O_RDWR|O_CREAT|O_TRUNC, 0666);
		if (file < 0) {
			printf("can't open \"floppy\"\n");
			exit(1);
		}
		for (count = dsize; count > 0 ; count -= 512) {
			n = count > 512 ? 512 : count;
			lread(startad, n, buff);
			write(file, buff, n);
			startad += 512;
		}
	}
	if (rflag)
		exit(0);
	printf("Change Floppy, Hit return when done.\n");
	gets(buff);
	lseek(file, 0, 0);
	count = dsize;
	startad = -26 * 128;
	for ( ; count > 0 ; count -= 512) {
		n = count > 512 ? 512 : count;
		read(file, buff, n);
		lwrite(startad, n, buff);
		startad += 512;
	}
	exit(0);
}

rt_init()
{
	static initized = 0;
	int mode = 2;

	if (initized)
		return;
	if (rflag)
		mode = 0;
	initized = 1;
	if ((floppydes = open(flopname, mode)) < 0) {
		printf("Floppy open failed\n");
		exit(1);
	}
}

/*
 * Logical to physical adress translation
 */
long
trans(logical)
	register int logical;
{
	register int sector, bytes, track;

	logical += 26 * 128;
	bytes = (logical & 127);
	logical >>= 7;
	sector = logical % 26;
	if (sector >= 13)
		sector = sector*2 +1;
	else
		sector *= 2;
	sector += 26 + ((track = (logical / 26)) - 1) * 6;
	sector %= 26;
	return ((((track *26) + sector) << 7) + bytes);
}

lread(startad, count, obuff)
	register startad, count;
	register char *obuff;
{
	long trans();
	extern floppydes;

	rt_init();
	while ((count -= 128) >= 0) {
		lseek(floppydes, trans(startad), 0);
		read(floppydes, obuff, 128);
		obuff += 128;
		startad += 128;
	}
}

lwrite(startad, count, obuff)
	register startad, count;
	register char *obuff;
{
	long trans();
	extern floppydes;

	rt_init();
	while ((count -= 128) >= 0) {
		lseek(floppydes, trans(startad), 0);
		write(floppydes, obuff, 128);
		obuff += 128;
		startad += 128;
	}
}
