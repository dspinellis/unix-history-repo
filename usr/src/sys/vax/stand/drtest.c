/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)drtest.c	7.2 (Berkeley) %G%
 */

/*
 * Standalone program to test a disk and driver
 * by reading the disk a track at a time.
 */
#include "param.h"
#include "inode.h"
#include "fs.h"

#include "saio.h"

#define SECTSIZ	512

extern	int end;
char	*malloc();

main()
{
	char *cp, *bp;
	int fd, tracksize, debug;
	register int sector, lastsector;
	struct st st;

	printf("Testprogram for stand-alone driver\n\n");
again:
	debug = getdebug("Enable debugging (1=bse, 2=ecc, 3=bse+ecc)? ");
	if (debug < 0)
		debug = 0;
	fd = getfile("Device to read?", 2);
	ioctl(fd, SAIODEVDATA, (char *)&st);
	printf("Device data: #cylinders=%d, #tracks=%d, #sectors=%d\n",
		st.ncyl, st.ntrak, st.nsect);
	ioctl(fd, SAIODEBUG, (char *)debug);
	tracksize = st.nsect * SECTSIZ;
	bp = malloc(tracksize);
	printf("Reading in %d byte records\n", tracksize);
	printf("Start ...make sure drive is on-line\n");
	lseek(fd, 0, L_SET);
	lastsector = st.ncyl * st.nspc;
	for (sector = 0; sector < lastsector; sector += st.nsect) {
		if (sector && (sector % (st.nspc * 10)) == 0)
			printf("cylinder %d\n", sector/st.nspc);
		read(fd, bp, tracksize);
	}
	goto again;
}

getdebug(msg)
	char *msg;
{
	char buf[132];

	printf("%s", msg);
	gets(buf);
	return (atoi(buf));
}

/*
 * Allocate memory on a page-aligned address.
 * Round allocated chunk to a page multiple to
 * ease next request.
 */
char *
malloc(size)
	int size;
{
	char *result;
	static caddr_t last = 0;

	if (last == 0)
		last = (caddr_t)(((int)&end + 511) & ~0x1ff);
	size = (size + 511) & ~0x1ff;
	result = (char *)last;
	last += size;
	return (result);
}
