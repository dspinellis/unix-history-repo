/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)drtest.c	7.3 (Berkeley) 3/4/88
 */

/*
 * Standalone program to test a disk and driver
 * by reading the disk a track at a time.
 */
#include "param.h"
#include "inode.h"
#include "fs.h"
#include "disklabel.h"

#include "saio.h"

#define SECTSIZ	512

extern	int end;
char	*malloc();

main()
{
	register int fd, sector, lastsector, tracksize;
	register char *bp;
	struct disklabel dl;
	int debug;

	printf("Testprogram for stand-alone driver\n\n");
again:
	debug = getdebug("Enable debugging (0=none, 1=bse, 2=ecc, 3=bse+ecc)? ");
	if (debug < 0)
		debug = 0;
	fd = getfile("Device to read?", 2);
	ioctl(fd, SAIODEVDATA, &dl);
	printf("Device data: #cylinders=%d, #tracks=%d, #sectors=%d\n",
		dl.d_ncylinders, dl.d_ntracks, dl.d_nsectors);
	ioctl(fd, SAIODEBUG, (char *)debug);
	tracksize = dl.d_nsectors * SECTSIZ;
	bp = malloc(tracksize);
	printf("Reading in %d byte records\n", tracksize);
	printf("Start ...make sure drive is on-line\n");
	lseek(fd, 0, L_SET);
	lastsector = dl.d_ncylinders * dl.d_secpercyl;
	for (sector = 0; sector < lastsector; sector += dl.d_nsectors) {
		if (sector && (sector % (dl.d_secpercyl * 10)) == 0)
			printf("cylinder %d\n", sector/dl.d_secpercyl);
		read(fd, bp, tracksize);
	}
	goto again;
	/*NOTREACHED*/
}

static
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
static char *
malloc(size)
	int size;
{
	static caddr_t last = 0;
	char *result;

	if (last == 0)
		last = (caddr_t)(((int)&end + 511) & ~0x1ff);
	size = (size + 511) & ~0x1ff;
	result = (char *)last;
	last += size;
	return (result);
}
