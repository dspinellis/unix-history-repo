/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)sum1.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <unistd.h>

int
csum1(fd, cval, clen)
	register int fd;
	u_long *cval, *clen;
{
	register u_long total;
	register int nr;
	register u_int crc;
	register u_char *p;
	u_char buf[8192];

	/*
	 * 16-bit checksum, rotating right before each addition;
	 * overflow is discarded.
	 */
	crc = total = 0;
	while ((nr = read(fd, buf, sizeof(buf))) > 0)
		for (total += nr, p = buf; nr--; ++p) {
			if (crc & 1)
				crc |= 0x10000;
			crc = ((crc >> 1) + *p) & 0xffff;
		}
	if (nr < 0)
		return(1);

	*cval = crc;
	*clen = total;
	return(0);
}
