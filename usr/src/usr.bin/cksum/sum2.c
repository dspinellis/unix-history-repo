/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)sum2.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <unistd.h>

int
csum2(fd, cval, clen)
	register int fd;
	u_long *cval, *clen;
{
	register u_long crc, total;
	register int nr;
	register u_char *p;
	u_char buf[8192];

	/*
	 * Draft 8 POSIX 1003.2:
	 *
	 *   s = sum of all bytes
	 *   r = s % 2^16 + (s % 2^32) / 2^16
	 * crc = (r % 2^16) + r / 2^16
	 */
	crc = total = 0;
	while ((nr = read(fd, buf, sizeof(buf))) > 0)
		for (total += nr, p = buf; nr--; ++p)
			crc += *p;
	if (nr < 0)
		return(1);

	crc = (crc & 0xffff) + (crc >> 16);
	crc = (crc & 0xffff) + (crc >> 16);

	*cval = crc;
	*clen = total;
	return(0);
}
