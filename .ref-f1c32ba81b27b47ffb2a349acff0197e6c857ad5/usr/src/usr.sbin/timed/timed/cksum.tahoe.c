/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cksum.tahoe.c	2.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

/*
 * Checksum routine for Internet Protocol family headers.
 *
 * This routine is very heavily used in the network
 * code and should be modified for each CPU to be as fast as possible.
 * 
 * This implementation is TAHOE version.
 */

#undef	ADDCARRY
#define ADDCARRY(sum) { \
	if (sum & 0xffff0000) {	\
		sum &= 0xffff; \
		sum++; \
	} \
}

in_cksum(addr, len)
	register u_short *addr;
	register int len;
{
	union word {
		char	c[2];
		u_short	s;
	} u;
	register int sum = 0;

	while (len > 0) {
		/*
		 * add by words.
		 */
		while ((len -= 2) >= 0) {
			if ((int)addr & 0x1) {
				/* word is not aligned */
				u.c[0] = *(char *)addr;
				u.c[1] = *((char *)addr+1);
				sum += u.s;
				addr++;
			} else
				sum += *addr++;
			ADDCARRY(sum);
		}
		if (len == -1)
			/*
			 * Odd number of bytes. 
			 */
			u.c[0] = *(u_char *)addr;
	}
	if (len == -1) {
		/* The last mbuf has odd # of bytes. Follow the
		   standard (the odd byte is shifted left by 8 bits) */
		u.c[1] = 0;
		sum += u.s;
		ADDCARRY(sum);
	}
	return (~sum & 0xffff);
}
