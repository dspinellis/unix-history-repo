/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)pup_cksum.c	6.3 (Berkeley) %G%
 */

#include "types.h"
#include "mbuf.h"

/*
 * Checksum routine for PUP-I Protocol family (VAX Version).
 */
pup_cksum(m, len)
	register struct mbuf *m;
	register int len;
{
	register u_short *w;
	register int sum = 0;			/* known to be r8 */
	register int mlen = 0;

	for (;;) {
		/*
		 * Each trip around loop adds in
		 * words from one mbuf segment.
		 */
		w = mtod(m, u_short *);
		if (mlen == -1) {
			/*
			 * There is a byte left from the last segment;
			 * add it into the checksum. 
			 */
			sum += *(u_char *)w << 8;
			asm("rotl $1,r8,r8");
			w = (u_short *)((char *)w + 1);
			mlen = m->m_len - 1;
			len--;
		} else
			mlen = m->m_len;
		m = m->m_next;
		if (len < mlen)
			mlen = len;
		len -= mlen;
		while ((mlen -= 2) >= 0) {
			asm("addw2 (r9)+,r8; rotl $1,r8,r8;");
		}
		if (mlen == -1)
			sum += *(u_char *)w;
		if (len == 0)
			break;
		/*
		 * Locate the next block with some data.
		 * If there is a word split across a boundary we
		 * will wrap to the top with mlen == -1 and
		 * then add it in shifted appropriately.
		 */
		for (;;) {
			if (m == 0) {
				printf("pup_cksum: out of data\n");
				goto done;
			}
			if (m->m_len)
				break;
			m = m->m_next;
		}
	}
done:
	return (sum);
}
