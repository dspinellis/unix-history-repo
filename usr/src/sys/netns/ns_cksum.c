/*	ns_cksum.c	1.1	86/01/23	*/

/*
 * ns_cksum.c - Xerox Internet Datagram protocol checksum
 */
#include "types.h"
#include "mbuf.h"

/*
 * Perform (slowly) the Xerox Internet checksum algorithm on a
 *	chain of mbufs.  This means we add all the 16-bits words,
 *	shifting the sum after each 16-bit add.  Ones-complement
 *	arithmetic is required, so we fold the carry bits after
 *	each 16-bit add as well.
 * If the result is the *no-checksum* value 0xffff, return zero instead.
 *
 * Chris Torek <chris@maryland>
 * James O'Toole <james@maryland>
 */
u_short
ns_cksum(m, len)
	register struct mbuf *m;
	register int len;
{
	register int cksum = 0;
	register int shift = 8;
	register u_char *p;
	register int mlen;

	if (len & 1)
		printf("ns_cksum: odd length\n");
	while (m && len) {
		p = mtod(m, u_char *);
		mlen = m->m_len;
		if ((len -= mlen) < 0)
			mlen += len, len = 0;
		while (--mlen >= 0) {
			cksum += *p++ << shift;
			if ((shift = 8 - shift) != 0) {
				cksum <<= 1;
				cksum = (cksum & 0xffff) + (cksum >> 16);
			}
		}
		m = m->m_next;
	}
#ifdef notdef
	if (len)
		printf("ns_cksum: out of data\n");
#endif
	return (cksum == 0xffff ? 0 : cksum);
}
