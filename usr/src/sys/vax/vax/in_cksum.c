/* in_cksum.c 1.3 81/10/18 */

#include <sys/types.h>
#include "../bbnnet/net.h"
#include "../bbnnet/count.h"

/*
 * Network primitives; this file varies per-cpu,
 * and the code here is for VAX only.
 */

/*
 * Checksum routine for TCP/IP headers.  This
 * is very heavily used in the network
 * code and should be rewritten for each CPU
 * to be as fast as possible.
 */
cksum(m, len)
	register struct mbuf *m;
	register int len;
{
	register u_short *w;		/* known to be r9 */
	register int sum = 0;		/* known to be r8 */
	register int mlen = 0;
COUNT(CKSUM);

	for (;;) {
		w = (u_short *)((int)m + m->m_off);
		if (mlen == -1) {
			sum += *(u_char *)w << 8;
			w = (unsigned short *)((char *)w + 1);
			mlen = m->m_len - 1;
			len--;
		} else
			mlen = m->m_len;
		m = m->m_next;
		if (len < mlen)
			mlen = len;
		len -= mlen;
		while ((mlen -= 32) >= 0) {
#define ADD		asm("movzwl (r9)+,r0; addl2 r0,r8");
			ADD; ADD; ADD; ADD; ADD; ADD; ADD; ADD;
			ADD; ADD; ADD; ADD; ADD; ADD; ADD; ADD;
		}
		mlen += 32;
		while ((mlen -= 8) >= 0) {
			ADD; ADD; ADD; ADD;
		}
		mlen += 8;
		while ((mlen -= 2) >= 0) {
			ADD;
		}
		if (mlen == -1)
			sum += *(u_char *)w;
		if (len == 0)
			break;
		for (;;) {
			if (m == 0) {
				printf("cksum: out of data");
				goto done;
			}
			if (m->m_len)
				break;
			m = m->m_next;
		}
	}
done:
	return(~(sum + (sum >> 16)) & 0xffff);
}

/*
 * These routines are implemented as inline expansions
 * and are mentioned here for reference only
 *
 *	htons and ntohs		do byte reverse of a 16 bit integer
 *	htonl and ntohl		do byte reverse of a 32 bit integer
 */
