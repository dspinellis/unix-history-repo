/* in_cksum.c 1.6 81/10/28 */

#include <sys/types.h>
#include "../bbnnet/net.h"
#include "../bbnnet/mbuf.h"
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
	register long *l;		/* known to be r9 */
	register int sum = 0;		/* known to be r8 */
	register u_short *w;		/* known to be r7 */
	register int mlen = 0;
COUNT(CKSUM);

	for (;;) {
		w = (u_short *)((int)m + m->m_off);
		if (mlen == -1) {
			sum += *(u_char *)w << 8;
			w = (u_short *)((char *)w + 1);
			mlen = m->m_len - 1;
			len--;
		} else
			mlen = m->m_len;
		m = m->m_next;
		if (len < mlen)
			mlen = len;
		len -= mlen;
		l = (long *)w;
		while ((mlen -= 32) >= 0) {
			asm("clrl r0");		/* clears carry */
#undef ADD
#define ADD		asm("adwc (r9)+,r8;");
			ADD; ADD; ADD; ADD; ADD; ADD; ADD; ADD;
			asm("adwc $0,r8");
		}
		mlen += 32;
		while ((mlen -= 8) >= 0) {
			asm("clrl r0");
			ADD; ADD;
			asm("adwc $0,r8");
		}
		mlen += 8;
		sum = ((sum >> 16) & 0xffff) + (sum & 0xffff);
		w = (u_short *)l;
		while ((mlen -= 2) >= 0) {
			asm("movzwl (r7)+,r0; addl2 r0,r8");
		}
		if (mlen == -1)
			sum += *(u_char *)w;
		if (len == 0)
			break;
		for (;;) {
			if (m == 0) {
				printf("cksum: out of data\n");
				goto done;
			}
			if (m->m_len)
				break;
			m = m->m_next;
		}
	}
done:
	/* add together high and low parts of sum and carry to get cksum */
	{ asm("ashl $-16,r8,r0; addw2 r0,r8; adwc $0,r8");
	  asm("mcoml r8,r8; movzwl r8,r8"); }
	return (sum);
}
