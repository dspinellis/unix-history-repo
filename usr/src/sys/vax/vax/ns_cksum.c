/*	ns_cksum.c	6.1	85/05/30	*/

#include "types.h"
#include "mbuf.h"
#include "../netns/ns.h"

/*
 * Checksum routine for Network Systems Protocol Packets (VAX Version).
 *
 * This routine is very heavily used in the network
 * code and should be modified for each CPU to be as fast as possible.
 */
u_short
ns_cksum(m, len)
	register struct mbuf *m;
	register int len;
{
	register u_short *w;		/* on vax, known to be r9 */
	register int sum = 0;		/* on vax, known to be r8 */
	register int low = 0;		/* on vax, known to be r7 */
	register int mlen = 0;

	for (;;) {
		/*
		 * Each trip around loop adds in
		 * word from one mbuf segment.
		 */
		w = mtod(m, u_short *);
		if (mlen == -1) {
			/*
			 * There is a byte left from the last segment;
			 * add it into the checksum.  Don't have to worry
			 * about a carry-out here because although we may do
			 * the 16th and 17th additions, the contribution
			 * of this byte and the previous cannot cause
			 * more than 1 carry into the high order 16 bits.
			 */
			sum += *(u_char *)w << 8;
			sum += sum;
			w = (u_short *)((char *)w + 1);
			mlen = m->m_len - 1;
			len--;
		} else
			mlen = m->m_len;
		m = m->m_next;
		if (len < mlen)
			mlen = len;
		len -= mlen;
		/*
		 * This loop is unrolled to make overhead from
		 * branches &c small.
		 *
		 * We can do a 16 bit ones complement sum 32 bits at a time
		 * by using regular arithmetic for 16 additions, then
		 * folding the carries in.  Each addition can generate
		 * no more than one carry.
		 *
		 */
		while ((mlen -= 32) >= 0) {
#undef ADD
#define ADD asm("movw (r9)+,r7")asm("addl2 r7,r8")asm("addl2 r8,r8")
#define FOLD { asm("ashl $-16,r8,r0")asm(" addw2 r0,r8"); \
		  asm("adwc $0,r8")asm(" movzwl r8,r8"); }
			FOLD;
			ADD; ADD; ADD; ADD; ADD; ADD; ADD; ADD;
			FOLD;
			ADD; ADD; ADD; ADD; ADD; ADD; ADD; ADD;
		}
		mlen += 32;
		while ((mlen -= 16) >= 0) {
			FOLD;
			ADD; ADD; ADD; ADD; ADD; ADD; ADD; ADD; 
		}
		mlen += 16;
		/*
		 * Now eliminate the possibility of carry-out's by
		 * folding back to a 16 bit number (adding high and
		 * low parts together.)  Then mop up trailing words
		 * and maybe an odd byte. There can be at most 7
		 * words added in this loop, or 14 carries.
		 */
		FOLD;
		while ((mlen -= 2) >= 0) {
			ADD;
		}
		if (mlen == -1) {
			sum += *(u_char *)w;
		}
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
				printf("cksum: out of data\n");
				goto done;
			}
			if (m->m_len)
				break;
			m = m->m_next;
		}
	}
done:
	/*
	 * Add together high and low parts of sum
	 * and carry to get cksum.
	 * Have to be careful to not drop the last
	 * carry here.
	 */
	FOLD;
	if(sum==0xffff) sum = 0;
	return ((u_short)sum);
}
