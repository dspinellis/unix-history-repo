/*	in_cksum.c	6.1	83/07/29	*/

#include "../h/types.h"
#include "../h/mbuf.h"
#include "../netinet/in.h"
#include "../netinet/in_systm.h"

#if defined(tahoe)
/*
 * Checksum routine for Internet Protocol family headers.
 *
 * This routine is very heavily used in the network
 * code and should be modified for each CPU to be as fast as possible.
 * 
 * This implementation is TAHOE version.
 */

#undef	ADDCARRY
#define ADDCARRY(sum)  {				\
			if (sum & 0xffff0000) {		\
				sum &= 0xffff;		\
				sum++;			\
			}				\
		}
in_cksum(m, len)
	register struct mbuf *m;
	register int len;
{
	union word {
		char	c[2];
		u_short	s;
	} u;
	register u_short *w;
	register int sum = 0;
	register int mlen = 0;

	for (;m && len; m = m->m_next) {

		if (m->m_len == 0) {
			continue;
		}

		w = mtod(m, u_short *);
		if (mlen == -1) {
			/*
			 * The first byte of this mbuf is the continuation
			 * of a word spanning between this mbuf and the
			 * last mbuf.
			 */

			/* u.c[0] is already saved when scanning previous 
			 * mbuf.
			 */
			u.c[1] = *(u_char *)w;
			sum += u.s;
			ADDCARRY(sum);

			w = (u_short *)((char *)w + 1);
			mlen = m->m_len - 1;
			len--;
		} else
			mlen = m->m_len;

		if (len < mlen)
			mlen = len;
		len -= mlen;

		/*
		 * add by words.
		 */
		while ((mlen -= 2) >= 0) {
			
			if ((int)w & 0x1) {
				/* word is not aligned */
				u.c[0] = *(char *)w;
				u.c[1] = *((char *)w+1);
				sum += u.s;
				w++;
			}
			else {
				sum += *w++;
			}
			ADDCARRY(sum);
		}
		if (mlen == -1) {
			/*
			 * This mbuf has odd number of bytes. 
			 * There could be a word split betwen
			 * this mbuf and the next mbuf.
			 * Save the last byte (to prepend to next mbuf).
			 */
			u.c[0] = *(u_char *)w;
		}
	}
	if (len)
		printf("cksum: out of data\n");
	if (mlen == -1) {
		/* The last mbuf has odd # of bytes. Follow the
		   standard (the odd byte is shifted left by 8 bits) */
		u.c[1] = 0;
		sum += u.s;
		ADDCARRY(sum);
	}
done:
	return (~sum & 0xffff);
}
#endif

#if defined(vax)

/*
 * Checksum routine for Internet Protocol family headers (VAX Version).
 *
 * This routine is very heavily used in the network
 * code and should be modified for each CPU to be as fast as possible.
 */

in_cksum(m, len)
	register struct mbuf *m;
	register int len;
{
	register u_short *w;		/* on Tahoe, known to be r10 */
	register int sum = 0;		/* on Tahoe, known to be r9 */
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
			 * about a carry-out here because we make sure
			 * that high part of (32 bit) sum is small below.
			 */
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
		/*
		 * Force to long boundary so we do longword aligned
		 * memory operations.  
		 */
		if (((int)w&0x1) && mlen >= 1) {
			sum += *(u_char *)w++;
			mlen -= 1;
		}
		if (((int)w&0x2) && mlen >= 2) {
			sum += *w++;
			mlen -= 2;
		}
		/*
		 * Do as much of the checksum as possible 32 bits at at time.
		 * In fact, this loop is unrolled to make overhead from
		 * branches &c small.
		 *
		 * We can do a 16 bit ones complement sum 32 bits at a time
		 * because the 32 bit register is acting as two 16 bit
		 * registers for adding, with carries from the low added
		 * into the high (by normal carry-chaining) and carries
		 * from the high carried into the low on the next word
		 * by use of the adwc instruction.  This lets us run
		 * this loop at almost memory speed.
		 *
		 * Here there is the danger of high order carry out, and
		 * we carefully use adwc.
		 */
		while ((mlen -= 32) >= 0) {
#undef ADD
			asm("clrl r0");		/* clears carry */
#define ADD		asm("adwc (r10),r9;addl2 $4,r10; ");
			ADD; ADD; ADD; ADD; ADD; ADD; ADD; ADD;
			asm("adwc $0,r9");
		}
		mlen += 32;
		while ((mlen -= 8) >= 0) {
			asm("clrl r0");
			ADD; ADD;
			asm("adwc $0,r9");
		}
		mlen += 8;
		/*
		 * Now eliminate the possibility of carry-out's by
		 * folding back to a 16 bit number (adding high and
		 * low parts together.)  Then mop up trailing words
		 * and maybe an odd byte.
		 */
		{ asm("shal $-16,r9,r0; addw2 r0,r9");
		  asm("adwc $0,r9; movzwl r9,r9"); }
		while ((mlen -= 2) >= 0) {
			asm("movzwl (r10),r0;addl2 $4,r10;  addl2 r0,r9");
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
	{ asm("shal $-16,r9,r0; addw2 r0,r9; adwc $0,r9");
	  asm("mcoml r9,r9; movzwl r9,r9"); }
	return (sum);
}
#endif
