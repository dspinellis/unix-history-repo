/* in_cksum.c 1.1 81/10/14 */

#include <sys/types.h>
#include "../bbnnet/net.h"
#include "../bbnnet/count.h"

/*
 * Network primitives; this file varies per-cpu,
 * and the code here is for VAX only.
 */
cksum(m, len)
register struct mbuf *m;
{
	int i,j;
	i = ncksum(m, len);
	j = ocksum(m, len);
	if (i != j) {
		printf("old %x new %x \n", i, j);
		while (m != 0) {
			printf("m->m_off %d m->m_len %d\n", m->m_off, m->m_len);
			m = m->m_next;
		}
	}
	return (j);
}

/*
 * Checksum routine for TCP/IP headers.  This
 * is very heavily used in the network
 * code and should be rewritten for each CPU
 * to be as fast as possible.
 */
ncksum(m, len)
	register struct mbuf *m;
	register int len;
{
	register u_short *w;		/* known to be r9 */
	register int sum = 0;		/* known to be r8 */
	register int mlen = 0;
COUNT(NCKSUM);

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
			if (m == 0)
				panic("cksum: out of data");
			if (m->m_len)
				break;
			m = m->m_next;
		}
	}
	return(~(sum + (sum >> 16)) & 0xffff);
}

/*
 * These routines are implemented as inline expansions
 * and are mentioned here for reference only
 *
 *	htons and ntohs		do byte reverse of a 16 bit integer
 *	htonl and ntohl		do byte reverse of a 32 bit integer
 */
ocksum(m, len)
register struct mbuf *m;
register len;
{
	register unsigned short *w;
	register unsigned long sum;
	register mlen;
COUNT(OCKSUM);

	w = (unsigned short *)((int)m + m->m_off);
	mlen = m->m_len;
	sum = 0;

	for (; len > 0; len -= 2, mlen -= 2) {

try:            if (mlen > 1) {         /* can get a word */

			if (len > 1) {
				sum += *(w++);

			} else            /* trailing odd byte */

				sum += *((char *)w) & 0xff;

		} else if (mlen > 0) {  /* last byte of mbuf */

			sum += *((char *)w) & 0xff;

			if (len > 1) {

        			/* get next good mbuf for hi byte */
        
        			while ((m = m->m_next) != 0 && 
					(mlen = m->m_len + 1) == 1);
        			if (m != 0) {
        				w = (unsigned short *)((int)m + m->m_off);
        				sum += (*((char *)w) & 0xff) << 8;
					w = (unsigned short *)((int)w + 1);
        			} else
        				len = 0;        /* force loop exit */
			}

		} else {                /* end of mbuf, get next and try again */

			while ((m = m->m_next) != 0 && (mlen = m->m_len) == 0);
			if (m != 0) {
				w = (unsigned short *)((int)m + m->m_off);
				goto try;
			} else
				break;
		}
	}

	/* add in one's complement carry */

	sum = (sum + (sum >> 16)) & 0xffff;
	return(~sum & 0xffff);
}
