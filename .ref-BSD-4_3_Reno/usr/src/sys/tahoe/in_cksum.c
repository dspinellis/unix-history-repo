/*
 * Copyright (c) 1982, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)in_cksum.c	7.5 (Berkeley) 6/28/90
 */

#include "param.h"
#include "mbuf.h"

/*
 * Checksum routine for Internet Protocol family headers (CCI Version).
 *
 * This routine is very heavily used in the network
 * code and should be modified for each CPU to be as fast as possible.
 */

#define ADDCARRY(x)  { if ((x) > 65535) (x) -= 65535; }
#define REDUCE {l_util.l = sum; sum = l_util.s[0] + l_util.s[1]; ADDCARRY(sum);}

#define ADD(n)	asm("adwc n (r10),r9")
#define MOP	asm("adwc $0,r9")
#define BOTCH	asm("addl2 r7,r9")

in_cksum(m, len)
	register struct mbuf *m;
	register int len;
{
	register u_short *w;		/* On CCI, known to be r10 */
	register int sum = 0;		/* On CCI, known to be r9 */
	register int mlen = 0;
#ifndef lint
	register int ClearCarry = 0;	/* On CCI, known to be r7; see BOTCH */
#endif
	int byte_swapped = 0;

	union {
		char	c[2];
		u_short	s;
	} s_util;
	union {
		u_short s[2];
		long	l;
	} l_util;

	for (;m && len; m = m->m_next) {
		if (m->m_len == 0)
			continue;
		w = mtod(m, u_short *);
		if (mlen == -1) {
			/*
			 * The first byte of this mbuf is the continuation
			 * of a word spanning between this mbuf and the
			 * last mbuf.
			 *
			 * s_util.c[0] is already saved when scanning previous 
			 * mbuf.  sum was REDUCEd when we found mlen == -1.
			 */
			s_util.c[1] = *(char *)w;
			sum += s_util.s;
			w = (u_short *)((char *)w + 1);
			mlen = m->m_len - 1;
			len--;
		} else
			mlen = m->m_len;
		if (len < mlen)
			mlen = len;
		len -= mlen;
		/*
		 * Force to long boundary so we do longword aligned
		 * memory operations.
		 */
		if (3 & (int) w) {
			REDUCE;
			if ((1 & (int) w) && (mlen > 0)) {
				sum <<= 8;
				s_util.c[0] = *(char *)w;
				w = (u_short *)((char *)w + 1);
				mlen--;
				byte_swapped = 1;
			}
			if ((2 & (int) w) && (mlen >= 2)) {
				sum += *w++;
				mlen -= 2;
			}
		}
		/*
		 * Do as much of the checksum as possible 32 bits at at time.
		 * In fact, this loop is unrolled to make overhead from
		 * branches &c small.
		 */
		while ((mlen -= 32) >= 0) {
			/*
			 * The loop construct clears carry for us
			 * on vaxen, however, on the CCI machine subtracting
			 * a small postive number from a larger one doesn't.
			 * 
			 * Doing a bicpsw is very slow (slows down the routine
			 * by a factor of 2); explicitly adding an immediate
			 * 0 to a register is optimized out; so we fake out
			 * the optimizer and add a register whose contents
			 * is always zero.
			 */
			BOTCH;
			ADD(0); ADD(4); ADD(8); ADD(12);
			ADD(16); ADD(20); ADD(24); ADD(28);
			MOP; w += 16;
		}
		mlen += 32;
		while ((mlen -= 8) >= 0) {
			BOTCH;
			ADD(0); ADD(4);
			MOP;
			w += 4;
		}
		mlen += 8;
		if (mlen == 0 && byte_swapped == 0)
			continue;	/* worth 1% maybe ?? */
		REDUCE;
		while ((mlen -= 2) >= 0) {
			sum += *w++;
		}
		if (byte_swapped) {
			sum <<= 8;
			byte_swapped = 0;
			if (mlen == -1) {
				s_util.c[1] = *(char *)w;
				sum += s_util.s;
				mlen = 0;
			} else
				mlen = -1;
		} else if (mlen == -1)
			/*
			 * This mbuf has odd number of bytes. 
			 * There could be a word split betwen
			 * this mbuf and the next mbuf.
			 * Save the last byte (to prepend to next mbuf).
			 */
			s_util.c[0] = *(char *)w;
	}
	if (len)
		printf("cksum: out of data\n");
	if (mlen == -1) {
		/* The last mbuf has odd # of bytes. Follow the
		   standard (the odd byte is shifted left by 8 bits) */
		s_util.c[1] = 0;
		sum += s_util.s;
	}
	REDUCE;
	return (~sum & 0xffff);
}
