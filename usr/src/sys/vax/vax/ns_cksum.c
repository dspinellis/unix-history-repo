/*
 * Copyright (c) 1985, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)ns_cksum.c	7.6 (Berkeley) 12/16/90
 */

#include "sys/param.h"
#include "sys/mbuf.h"


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
	register int mlen = low;	/* want 0, shuts lint up about low */

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
		 * Force to long boundary so we do longword aligned
		 * memory operations.  It is too hard to do byte
		 * adjustment, do only word adjustment.
		 */
		if (((int)w&0x2) && mlen >= 2) {
			sum += *w++;
			sum += sum;
			mlen -= 2;
		}
		/*
		 *
		 * We can do a 16 bit ones complement sum using
		 * 32 bit arithmetic registers for adding,
		 * with carries from the low added
		 * into the high (by normal carry-chaining)
		 * so long as we fold back before 16 carries have occured.
		 *
		 */
		while ((mlen -= 32) >= 0) {
			/*asm("bicpsw $1");		 clears carry */
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
		while ((mlen -= 8) >= 0) {
			/*asm("bicpsw $1");		 clears carry */
			FOLD;
			ADD; ADD; ADD; ADD;
		}
		mlen += 8;
		/*
		 * Now eliminate the possibility of carry-out's by
		 * folding back to a 16 bit number (adding high and
		 * low parts together.)  Then mop up trailing words
		 * and maybe an odd byte.
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
				printf("idpcksum: out of data\n");
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
	return (sum);
}
