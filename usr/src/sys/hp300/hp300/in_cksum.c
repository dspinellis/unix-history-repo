/*
 * Copyright (c) 1988, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 * from: Utah $Hdr: in_cksum.c 1.1 90/07/09$
 *
 *	@(#)in_cksum.c	8.1 (Berkeley) 6/10/93
 */

/*
 * in_cksum - checksum routine for the Internet Protocol family.
 */

#include <sys/param.h>
#include <sys/mbuf.h>

extern int oc_cksum();

/*
 * Checksum routine for the Internet Protocol family.
 *
 * This isn't as bad as it looks.  For ip headers the "while" isn't
 * executed and we just drop through to the return statement at the
 * end.  For the usual tcp or udp packet (a single header mbuf
 * chained onto a cluster of data, we make exactly one trip through
 * the while (for the header mbuf) and never do the hairy code
 * inside the "if".  If fact, if m_copydata & sb_compact are doing
 * their job, we should never do the hairy code inside the "if".
 */
in_cksum(m, len)
	register struct mbuf *m;
	register int len;
{
	register int sum = 0;
	register int i;

	while (len > m->m_len) {
		sum = oc_cksum(mtod(m, u_char *), i = m->m_len, sum);
		m = m->m_next;
		len -= i;
		if (i & 1) {
			/*
			 * ouch - we ended on an odd byte with more
			 * to do.  This xfer is obviously not interested
			 * in performance so finish things slowly.
			 */
			register u_char *cp;

			while (len > m->m_len) {
				cp = mtod(m, u_char *);
				if (i & 1) {
					i = m->m_len - 1;
					--len;
					sum += *cp++;
				} else
					i = m->m_len;

				sum = oc_cksum(cp, i, sum);
				m = m->m_next;
				len -= i;
			}
			if (i & 1) {
				cp =  mtod(m, u_char *);
				sum += *cp++;
				return (0xffff & ~oc_cksum(cp, len - 1, sum));
			}
		}
	}
	return (0xffff & ~oc_cksum(mtod(m, u_char *), len, sum));
}
