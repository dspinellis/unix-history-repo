/*
 * Copyright (c) 1988, 1990 Regents of the University of California.
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
 * from: Utah $Hdr: in_cksum.c 1.6 89/08/24$
 *
 *	@(#)in_cksum.c	7.1 (Berkeley) 5/8/90
 */

/*
 * in_cksum - checksum routine for the Internet Protocol family.
 */

#include "param.h"
#include "mbuf.h"
#include "../netinet/in.h"
#include "../netinet/in_systm.h"

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
