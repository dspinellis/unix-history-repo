/*
 * Copyright (c) 1988, 1990 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: in_cksum.c 1.1 90/07/09$
 *
 *	@(#)in_cksum.c	7.5 (Berkeley) %G%
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
