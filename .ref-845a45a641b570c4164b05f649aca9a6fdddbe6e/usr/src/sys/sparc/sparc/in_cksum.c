/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)in_cksum.c	7.4 (Berkeley) %G%
 *
 * from: $Header: in_cksum.c,v 1.7 92/11/26 03:04:52 torek Exp $
 */

#include <sys/param.h>
#include <sys/mbuf.h>

#include <netinet/in.h>
#include <netinet/in_systm.h>

/*
 * Checksum routine for Internet Protocol family headers.
 * This routine is very heavily used in the network
 * code and should be modified for each CPU to be as fast as possible.
 * In particular, it should not be this one.
 */
int
in_cksum(m, len)
	register struct mbuf *m;
	register int len;
{
	register int sum = 0, i, oddbyte = 0, v = 0;
	register u_char *cp;

	/* we assume < 2^16 bytes being summed */
	while (len) {
		while ((i = m->m_len) == 0)
			m = m->m_next;
		if (i > len)
			i = len;
		len -= i;
		cp = mtod(m, u_char *);
		if (oddbyte) {
			sum += v + *cp++;
			i--;
		}
		if (((int)cp & 1) == 0) {
			while ((i -= 2) >= 0) {
				sum += *(u_short *)cp;
				cp += 2;
			}
		} else {
			while ((i -= 2) >= 0) {
				sum += *cp++ << 8;
				sum += *cp++;
			}
		}
		if ((oddbyte = i & 1) != 0)
			v = *cp << 8;
		m = m->m_next;
	}
	if (oddbyte)
		sum += v;
	sum = (sum >> 16) + (sum & 0xffff); /* add in accumulated carries */
	sum += sum >> 16;		/* add potential last carry */
	return (0xffff & ~sum);
}
