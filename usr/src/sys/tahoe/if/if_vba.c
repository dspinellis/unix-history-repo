/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)if_vba.c	1.1 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "cmap.h"
#include "vmmac.h"
#include "socket.h"

#include "../tahoe/mtpr.h"
#include "../tahoe/pte.h"

#include "../tahoevba/vbavar.h"

#include "../net/if.h"
#include "../netinet/in.h"
#include "../netinet/if_ether.h"

#include "if_vba.h"

if_vbareserve(ifvba0, n, size)
struct ifvba *ifvba0;
register int n;
int size;
{
	register caddr_t cp;
	register struct pte *pte;
	register struct ifvba *ifvba = ifvba0;
	struct ifvba *vlim  = ifvba + n;

	n = roundup(n * size, NBPG);
	cp = (caddr_t)malloc((u_long)n, M_DEVBUF, M_NOWAIT);
	if (cp == 0) {
		printf("No memory for device buffer\n");
		return (0);
	}
	/*
	 * Make raw buffer pages uncacheable.
	 */
	pte = kvtopte(cp);
	for (n = btoc(n); n--; pte++)
		pte->pg_nc = 1;
	mtpr(TBIA, 0);
	for (; ifvba < vlim; ifvba++) {
		ifvba->iff_buffer = cp;
		ifvba->iff_physaddr = kvtophys(cp);
		cp += size;
	}
	return (1);
}
/*
 * Routine to copy from VERSAbus memory into mbufs.
 *
 * Warning: This makes the fairly safe assumption that
 * mbufs have even lengths.
 */
struct mbuf *
if_vbaget(rxbuf, totlen, off, ifp, flags)
	u_char *rxbuf;
	int totlen, off, flags;
	struct ifnet *ifp;
{
	register u_char *cp;
	register struct mbuf *m;
	struct mbuf *top = 0, **mp = &top;
	int len;
	u_char *packet_end;

	rxbuf += sizeof (struct ether_header);
	cp = rxbuf;
	packet_end = cp + totlen;
	if (off) {
		off += 2 * sizeof(u_short);
		totlen -= 2 *sizeof(u_short);
		cp = rxbuf + off;
	}

	MGETHDR(m, M_DONTWAIT, MT_DATA);
	if (m == 0)
		return (0);
	m->m_pkthdr.rcvif = ifp;
	m->m_pkthdr.len = totlen;
	m->m_len = MHLEN;

	while (totlen > 0) {
		if (top) {
			MGET(m, M_DONTWAIT, MT_DATA);
			if (m == 0) {
				m_freem(top);
				return (0);
			}
			m->m_len = MLEN;
		}
		len = min(totlen, (packet_end - cp));
		if (len >= MINCLSIZE) {
			MCLGET(m, M_DONTWAIT);
			if (m->m_flags & M_EXT)
				m->m_len = len = min(len, MCLBYTES);
			else
				len = m->m_len;
		} else {
			/*
			 * Place initial small packet/header at end of mbuf.
			 */
			if (len < m->m_len) {
				if (top == 0 && len + max_linkhdr <= m->m_len)
					m->m_data += max_linkhdr;
				m->m_len = len;
			} else
				len = m->m_len;
		}
		if (flags)
			if_vba16copy(cp, mtod(m, u_char *), (u_int)len);
		else
			bcopy(cp, mtod(m, u_char *), (u_int)len);

		*mp = m;
		mp = &m->m_next;
		totlen -= len;
		cp += len;
		if (cp == packet_end)
			cp = rxbuf;
	}
	return (top);
}

if_vbaput(ifu, m0, flags)
register u_char *ifu;
register struct mbuf *m0;
{
	register struct mbuf *m = m0;
	register u_char *cp = ifu;

	while (m) {
		if (flags)
			if_vba16copy(mtod(m, u_char *), cp, m->m_len);
		else
			bcopy(mtod(m, u_char *), cp, m->m_len);
		cp += m->m_len;
		MFREE(m, m0);
		m = m0;
	}
	if ((int)cp & 1)
		*cp++ = 0;
	return (cp - ifu);
}

if_vba16copy(from, to, cnt)
	register u_char *from, *to;
	register u_int cnt;
{
	register c;
	register short *f, *t;

	if (((int)from&01) && ((int)to&01)) {
		/* source & dest at odd addresses */
		*to++ = *from++;
		--cnt;
	}
	if (cnt > 1 && (((int)to&01) == 0) && (((int)from&01) == 0)) {
		t = (short *)to;
		f = (short *)from;
		for (c = cnt>>1; c; --c)	/* even address copy */
			*t++ = *f++;
		cnt &= 1;
		if (cnt) {			/* odd len */
			from = (u_char *)f;
			to = (u_char *)t;
			*to = *from;
		}
	}
	while ((int)cnt-- > 0)	/* one of the address(es) must be odd */
		*to++ = *from++;
}
