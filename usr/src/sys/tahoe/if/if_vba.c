/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_vba.c	1.4 (Berkeley) %G%
 */

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/mbuf.h"
#include "sys/buf.h"
#include "sys/cmap.h"
#include "sys/vmmac.h"
#include "sys/socket.h"

#include "../include/mtpr.h"
#include "../include/pte.h"

#include "../vba/vbavar.h"

#include "net/if.h"
#include "netinet/in.h"
#include "netinet/if_ether.h"

#include "if_vba.h"

if_vbareserve(ifvba0, n, bufsize, extra, extrasize)
struct ifvba *ifvba0;
register int n;
int bufsize;
caddr_t *extra;
int extrasize;
{
	register caddr_t cp;
	register struct pte *pte;
	register struct ifvba *ifvba = ifvba0;
	struct ifvba *vlim  = ifvba + n;

	n = roundup(extrasize + (n * bufsize), NBPG);
	cp = (caddr_t)malloc((u_long)n, M_DEVBUF, M_NOWAIT);
	if ((n + kvtophys(cp)) > VB_MAXADDR24) {
		free(cp, M_DEVBUF);
		cp = 0;
	}
	if (cp == 0) {
		printf("No memory for device buffer(s)\n");
		return (0);
	}
	/*
	 * Make raw buffer pages uncacheable.
	 */
	pte = kvtopte(cp);
	for (n = btoc(n); n--; pte++)
		pte->pg_nc = 1;
	mtpr(TBIA, 0);
	if (extra) {
		*extra = cp;
		cp += extrasize;
	}
	for (; ifvba < vlim; ifvba++) {
		ifvba->iff_buffer = cp;
		ifvba->iff_physaddr = kvtophys(cp);
		cp += bufsize;
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
	caddr_t rxbuf;
	int totlen, off, flags;
	struct ifnet *ifp;
{
	register caddr_t cp;
	register struct mbuf *m;
	struct mbuf *top = 0, **mp = &top;
	int len;
	caddr_t packet_end;

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
			if_vba16copy(cp, mtod(m, caddr_t), (u_int)len);
		else
			bcopy(cp, mtod(m, caddr_t), (u_int)len);

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
caddr_t ifu;
struct mbuf *m0;
{
	register struct mbuf *m = m0;
	register caddr_t cp = ifu;

	while (m) {
		if (flags)
			if_vba16copy(mtod(m, caddr_t), cp, (u_int)m->m_len);
		else
			bcopy(mtod(m, caddr_t), cp, (u_int)m->m_len);
		cp += m->m_len;
		MFREE(m, m0);
		m = m0;
	}
	if ((int)cp & 1)
		*cp++ = 0;
	return (cp - ifu);
}

if_vba16copy(from, to, cnt)
	register caddr_t from, to;
	register unsigned cnt;
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
			from = (caddr_t)f;
			to = (caddr_t)t;
			*to = *from;
		}
	}
	while ((int)cnt-- > 0)	/* one of the address(es) must be odd */
		*to++ = *from++;
}
