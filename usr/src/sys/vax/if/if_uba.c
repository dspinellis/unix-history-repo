/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_uba.c	7.8 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "map.h"
#include "buf.h"
#include "cmap.h"
#include "vmmac.h"
#include "socket.h"
#include "syslog.h"
#include "malloc.h"

#include "../net/if.h"

#include "../vax/pte.h"
#include "../vax/mtpr.h"
#include "if_uba.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/ubavar.h"

/*
 * Routines supporting UNIBUS network interfaces.
 *
 * TODO:
 *	Support interfaces using only one BDP statically.
 */

/*
 * Init UNIBUS for interface on uban whose headers of size hlen are to
 * end on a page boundary.  We allocate a UNIBUS map register for the page
 * with the header, and nmr more UNIBUS map registers for i/o on the adapter,
 * doing this once for each read and once for each write buffer.  We also
 * allocate page frames in the mbuffer pool for these pages.
 */
if_ubaminit(ifu, uban, hlen, nmr, ifr, nr, ifw, nw)
	register struct ifubinfo *ifu;
	int uban, hlen, nmr, nr, nw;
	register struct ifrw *ifr;
	register struct ifxmt *ifw;
{
	register caddr_t p;
	caddr_t cp;
	int i, nclbytes, off;

	if (hlen)
		off = CLBYTES - hlen;
	else
		off = 0;
	nclbytes = CLBYTES * (clrnd(nmr) / CLSIZE);
	if (hlen)
		nclbytes += CLBYTES;
	if (ifr[0].ifrw_addr)
		cp = ifr[0].ifrw_addr - off;
	else {
		cp = (caddr_t)malloc((nr + nw) * nclbytes, M_DEVBUF, M_NOWAIT);
		if (cp == 0)
			return (0);
		p = cp;
		for (i = 0; i < nr; i++) {
			ifr[i].ifrw_addr = p + off;
			p += nclbytes;
		}
		for (i = 0; i < nw; i++) {
			ifw[i].ifw_base = p;
			ifw[i].ifw_addr = p + off;
			p += nclbytes;
		}
		ifu->iff_hlen = hlen;
		ifu->iff_uban = uban;
		ifu->iff_uba = uba_hd[uban].uh_uba;
	}
	for (i = 0; i < nr; i++)
		if (if_ubaalloc(ifu, &ifr[i], nmr) == 0) {
			nr = i;
			nw = 0;
			goto bad;
		}
	for (i = 0; i < nw; i++)
		if (if_ubaalloc(ifu, &ifw[i].ifrw, nmr) == 0) {
			nw = i;
			goto bad;
		}
	while (--nw >= 0) {
		for (i = 0; i < nmr; i++)
			ifw[nw].ifw_wmap[i] = ifw[nw].ifw_mr[i];
		ifw[nw].ifw_xswapd = 0;
		ifw[nw].ifw_flags = IFRW_W;
		ifw[nw].ifw_nmr = nmr;
	}
	return (1);
bad:
	while (--nw >= 0)
		ubarelse(ifu->iff_uban, &ifw[nw].ifw_info);
	while (--nr >= 0)
		ubarelse(ifu->iff_uban, &ifr[nr].ifrw_info);
	free(cp, M_DEVBUF);
	ifr[0].ifrw_addr = 0;
	return (0);
}

/*
 * Setup an ifrw structure by allocating UNIBUS map registers,
 * possibly a buffered data path, and initializing the fields of
 * the ifrw structure to minimize run-time overhead.
 */
static
if_ubaalloc(ifu, ifrw, nmr)
	struct ifubinfo *ifu;
	register struct ifrw *ifrw;
	int nmr;
{
	register int info;

	info =
	    uballoc(ifu->iff_uban, ifrw->ifrw_addr, nmr*NBPG + ifu->iff_hlen,
	        ifu->iff_flags);
	if (info == 0)
		return (0);
	ifrw->ifrw_info = info;
	ifrw->ifrw_bdp = UBAI_BDP(info);
	ifrw->ifrw_proto = UBAMR_MRV | (UBAI_BDP(info) << UBAMR_DPSHIFT);
	ifrw->ifrw_mr = &ifu->iff_uba->uba_map[UBAI_MR(info) + (ifu->iff_hlen?
		1 : 0)];
	return (1);
}

/*
 * Pull read data off a interface.
 * Len is length of data, with local net header stripped.
 * Off is non-zero if a trailer protocol was used, and
 * gives the offset of the trailer information.
 * We copy the trailer information and then all the normal
 * data into mbufs.  When full cluster sized units are present
 * on the interface on cluster boundaries we can get them more
 * easily by remapping, and take advantage of this here.
 * Prepend a pointer to the interface structure,
 * so that protocols can determine where incoming packets arrived.
 * Note: we may be called to receive from a transmit buffer by some
 * devices.  In that case, we must force normal mapping of the buffer,
 * so that the correct data will appear (only unibus maps are 
 * changed when remapping the transmit buffers).
 */
struct mbuf *
if_ubaget(ifu, ifr, totlen, off0, ifp)
	struct ifubinfo *ifu;
	register struct ifrw *ifr;
	int totlen, off0;
	struct ifnet *ifp;
{
	struct mbuf *top, **mp;
	register struct mbuf *m;
	int off = off0, len;
	register caddr_t cp = ifr->ifrw_addr + ifu->iff_hlen, pp;

	top = 0;
	mp = &top;
	if (ifr->ifrw_flags & IFRW_W)
		rcv_xmtbuf((struct ifxmt *)ifr);
	while (totlen > 0) {
		MGET(m, M_DONTWAIT, MT_DATA);
		if (m == 0) {
			m_freem(top);
			top = 0;
			goto out;
		}
		if (off) {
			len = totlen - off;
			cp = ifr->ifrw_addr + ifu->iff_hlen + off;
		} else
			len = totlen;
		if (len >= CLBYTES/2) {
			struct pte *cpte, *ppte;
			int x, *ip, i;

			/*
			 * If doing the first mbuf and
			 * the interface pointer hasn't been put in,
			 * put it in a separate mbuf to preserve alignment.
			 */
			if (ifp) {
				len = 0;
				goto nopage;
			}
			MCLGET(m);
			if (m->m_len != CLBYTES)
				goto nopage;
			m->m_len = MIN(len, CLBYTES);
			if (!claligned(cp))
				goto copy;

			/*
			 * Switch pages mapped to UNIBUS with new page pp,
			 * as quick form of copy.  Remap UNIBUS and invalidate.
			 */
			pp = mtod(m, char *);
			cpte = kvtopte(cp);
			ppte = kvtopte(pp);
			x = btop(cp - ifr->ifrw_addr);
			ip = (int *)&ifr->ifrw_mr[x];
			for (i = 0; i < CLSIZE; i++) {
				struct pte t;
				t = *ppte; *ppte++ = *cpte; *cpte = t;
				*ip++ = cpte++->pg_pfnum|ifr->ifrw_proto;
				mtpr(TBIS, cp);
				cp += NBPG;
				mtpr(TBIS, (caddr_t)pp);
				pp += NBPG;
			}
			goto nocopy;
		}
nopage:
		m->m_off = MMINOFF;
		if (ifp) {
			/*
			 * Leave room for ifp.
			 */
			m->m_len = MIN(MLEN - sizeof(ifp), len);
			m->m_off += sizeof(ifp);
		} else 
			m->m_len = MIN(MLEN, len);
copy:
		bcopy(cp, mtod(m, caddr_t), (unsigned)m->m_len);
		cp += m->m_len;
nocopy:
		*mp = m;
		mp = &m->m_next;
		if (off) {
			/* sort of an ALGOL-W style for statement... */
			off += m->m_len;
			if (off == totlen) {
				cp = ifr->ifrw_addr + ifu->iff_hlen;
				off = 0;
				totlen = off0;
			}
		} else
			totlen -= m->m_len;
		if (ifp) {
			/*
			 * Prepend interface pointer to first mbuf.
			 */
			m->m_len += sizeof(ifp);
			m->m_off -= sizeof(ifp);
			*(mtod(m, struct ifnet **)) = ifp;
			ifp = (struct ifnet *)0;
		}
	}
out:
	if (ifr->ifrw_flags & IFRW_W)
		restor_xmtbuf((struct ifxmt *)ifr);
	return (top);
}

/*
 * Change the mapping on a transmit buffer so that if_ubaget may
 * receive from that buffer.  Copy data from any pages mapped to Unibus
 * into the pages mapped to normal kernel virtual memory, so that
 * they can be accessed and swapped as usual.  We take advantage
 * of the fact that clusters are placed on the xtofree list
 * in inverse order, finding the last one.
 */
static
rcv_xmtbuf(ifw)
	register struct ifxmt *ifw;
{
	register struct mbuf *m;
	struct mbuf **mprev;
	register i;
	char *cp;

	while (i = ffs((long)ifw->ifw_xswapd)) {
		cp = ifw->ifw_base + i * CLBYTES;
		i--;
		ifw->ifw_xswapd &= ~(1<<i);
		mprev = &ifw->ifw_xtofree;
		for (m = ifw->ifw_xtofree; m && m->m_next; m = m->m_next)
			mprev = &m->m_next;
		if (m == NULL)
			break;
		bcopy(mtod(m, caddr_t), cp, CLBYTES);
		(void) m_free(m);
		*mprev = NULL;
	}
	ifw->ifw_xswapd = 0;
	for (i = 0; i < ifw->ifw_nmr; i++)
		ifw->ifw_mr[i] = ifw->ifw_wmap[i];
}

/*
 * Put a transmit buffer back together after doing an if_ubaget on it,
 * which may have swapped pages.
 */
static
restor_xmtbuf(ifw)
	register struct ifxmt *ifw;
{
	register i;

	for (i = 0; i < ifw->ifw_nmr; i++)
		ifw->ifw_wmap[i] = ifw->ifw_mr[i];
}

/*
 * Map a chain of mbufs onto a network interface
 * in preparation for an i/o operation.
 * The argument chain of mbufs includes the local network
 * header which is copied to be in the mapped, aligned
 * i/o space.
 */
if_ubaput(ifu, ifw, m)
	struct ifubinfo *ifu;
	register struct ifxmt *ifw;
	register struct mbuf *m;
{
	register struct mbuf *mp;
	register caddr_t cp, dp;
	register int i;
	int xswapd = 0;
	int x, cc, t;

	cp = ifw->ifw_addr;
	while (m) {
		dp = mtod(m, char *);
		if (claligned(cp) && claligned(dp) &&
		    (m->m_len == CLBYTES || m->m_next == (struct mbuf *)0)) {
			struct pte *pte;
			int *ip;

			pte = kvtopte(dp);
			x = btop(cp - ifw->ifw_addr);
			ip = (int *)&ifw->ifw_mr[x];
			for (i = 0; i < CLSIZE; i++)
				*ip++ = ifw->ifw_proto | pte++->pg_pfnum;
			xswapd |= 1 << (x>>(CLSHIFT-PGSHIFT));
			mp = m->m_next;
			m->m_next = ifw->ifw_xtofree;
			ifw->ifw_xtofree = m;
			cp += m->m_len;
		} else {
			bcopy(mtod(m, caddr_t), cp, (unsigned)m->m_len);
			cp += m->m_len;
			MFREE(m, mp);
		}
		m = mp;
	}

	/*
	 * Xswapd is the set of clusters we just mapped out.  Ifu->iff_xswapd
	 * is the set of clusters mapped out from before.  We compute
	 * the number of clusters involved in this operation in x.
	 * Clusters mapped out before and involved in this operation
	 * should be unmapped so original pages will be accessed by the device.
	 */
	cc = cp - ifw->ifw_addr;
	x = ((cc - ifu->iff_hlen) + CLBYTES - 1) >> CLSHIFT;
	ifw->ifw_xswapd &= ~xswapd;
	while (i = ffs((long)ifw->ifw_xswapd)) {
		i--;
		if (i >= x)
			break;
		ifw->ifw_xswapd &= ~(1<<i);
		i *= CLSIZE;
		for (t = 0; t < CLSIZE; t++) {
			ifw->ifw_mr[i] = ifw->ifw_wmap[i];
			i++;
		}
	}
	ifw->ifw_xswapd |= xswapd;
	return (cc);
}
