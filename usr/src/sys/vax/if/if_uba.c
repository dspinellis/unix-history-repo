/*	if_uba.c	4.5	81/12/09	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/ubareg.h"
#include "../h/ubavar.h"
#include "../h/cmap.h"
#include "../h/mtpr.h"
#include "../h/vmmac.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/if_uba.h"

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
 * doing this twice: once for reading and once for writing.  We also
 * allocate page frames in the mbuffer pool for these pages.
 */
if_ubainit(ifu, uban, hlen, nmr)
	register struct ifuba *ifu;
	int uban, hlen, nmr;
{
	register caddr_t cp;
	int i, ncl;

COUNT(IF_UBAINIT);
	ncl = clrnd(nmr + CLSIZE) / CLSIZE;
	cp = m_clalloc(2 * ncl, MPG_SPACE);
	if (cp == 0)
		return (0);
	ifu->ifu_hlen = hlen;
	ifu->ifu_uban = uban;
	ifu->ifu_uba = uba_hd[uban].uh_uba;
	ifu->ifu_r.ifrw_addr = cp + CLBYTES - hlen;
	ifu->ifu_w.ifrw_addr = ifu->ifu_r.ifrw_addr + ncl * CLBYTES;
	if (if_ubaalloc(ifu, &ifu->ifu_r, nmr) == 0)
		goto bad;
	if (if_ubaalloc(ifu, &ifu->ifu_w, nmr) == 0)
		goto bad2;
	for (i = 0; i < nmr; i++)
		ifu->ifu_wmap[i] = ifu->ifu_w.ifrw_mr[i];
	ifu->ifu_xswapd = 0;
	return (1);
bad2:
	ubarelse(ifu->ifu_uban, &ifu->ifu_r.ifrw_info);
bad:
	m_pgfree(cp, 2 * ncl);
	return (0);
}

/*
 * Setup either a ifrw structure by allocating UNIBUS map registers,
 * a buffered data path, and initializing the fields of the ifrw structure
 * to minimize run-time overhead.
 */
static
if_ubaalloc(ifu, ifrw, nmr)
	struct ifuba *ifu;
	register struct ifrw *ifrw;
	int nmr;
{
	register int info;

COUNT(IF_UBAALLOC);
	info =
	    uballoc(ifu->ifu_uban, ifrw->ifrw_addr, nmr*NBPG + ifu->ifu_hlen,
	        UBA_NEED16|UBA_NEEDBDP);
	if (info == 0)
		return (0);
	ifrw->ifrw_info = info;
	ifrw->ifrw_bdp = UBAI_BDP(info);
	ifrw->ifrw_proto = UBAMR_MRV | (UBAI_BDP(info) << UBAMR_DPSHIFT);
	ifrw->ifrw_mr = &ifu->ifu_uba->uba_map[UBAI_MR(info) + 1];
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
 */
struct mbuf *
if_rubaget(ifu, totlen, off0)
	register struct ifuba *ifu;
	int totlen, off0;
{
	struct mbuf *top, **mp, *m;
	int off = off0, len;
	register caddr_t cp = ifu->ifu_r.ifrw_addr + ifu->ifu_hlen;

COUNT(IF_RUBAGET);

printf("if_rubaget totlen %d off0 %d cp %x\n", totlen, off0, cp);
	top = 0;
	mp = &top;
	while (totlen > 0) {
		MGET(m, 0);
		if (m == 0)
			goto bad;
		if (off) {
			len = totlen - off;
			cp = ifu->ifu_r.ifrw_addr + ifu->ifu_hlen + off;
		} else
			len = totlen;
printf("m %x len %d off %d cp %x\n", m, len, off, cp);
		if (len >= CLBYTES) {
			struct mbuf *p;
			struct pte *cpte, *ppte;
			int x, *ip, i;

			MCLGET(p, 1);
			if (p == 0)
				goto nopage;
			len = m->m_len = CLBYTES;
			m->m_off = (int)p - (int)m;
			if (!claligned(cp))
				goto copy;

			/*
			 * Switch pages mapped to UNIBUS with new page p,
			 * as quick form of copy.  Remap UNIBUS and invalidate.
			 */
			cpte = &Mbmap[mtocl(cp)*CLSIZE];
			ppte = &Mbmap[mtocl(p)*CLSIZE];
			x = btop(cp - ifu->ifu_r.ifrw_addr);
			ip = (int *)&ifu->ifu_r.ifrw_mr[x];
			for (i = 0; i < CLSIZE; i++) {
				struct pte t;
				t = *ppte; *ppte++ = *cpte; *cpte = t;
				*ip++ =
				    cpte++->pg_pfnum|ifu->ifu_r.ifrw_proto;
				mtpr(TBIS, cp);
				cp += NBPG;
				mtpr(TBIS, (caddr_t)p);
				p += NBPG / sizeof (*p);
			}
			goto nocopy;
		}
nopage:
		m->m_len = MIN(MLEN, len);
		m->m_off = MMINOFF;
printf("nopage m->m_len %d, m %x\n", m->m_len, len);
copy:
printf("copy %d from %x to %x", m->m_len, cp, mtod(m, caddr_t));
		bcopy(cp, mtod(m, caddr_t), (unsigned)m->m_len);
		cp += m->m_len;
nocopy:
		*mp = m;
		mp = &m->m_next;
		if (off) {
			/* sort of an ALGOL-W style for statement... */
			off += m->m_len;
			if (off == totlen) {
				cp = ifu->ifu_r.ifrw_addr + ifu->ifu_hlen;
				off = 0;
				totlen = off0;
			}
		} else
			totlen -= len;
	}
	return (top);
bad:
	m_freem(top);
	return (0);
}

/*
 * Map a chain of mbufs onto a network interface
 * in preparation for an i/o operation.
 * The argument chain of mbufs includes the local network
 * header which is copied to be in the mapped, aligned
 * i/o space.
 */
if_wubaput(ifu, m)
	register struct ifuba *ifu;
	register struct mbuf *m;
{
	register struct mbuf *mp;
	register caddr_t cp, dp;
	register int i;
	int xswapd = 0;
	int x, cc;

COUNT(IF_WUBAPUT);
	ifu->ifu_xswapd = 0;
	cp = ifu->ifu_w.ifrw_addr;
	while (m) {
		dp = mtod(m, char *);
		if (claligned(cp) && claligned(dp) && m->m_len == CLBYTES) {
			struct pte *pte; int *ip;
			pte = &Mbmap[mtocl(dp)*CLSIZE];
			x = btop(cp - ifu->ifu_w.ifrw_addr);
			ip = (int *)&ifu->ifu_w.ifrw_mr[x];
			for (i = 0; i < CLSIZE; i++)
				*ip++ =
				    ifu->ifu_w.ifrw_proto | pte++->pg_pfnum;
			xswapd |= 1 << (x>>(CLSHIFT-PGSHIFT));
			mp = m->m_next;
			m->m_next = ifu->ifu_xtofree;
			ifu->ifu_xtofree = m;
			cp += m->m_len;
		} else {
			bcopy(mtod(m, caddr_t), cp, (unsigned)m->m_len);
			cp += m->m_len;
			MFREE(m, mp);
		}
		m = mp;
	}

	/*
	 * Xswapd is the set of clusters we just mapped out.  Ifu->ifu_xswapd
	 * is the set of clusters mapped out from before.  We compute
	 * the number of clusters involved in this operation in x.
	 * Clusters mapped out before and involved in this operation
	 * should be unmapped so original pages will be accessed by the device.
	 */
	cc = cp - ifu->ifu_w.ifrw_addr;
	x = ((cc - ifu->ifu_hlen) + CLBYTES - 1) >> CLSHIFT;
	ifu->ifu_xswapd &= ~xswapd;
	xswapd &= ~ifu->ifu_xswapd;
	while (i = ffs(ifu->ifu_xswapd)) {
		i--;
		if (i >= x)
			break;
		ifu->ifu_xswapd &= ~(1<<i);
		i *= CLSIZE;
		for (x = 0; x < CLSIZE; x++) {
			ifu->ifu_w.ifrw_mr[i] = ifu->ifu_wmap[i];
			i++;
		}
	}
	ifu->ifu_xswapd |= xswapd;
	return (cc);
}
