/*	if_uba.c	4.3	81/11/29	*/

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
	register caddr_t cp = (caddr_t)m_pgalloc(2 * (nmr + 1));
	int i;

COUNT(IF_UBAINIT);
	if (cp == 0)
		return (0);
	ifu->ifu_uban = uban;
	ifu->ifu_uba = uba_hd[uban].uh_uba;
	ifu->ifu_r.ifrw_addr = cp + NBPG - hlen;
	ifu->ifu_w.ifrw_addr = ifu->ifu_r.ifrw_addr + (nmr + 1) * NBPG;
	if (if_ubaalloc(ifu, &ifu->ifu_r) == 0)
		goto bad;
	if (if_ubaalloc(ifu, &ifu->ifu_w) == 0)
		goto bad2;
	for (i = 0; i < IF_NUBAMR; i++)
		ifu->ifu_wmap[i] = ifu->ifu_w.ifrw_mr[i+1];
	ifu->ifu_xswapd = 0;
	return (1);
bad2:
	ubarelse(ifu->ifu_uban, &ifu->ifu_r.ifrw_info);
bad:
	m_pgfree(cp, 2 * (nmr + 1));
	return (0);
}

/*
 * Setup either a ifrw structure by allocating UNIBUS map registers,
 * a buffered data path, and initializing the fields of the ifrw structure
 * to minimize run-time overhead.
 */
static
if_ubaalloc(ifu, ifrw)
	struct ifuba *ifu;
	register struct ifrw *ifrw;
{
	register int info;

COUNT(IF_UBAALLOC);
	info =
	    uballoc(ifu->ifu_uban, ifrw->ifrw_addr, IF_NUBAMR*NBPG + ifu->ifu_hlen,
	        UBA_NEED16|UBA_NEEDBDP);
	if (info == 0)
		return (0);
	ifrw->ifrw_info = info;
	ifrw->ifrw_bdp = UBAI_BDP(info);
	ifrw->ifrw_proto = UBAMR_MRV | (UBAI_MR(info) << UBAMR_DPSHIFT);
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
	register struct mbuf *m;
	register caddr_t cp;
	struct mbuf **mp, *p, *top;
	int len, off = off0;

COUNT(IF_RUBAGET);

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
		if (len >= CLSIZE) {
			struct pte *cpte, *ppte;
			int i, x, *ip;

			MCLGET(p, 1);
			if (p == 0)
				goto nopage;
			m->m_len = CLSIZE;
			m->m_off = (int)p - (int)m;
			if ((int)cp & CLOFF)
				goto copy;

			/*
			 * Cluster size data on cluster size boundary.
			 * Input by remapping newly allocated pages to
			 * UNIBUS, and taking pages with data already
			 * in them.
			 *
			 * Cpte is the pte of the virtual memory which
			 * is mapped to the UNIBUS, and ppte is the pte
			 * for the fresh pages.  We switch the memory
			 * copies of these pte's, to make the allocated
			 * virtual memory contain the data (using the old
			 * physical pages).  We have to rewrite
			 * the UNIBUS map so that the newly allocated
			 * pages will be used for the next UNIBUS read,
			 * and invalidate the kernel translations
			 * for the virtual addresses of the pages
			 * we are flipping.
			 *
			 * The idea here is that this is supposed
			 * to take less time than copying the data.
			 */
			cpte = &Mbmap[mtocl(cp)];
			ppte = &Mbmap[mtocl(p)];
			x = btop(cp - ifu->ifu_r.ifrw_addr);
			ip = (int *)&ifu->ifu_r.ifrw_mr[x+1];
			for (i = 0; i < CLSIZE; i++) {
				struct pte t;
				t = *ppte; *ppte = *cpte; *cpte = t;
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
copy:
		bcopy(cp, mtod(m, caddr_t), (unsigned)m->m_len);
		cp += m->m_len;
nocopy:
		*mp = m;
		mp = &m->m_next;
		if (off) {
			off += m->m_len;
			if (off == totlen) {
				cp = ifu->ifu_r.ifrw_addr + ifu->ifu_hlen;
				off = 0;
				totlen -= off0;
			}
		}
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
	int xswapd = ifu->ifu_xswapd;
	int x;

COUNT(IF_WUBAPUT);
	ifu->ifu_xswapd = 0;
	cp = ifu->ifu_w.ifrw_addr;
	while (m) {
		dp = mtod(m, char *);
		if (claligned(cp) && claligned(dp)) {
			struct pte *pte; int *ip;
			pte = &Mbmap[mtocl(dp)];
			x = btop(cp - ifu->ifu_w.ifrw_addr);
			ip = (int *)&ifu->ifu_w.ifrw_mr[x + 1];
			for (i = 0; i < CLSIZE; i++)
				*ip++ =
				    ifu->ifu_w.ifrw_proto | pte++->pg_pfnum;
			ifu->ifu_xswapd |= 1 << (x>>CLSHIFT);
		} else
			bcopy(mtod(m, caddr_t), cp, (unsigned)m->m_len);
		cp += m->m_len;
		MFREE(m, mp);			/* XXX too soon! */
		m = mp;
	}
	xswapd &= ~ifu->ifu_xswapd;
	if (xswapd)
		while (i = ffs(xswapd)) {
			i--;
			xswapd &= ~(1<<i);
			i <<= CLSHIFT;
			for (x = 0; x < CLSIZE; x++) {
				ifu->ifu_w.ifrw_mr[i] = ifu->ifu_wmap[i];
				i++;
			}
		}
	return (cp - ifu->ifu_w.ifrw_addr);
}
