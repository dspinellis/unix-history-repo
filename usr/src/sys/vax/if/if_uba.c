/*	if_uba.c	4.1	81/11/25	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/ubareg.h"
#include "../h/ubavar.h"
#include "../h/cmap.h"
#include "../h/mtpr.h"
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
	register caddr_t cp = m_pgalloc(2 * (nmr + 1));

	if (cp == 0)
		return (0);
	ifu->if_uban = uban;
	ifu->if_uba = &uba_hd[uban]->uh_uba;
	ifu->if_r.if_addr = cp + NMBPG - hlen;
	ifu->if_w.if_addr = ifu->if_r.if_addr + (nmr + 1) * NMBPG;
	if (if_ubaalloc(ifu, &ifu->if_r) == 0)
		goto bad;
	if (if_ubaalloc(ifu, &ifu->if_w) == 0)
		goto bad2;
	for (i = 0; i < IF_NUBAMR; i++)
		ifu->if_xmap[i] = ifu->if_w.if_map[i+1];
	ifu->if_xswapd = 0;
	return (1);
bad2:
	ubafree(ifu->ifu_uban, ifu->if_r.ifrw_info);
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

	info =
	    uballoc(ifu->ifu_uban, ifrw->ifrw_addr, IF_NUBAMR*NMBPG + hlen,
	        UBA_NEED16|UBA_NEEDBDP);
	if (info == 0)
		goto bad;
	ifrw->ifrw_info = info;
	ifrw->ifrw_bdp = UBAI_BDP(info);
	ifrw->ifrw_proto = UBAMR_MRV | UBAI_DPDF(info);
	ifrw->ifrw_mr = &ifu->if_uba[UBAI_MR(info) + 1];
}

/*
 * Pull read data off a interface, given length.
 * Map the header into a mbuf, and then copy or
 * remap the data into a chain of mbufs.
 * Return 0 if there is no space, or a pointer
 * to the assembled mbuf chain.
 */
struct mbuf *
if_rubaget(ifu, len)
	register struct ifuba *ifu;
	int len;
{
	register struct mbuf *m;
	register caddr_t cp;
	struct mbuf *mp, *p, *top;

	/*
	 * First pull local net header off into a mbuf.
	 */
	MGET(m, 0);
	if (m == 0)
		return (0);
	m->m_off = MMINOFF;
	m->m_len = ifu->if_hlen;
	top = m;
	cp = ifu->ifu_r.ifrw_addr;
	bcopy(cp, mtod(m, caddr_t), ifu->if_hlen);
	len -= hlen;
	cp += hlen;

	/*
	 * Now pull data off.  If whole pages
	 * are there, pull into pages if possible,
	 * otherwise copy small blocks into mbufs.
	 */
	mp = m;
	while (len > 0) {
		MGET(m, 0);
		if (m == 0)
			goto flush;
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
			x = btop(cp - ifu->if_r.ifrw_addr);
			ip = (int *)&ifu->ifu_r.ifrw_mr[x+1];
			for (i = 0; i < CLSIZE; i++) {
				struct pte t;
				t = *ppte; *ppte = *cpte; *cpte = t;
				*ip++ =
				    *cpte++->pg_pfnum|ifu->if_r.ifrw_proto;
				mtpr(TBIS, cp);
				cp += NMBPG;
				mtpr(TBIS, (caddr_t)p);
				p += NMBPG / sizeof (*p);
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
		len -= m->m_len;
		mp->m_next = m;
		mp = m;
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

	ifu->ifu_xswapd = 0;
	cp = ifu->ifu_w.ifrw_addr;
	while (m) {
		dp = mtod(m, char *);
		if (claligned(cp) && claligned(dp)) {
			struct pte *pte; int *ip;
			pte = &Mbmap[mtocl(dp)];
			x = btop(cp - ifu->ifu_w.ifrw_addr);
			ip = &ifu->ifu_w.ifrw_mr[x + 1];
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
				ifu->ifu_rw.ifrw_mr[i] = ifu->ifu_xmap[i];
				i++;
			}
		}
}
