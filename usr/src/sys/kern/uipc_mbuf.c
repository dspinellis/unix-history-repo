/*	uipc_mbuf.c	1.21	81/12/12	*/

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/pte.h"
#include "../h/cmap.h"
#include "../h/map.h"
#include "../h/mbuf.h"
#include "../net/in_systm.h"		/* XXX */
#include "../h/vm.h"

mbinit()
{

COUNT(MBINIT);
	if (m_reserve(32) == 0)
		goto bad;
	if (m_clalloc(4, MPG_MBUFS) == 0)
		goto bad;
	if (m_clalloc(32, MPG_CLUSTERS) == 0)
		goto bad;
	return;
bad:
	panic("mbinit");
}

caddr_t
m_clalloc(ncl, how)
	register int ncl;
	int how;
{
	int npg, mbx;
	register struct mbuf *m;
	register int i;
	int s;

COUNT(M_CLALLOC);
	npg = ncl * CLSIZE;
	mbx = rmalloc(mbmap, npg);
printf("ncl %d how %d npg %d mbx %d\n", ncl, how, npg, mbx);
	if (mbx == 0)
		return (0);
	m = cltom(mbx / CLSIZE);
	if (memall(&Mbmap[mbx], ncl * CLSIZE, proc, CSYS) == 0)
		return (0);
	vmaccess(&Mbmap[mbx], (caddr_t)m, npg);
printf("m %x &Mbmap[mbx] %x\n", m, &Mbmap[mbx]);
	switch (how) {

	case MPG_CLUSTERS:
		s = splimp();
		for (i = 0; i < ncl; i++) {
			m->m_off = 0;
			m->m_next = mclfree;
			mclfree = m;
			m += CLBYTES / sizeof (*m);
			nmclfree++;
		}
		mbstat.m_clusters += ncl;
		splx(s);
		break;

	case MPG_MBUFS:
		for (i = ncl * CLBYTES / sizeof (*m); i > 0; i--) {
			m->m_off = 0;
			(void) m_free(m);
			m++;
		}
		mbstat.m_clusters += ncl;
		break;
	}
	return ((caddr_t)m);
}

m_pgfree(addr, n)
	caddr_t addr;
	int n;
{

COUNT(M_PGFREE);
	printf("m_pgfree %x %d\n", addr, n);
}

m_expand()
{

COUNT(M_EXPAND);
	if (mbstat.m_bufs >= mbstat.m_hiwat)
		return (0);
	if (m_clalloc(1, MPG_MBUFS) == 0)
		goto steal;
	return (1);
steal:
	/* should ask protocols to free code */
	return (0);
}

/* NEED SOME WAY TO RELEASE SPACE */

/*
 * Space reservation routines
 */
m_reserve(mbufs)
	int mbufs;
{

	if (mbstat.m_lowat + (mbufs>>1) > (NMBCLUSTERS-32) * CLBYTES) 
		return (0);
	mbstat.m_hiwat += mbufs;
	mbstat.m_lowat = mbstat.m_hiwat >> 1;
	return (1);
}

m_release(mbufs)
	int mbufs;
{

	mbstat.m_hiwat -= mbufs;
	mbstat.m_lowat = mbstat.m_hiwat >> 1;
}

/*
 * Space allocation routines.
 * These are also available as macros
 * for critical paths.
 */
struct mbuf *
m_get(canwait)
	int canwait;
{
	register struct mbuf *m;

COUNT(M_GET);
	MGET(m, canwait);
	return (m);
}

struct mbuf *
m_getclr(canwait)
	int canwait;
{
	register struct mbuf *m;

COUNT(M_GETCLR);
	m = m_get(canwait);
	if (m == 0)
		return (0);
	m->m_off = MMINOFF;
	bzero(mtod(m, caddr_t), MLEN);
	return (m);
}

struct mbuf *
m_free(m)
	struct mbuf *m;
{
	register struct mbuf *n;

COUNT(M_FREE);
	MFREE(m, n);
	return (n);
}

/*ARGSUSED*/
struct mbuf *
m_more(type)
	int type;
{
	register struct mbuf *m;

COUNT(M_MORE);
	if (!m_expand()) {
		mbstat.m_drops++;
		return (NULL);
	}
#define m_more(x) (panic("m_more"), (struct mbuf *)0)
	MGET(m, type);
	return (m);
}

m_freem(m)
	register struct mbuf *m;
{
	register struct mbuf *n;
	register int s;

COUNT(M_FREEM);
	if (m == NULL)
		return;
	s = splimp();
	do {
		MFREE(m, n);
	} while (m = n);
	splx(s);
}

/*
 * Mbuffer utility routines.
 */
struct mbuf *
m_copy(m, off, len)
	register struct mbuf *m;
	int off;
	register int len;
{
	register struct mbuf *n, **np;
	struct mbuf *top, *p;
COUNT(M_COPY);

	if (len == 0)
		return (0);
	if (off < 0 || len < 0)
		panic("m_copy");
	while (off > 0) {
		if (m == 0)
			panic("m_copy");
		if (off < m->m_len)
			break;
		off -= m->m_len;
		m = m->m_next;
	}
	np = &top;
	top = 0;
	while (len > 0) {
		MGET(n, 1);
		*np = n;
		if (n == 0)
			goto nospace;
		if (m == 0)
			panic("m_copy");
		n->m_len = MIN(len, m->m_len - off);
		if (m->m_off > MMAXOFF) {
			p = mtod(m, struct mbuf *);
			n->m_off = ((int)p - (int)n) + off;
			mclrefcnt[mtocl(p)]++;
		} else {
			n->m_off = MMINOFF;
			bcopy(mtod(m, caddr_t)+off, mtod(n, caddr_t),
			    (unsigned)n->m_len);
		}
		len -= n->m_len;
		off = 0;
		m = m->m_next;
		np = &n->m_next;
	}
	return (top);
nospace:
	printf("m_copy: no space\n");
	m_freem(top);
	return (0);
}

m_cat(m, n)
	register struct mbuf *m, *n;
{

	while (m->m_next)
		m = m->m_next;
	while (n)
		if (m->m_off + m->m_len + n->m_len <= MMAXOFF) {
			bcopy(mtod(n, caddr_t), mtod(m, caddr_t) + m->m_len,
			    (u_int)n->m_len);
			m->m_len += n->m_len;
			n = m_free(n);
		} else {
			m->m_next = n;
			m = n;
			n = m->m_next;
		}
}

m_adj(mp, len)
	struct mbuf *mp;
	register int len;
{
	register struct mbuf *m, *n;

COUNT(M_ADJ);
	if ((m = mp) == NULL)
		return;
	if (len >= 0) {
		while (m != NULL && len > 0) {
			if (m->m_len <= len) {
				len -= m->m_len;
				m->m_len = 0;
				m = m->m_next;
			} else {
				m->m_len -= len;
				m->m_off += len;
				break;
			}
		}
	} else {
		/* a 2 pass algorithm might be better */
		len = -len;
		while (len > 0 && m->m_len != 0) {
			while (m != NULL && m->m_len != 0) {
				n = m;
				m = m->m_next;
			}
			if (n->m_len <= len) {
				len -= n->m_len;
				n->m_len = 0;
				m = mp;
			} else {
				n->m_len -= len;
				break;
			}
		}
	}
}

/*ARGSUSED*/
m_pullup(m, len)
	struct mbuf *m;
	int len;
{

	return (0);
}
