/*	uipc_mbuf.c	1.10	81/11/16	*/

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/pte.h"
#include "../h/cmap.h"
#include "../h/map.h"
#include "../h/mbuf.h"
#include "../net/inet_systm.h"		/* XXX */
#include "../h/vm.h"

m_reserve(mbufs)
	int mbufs;
{

	if (mbstat.m_lowat + mbufs > NMBPAGES * NMBPG - 32) 
		return (0);
	mbstat.m_lowat += mbufs;
	mbstat.m_hiwat = 2 * mbstat.m_lowat;
	return (1);
}

m_release(mbufs)
	int mbufs;
{

	mbstat.m_lowat -= mbufs;
	mbstat.m_hiwat = 2 * mbstat.m_lowat;
}

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
	register int s, i;

COUNT(M_FREEM);
	if (m == NULL)
		return;
	i = 0;
	s = splimp();
	do {
		if (m->m_off > MMAXOFF)
			i += NMBPG;
		i++;
		MFREE(m, n);
	} while (m = n);
	splx(s);
	return;
}

mbinit()
{
	register struct mbuf *m;
	register i;

COUNT(MBUFINIT);
	m = (struct mbuf *)&mbutl[0];  /* ->start of buffer virt mem */
	(void) vmemall(&Mbmap[0], 2, proc, CSYS);
	vmaccess(&Mbmap[0], (caddr_t)m, 2);
	for (i=0; i < NMBPG; i++) {
		m->m_off = 0;
		m_free(m);
		m++;
	}
	(void) pg_alloc(3);
	mbstat.m_pages = 4;
	mbstat.m_bufs = 32;
	mbstat.m_lowat = 16;
	mbstat.m_hiwat = 32;
	{ int j,k,n;
	n = 32;
	k = n << 1;
	if ((i = rmalloc(mbmap, n)) == 0)
		panic("mbinit");
	j = i<<1;
	m = pftom(i);
	/* should use vmemall sometimes */
	if (memall(&Mbmap[j], k, proc, CSYS) == 0) {
		printf("botch\n");
		return;
	}
	vmaccess(&Mbmap[j], (caddr_t)m, k);
	for (j=0; j < n; j++) {
		m->m_off = 0;
		m->m_next = mpfree;
		mpfree = m;
		m += NMBPG;
		nmpfree++;
	}
	}
}

pg_alloc(n)
	register int n;
{
	register i, j, k;
	register struct mbuf *m;
	int bufs, s;

COUNT(PG_ALLOC);
	k = n << 1;
	if ((i = rmalloc(mbmap, n)) == 0)
		return (0);
	j = i<<1;
	m = pftom(i);
	/* should use vmemall sometimes */
	if (memall(&Mbmap[j], k, proc, CSYS) == 0)
		return (0);
	vmaccess(&Mbmap[j], (caddr_t)m, k);
	bufs = n << 3;
	s = splimp();
	for (j=0; j < bufs; j++) {
		m->m_off = 0;
		m_free(m);
		m++;
	}
	splx(s);
	mbstat.m_pages += n;
	return (1);
}

m_expand()
{
	register i;
	int need, needp, needs;

COUNT(M_EXPAND);
	needs = need = mbstat.m_hiwat - mbstat.m_bufs;
	needp = need >> 3;
	if (pg_alloc(needp))
		return (1);
	for (i=0; i < needp; i++, need -= NMBPG)
		if (pg_alloc(1) == 0)
			goto steal;
	return (need < needs);
steal:
	/* while (not enough) ask protocols to free code */
	;
	return (0);
}

#ifdef notdef
m_relse()
{

COUNT(M_RELSE);
}
#endif

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
