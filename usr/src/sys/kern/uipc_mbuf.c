/* mbuf.c 1.6 81/10/21 */

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/pte.h"
#include "../h/cmap.h"
#include "../h/map.h"
#include "../bbnnet/net.h"
#include "../bbnnet/mbuf.h"
#include "../bbnnet/tcp.h"
#include "../bbnnet/ip.h"
#include "../h/vm.h"

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
m_free(m)
	struct mbuf *m;
{
	register struct mbuf *n;

COUNT(M_FREE);
	MFREE(m, n);
	return (n);
}

struct mbuf *
m_more(type)
	int type;
{
	int s;
	register struct mbuf *m;

COUNT(M_MORE);
	if (!m_expand()) {
		netstat.m_drops++;
		return (NULL);
	}
#define m_more(x) ((struct mbuf *)panic("m_more"))
	MGET(m, 0);
	return (m);
}

m_freem(m)                      /* free mbuf chain headed by m */
	register struct mbuf *m;
{
	register struct mbuf *n;
	register int s, cnt;

COUNT(M_FREEM);
	if (m == NULL)
		return (0);
	cnt = 0;
	s = spl_imp();
	do {
		MFREE(m, n);
		cnt++;
	} while (m = n);
	splx(s);
	return (cnt);
}


mbufinit()                      /* init network buffer mgmt system */
{
	register struct mbuf *m;
	register i;

COUNT(MBUFINIT);
	m = (struct mbuf *)&netutl[0];  /* ->start of buffer virt mem */
	vmemall(&Netmap[0], 2, proc, CSYS);
	vmaccess(&Netmap[0], m, 2);
	for (i=0; i < NMBPG; i++) {
		m_free(m);
		m++;
	}
	pg_alloc(3);
	netcb.n_pages = 4;
	netcb.n_bufs = 32;
	netcb.n_lowat = 16;
	netcb.n_hiwat = 32;
}

pg_alloc(n)
	register int n;
{
	register i, j, k;
	register struct mbuf *m;
	int bufs, s;

COUNT(PG_ALLOC);
	k = n << 1;
	if ((i = rmalloc(netmap, n)) == 0)
		return (0);
	j = i<<1;
	m = pftom(i);
	/* should use vmemall sometimes */
	if (memall(&Netmap[j], k, proc, CSYS) == 0)
		return (0);
	vmaccess(&Netmap[j], (caddr_t)m, k);
	bufs = n << 3;
	s = spl_imp();
	for (j=0; j < bufs; j++) {
		m_free(m);
		m++;
	}
	splx(s);
	netcb.n_pages += n;
	return (1);
}

m_expand()
{
	register i;
	register struct ipq *fp;
	register struct ip *q;
	register struct tcb *tp;
	register struct mbuf *m, *n;
	int need, needp, needs;

COUNT(M_EXPAND);
	needs = need = netcb.n_hiwat - netcb.n_bufs;    /* #bufs to add */
	needp = need >> 3;                              /* #pages to add */
	if (pg_alloc(needp))
		return (1);
	for (i=0; i < needp; i++, need-=NMBPG)
		if (needp == 1 || pg_alloc(1) == 0)		/* ??? */
			goto steal;
	return (need < needs);
steal:
	fp = netcb.n_ip_tail;           /* ip reass.q */
	while (need > 0 && fp) {
		q = fp->iqx.ip_next;    /* free mbufs assoc. w/chain */
		while (q != (struct ip *)fp) {
			need -= m_freem(dtom(q));
			q = q->ip_next;
		}
		ip_freef(fp);           /* free header */
		fp = netcb.n_ip_tail;
	}
	tp = netcb.n_tcb_tail;          /* ->tcbs */
	while (need > 0 && tp != NULL) {
		m = tp->t_rcv_unack;
		while (m != NULL) {
			n = m->m_act;
			need -= m_freem(m);
			m = n;
		}
		tp->t_rcv_unack = NULL;
		tp = tp->t_tcb_prev;
	}
	return (need < needs);
}

#ifdef notdef
m_relse()
{
	int free;

COUNT(M_RELSE);
	free = (netcb.n_bufs - netcb.n_hiwat) >> 3;    /* # excess free pages */
	return;
}
#endif

struct mbuf *
m_adj(mp, len)
	struct mbuf *mp;
	register len;
{
	register struct mbuf *m, *n;

COUNT(M_ADJ);
	if ((m = mp) == NULL)
		return;
	if (len >= 0) {                 /* adjust from top of msg chain */
		while (m != NULL && len > 0) {
			if (m->m_len <= len) {          /* free this mbuf */
				len -= m->m_len;
				m->m_len = 0;
				m = m->m_next;
			} else {                        /* adjust mbuf */
				m->m_len -= len;
				m->m_off += len;
				break;
			}
		}

	} else {                        /* adjust from bottom of msg chain */
		len = -len;
		while (len > 0 && m->m_len != 0) {
			/* find end of chain */
			while (m != NULL && m->m_len != 0) {
				n = m;
				m = m->m_next;
			}
			if (n->m_len <= len) {          /* last mbuf */
				len -= n->m_len;
				n->m_len = 0;
				m = mp;
			} else {                        /* adjust length */
				n->m_len -= len;
				break;
			}
		}
	}
}

/*
 * convert mbuf virtual to physical addr for uballoc
 */
mtophys(m)
	register struct mbuf *m;
{
	register i;
	register unsigned long addr;
	register struct pte *pte;

COUNT(MTOPHYS);
	i = (((int)m & ~PGOFSET) - (int)netutl) >> PGSHIFT;
	pte = &Netmap[i];
	addr = (pte->pg_pfnum << PGSHIFT) | ((int)m & PGOFSET);
	return (addr);
}
