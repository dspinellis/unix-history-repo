/*	if_imphost.c	4.2	82/02/12	*/

#include "imp.h"
#if NIMP > 0
/*
 * Host table manipulation routines.
 * Only needed when shipping stuff through an IMP.
 */

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/host.h"
#include "../net/if_imp.h"

/*
 * Head of host table hash chains.
 */
struct mbuf hosttable = { 0, MMINOFF };

/*
 * Given an internet address
 * return a host structure (if it exists).
 */
struct host *
hostlookup(addr)
	struct in_addr addr;
{
	register struct host *hp;
	register struct mbuf *m;
	register int hash = HOSTHASH(addr);

COUNT(HOSTLOOKUP);
printf("hostlookup(%x)\n", addr);
	for (m = &hosttable; m; m = m->m_next) {
		hp = &mtod(m, struct hmbuf *)->hm_hosts[hash];
		if (hp->h_refcnt == 0)
			break;
printf("hostlookup: addr=%x\n", hp->h_addr.s_addr);
	        if (hp->h_addr.s_addr == addr.s_addr)    
			return (hp);
	}
	return (0);
}

/*
 * Enter a reference to this host's internet
 * address.  If no host structure exists, create
 * one and hook it into the host database.
 */
struct host *
hostenter(addr)                 
	struct in_addr addr;
{
	register struct mbuf *m, *mprev;
	register struct host *hp;
	register int hash = HOSTHASH(addr);

COUNT(HOSTENTER);
printf("hostenter(%x)\n", addr);
	for (m = &hosttable; m; mprev = m, m = m->m_next) {
		hp = &mtod(m, struct hmbuf *)->hm_hosts[hash];
		if (hp->h_refcnt == 0)
			break;
printf("hostenter: addr=%x\n", addr);
	        if (hp->h_addr.s_addr == addr.s_addr)    
			goto foundhost;
	}

	/*
	 * No current host structure, make one.
	 * If our search ran off the end of the
	 * chain of mbuf's, allocate another.
	 */
printf("hostenter: new host\n");
	if (m == 0) {
		m = m_getclr(M_DONTWAIT);
		if (m == 0)
			return (0);
		mprev->m_next = m;
		m->m_act = mprev;
		hp = &mtod(m, struct hmbuf *)->hm_hosts[hash];
	}
	mtod(m, struct hmbuf *)->hm_count++;
	hp->h_addr = addr;
	hp->h_status = HOSTS_UP;

foundhost:
	hp->h_refcnt++;		/* know new structures have 0 val */
	return (hp);
}

/*
 * Free a reference to a host.  If this causes the
 * host structure to be released do so.
 */
hostfree(addr)                               
	struct in_addr addr;
{
	register struct mbuf *m;
	register struct host *hp;
	register int hash = HOSTHASH(addr);

COUNT(HOSTFREE);
printf("hostfree(%x)\n", addr);
	for (m = &hosttable; m; m = m->m_next) {
		hp = &mtod(m, struct hmbuf *)->hm_hosts[hash];
		if (hp->h_refcnt == 0)
			return;
	        if (hp->h_addr.s_addr == addr.s_addr) {
			if (--hp->h_refcnt == 0)
				hostrelease(mtod(m, struct hmbuf *), hp);
			return;
		}
	}
	panic("hostfree");
}

/*
 * Reset a given network's host entries.
 * This involves clearing all packet queue's
 * and releasing host structures.
 */
hostreset(net)	    
	int net;
{
	register struct mbuf *m;
	register struct host *hp, *lp;

COUNT(HOSTRESET);
printf("hostreset(%x)\n", net);
	for (m = &hosttable; m; m = m->m_next) {
		hp = mtod(m, struct hmbuf *)->hm_hosts; 
		lp = hp + HPMBUF;
		while (hp < lp) {
			if (hp->h_addr.s_net == net)
				hostrelease(mtod(m, struct hmbuf *), hp);
			hp++;
		}
	}
}

/*
 * Remove a host structure and release
 * any resources it's accumulated.
 */
hostrelease(hm, hp)
	struct hmbuf *hm;
	register struct host *hp;
{
	register struct mbuf *m;

COUNT(HOSTRELEASE);
printf("hostrelease(%x,%x)\n", hm, hp);
	/*
	 * Discard any packets left on the waiting q
	 */
	if (m = hp->h_q) {
		m = m->m_next;
		hp->h_q->m_next = 0;
		hp->h_q = 0;
		m_freem(m);
	}
	/*
	 * We could compact the database here, but is
	 * it worth it?  For now we assume not and just
	 * handle the simple case.
	 */
printf("hostrelease: count=%d\n", hm->hm_count);
	if (--hm->hm_count || (m = dtom(hm)) == &hosttable)
		return;
	m->m_act->m_next = m->m_next;
	m->m_next->m_act = m->m_act;
	(void) m_free(m);
}
