/*	if_imphost.c	4.3	82/02/16	*/

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
#include "../net/if_imp.h"
#include "../net/if_imphost.h"

/*
 * Head of host table hash chains.
 */
struct mbuf *hosts;

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
	for (m = hosts; m; m = m->m_next) {
		hp = &mtod(m, struct hmbuf *)->hm_hosts[hash];
		if (hp->h_refcnt == 0)
			continue;
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
	register struct mbuf *m, **mprev;
	register struct host *hp, *hp0 = 0;
	register int hash = HOSTHASH(addr);

COUNT(HOSTENTER);
	mprev = &hosts;
	while (m = *mprev) {
		hp = &mtod(m, struct hmbuf *)->hm_hosts[hash];
		if (hp->h_refcnt == 0) {
			if (hp0 == 0)
				hp0 = hp;
			continue;
		}
	        if (hp->h_addr.s_addr == addr.s_addr)    
			goto foundhost;
		mprev = &m->m_next;
	}

	/*
	 * No current host structure, make one.
	 * If our search ran off the end of the
	 * chain of mbuf's, allocate another.
	 */
	if (hp0 == 0) {
		m = m_getclr(M_DONTWAIT);
		if (m == 0)
			return (0);
		*mprev = m;
		m->m_off = MMINOFF;
		hp0 = &mtod(m, struct hmbuf *)->hm_hosts[hash];
	}
	mtod(dtom(hp0), struct hmbuf *)->hm_count++;
	hp = hp0;
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
hostfree(hp)                               
	register struct host *hp;
{
	register struct mbuf *m;

COUNT(HOSTFREE);
	if (--hp->h_refcnt)
		return;
	hostrelease(hp);
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
	struct hmbuf *hm;
	int x;

COUNT(HOSTRESET);
	x = splimp();
	for (m = hosts; m; m = m->m_next) {
		hm = mtod(m, struct hmbuf *);
		hp = hm->hm_hosts; 
		lp = hp + HPMBUF;
		while (hm->hm_count != 0 && hp < lp) {
			if (hp->h_addr.s_net == net)
				hostrelease(mtod(m, struct hmbuf *), hp);
			hp++;
		}
	}
	splx(x);
}

/*
 * Remove a host structure and release
 * any resources it's accumulated.
 */
hostrelease(hp)
	register struct host *hp;
{
	register struct mbuf *m, **mprev, *mh = dtom(hp);

COUNT(HOSTRELEASE);
	/*
	 * Discard any packets left on the waiting q
	 */
	if (m = hp->h_q) {
		m = m->m_next;
		hp->h_q->m_next = 0;
		hp->h_q = 0;
		m_freem(m);
	}
	if (--mtod(mh, struct hmbuf *)->hm_count)
		return;
	mprev = &hosts;
	while ((m = *mprev) != mh)
		mprev = &m->m_next;
	*mprev = mh->m_next;
	(void) m_free(mh);
}
