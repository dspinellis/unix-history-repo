/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_imphost.c	6.4 (Berkeley) %G%
 */

#include "imp.h"
#if NIMP > 0
/*
 * Host table manipulation routines.
 * Only needed when shipping stuff through an IMP.
 *
 * Everything in here is called at splimp from
 * from the IMP protocol code (if_imp.c), or
 * interlocks with the code at splimp.
 */
#include "param.h"
#include "mbuf.h"

#include "../netinet/in.h"
#include "../netinet/in_systm.h"

#include "if_imp.h"
#include "if_imphost.h"

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

	for (m = hosts; m; m = m->m_next) {
		hp = &mtod(m, struct hmbuf *)->hm_hosts[hash];
	        if (hp->h_addr.s_addr == addr.s_addr) {
			hp->h_flags |= HF_INUSE;
			return (hp);
		}
	}
	return ((struct host *)0);
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

	mprev = &hosts;
	while (m = *mprev) {
		mprev = &m->m_next;
		hp = &mtod(m, struct hmbuf *)->hm_hosts[hash];
		if ((hp->h_flags & HF_INUSE) == 0) {
			if (hp->h_addr.s_addr == addr.s_addr)
				goto foundhost;
			if (hp0 == 0)
				hp0 = hp;
			continue;
		}
	        if (hp->h_addr.s_addr == addr.s_addr)    
			goto foundhost;
	}

	/*
	 * No current host structure, make one.
	 * If our search ran off the end of the
	 * chain of mbuf's, allocate another.
	 */
	if (hp0 == 0) {
		m = m_getclr(M_DONTWAIT, MT_HTABLE);
		if (m == NULL)
			return ((struct host *)0);
		*mprev = m;
		hp0 = &mtod(m, struct hmbuf *)->hm_hosts[hash];
	}
	mtod(dtom(hp0), struct hmbuf *)->hm_count++;
	hp = hp0;
	hp->h_addr = addr;
	hp->h_timer = 0;
	hp->h_flags = 0;

foundhost:
	hp->h_flags |= HF_INUSE;
	return (hp);
}

/*
 * Mark a host structure free and set it's
 * timer going.
 */
hostfree(hp)                               
	register struct host *hp;
{

	hp->h_flags &= ~HF_INUSE;
	hp->h_timer = HOSTTIMER;
	hp->h_rfnm = 0;
}

/*
 * Reset a given network's host entries.
 */
hostreset(net)	    
	long net;
{
	register struct mbuf *m;
	register struct host *hp, *lp;
	struct hmbuf *hm;

	for (m = hosts; m; m = m->m_next) {
		hm = mtod(m, struct hmbuf *);
		hp = hm->hm_hosts; 
		lp = hp + HPMBUF;
		while (hm->hm_count > 0 && hp < lp) {
			if (in_netof(hp->h_addr) == net) {
				hp->h_flags &= ~HF_INUSE;
				hostrelease(hp);
			}
			hp++;
		}
	}
}

/*
 * Remove a host structure and release
 * any resources it's accumulated.
 */
hostrelease(hp)
	register struct host *hp;
{
	register struct mbuf *m, **mprev, *mh = dtom(hp);

	/*
	 * Discard any packets left on the waiting q
	 */
	if (m = hp->h_q) {
		register struct mbuf *n;

		do {
			n = m->m_act;
			m_freem(m);
			m = n;
		} while (m != hp->h_q);
		hp->h_q = 0;
	}
	hp->h_flags = 0;
	hp->h_rfnm = 0;
	if (--mtod(mh, struct hmbuf *)->hm_count)
		return;
	mprev = &hosts;
	while ((m = *mprev) != mh)
		mprev = &m->m_next;
	*mprev = m_free(mh);
}

/*
 * Remove a packet from the holding q.
 * The RFNM counter is also bumped.
 */
struct mbuf *
hostdeque(hp)
	register struct host *hp;
{
	register struct mbuf *m;

	hp->h_rfnm--;
	HOST_DEQUE(hp, m);
	if (m)
		return (m);
	if (hp->h_rfnm == 0)
		hostfree(hp);
	return (0);
}

/*
 * Host data base timer routine.
 * Decrement timers on structures which are
 * waiting to be deallocated.  On expiration
 * release resources, possibly deallocating
 * mbuf associated with structure.
 */
hostslowtimo()
{
	register struct mbuf *m;
	register struct host *hp, *lp;
	struct hmbuf *hm;
	int s = splimp();

	for (m = hosts; m; m = m->m_next) {
		hm = mtod(m, struct hmbuf *);
		hp = hm->hm_hosts; 
		lp = hp + HPMBUF;
		for (; hm->hm_count > 0 && hp < lp; hp++) {
			if (hp->h_flags & HF_INUSE)
				continue;
			if (hp->h_timer && --hp->h_timer == 0)
				hostrelease(hp);
		}
	}
	splx(s);
}
#endif
