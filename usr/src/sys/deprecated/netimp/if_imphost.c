/*
 * Copyright (c) 1982,1986,1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)if_imphost.c	7.4 (Berkeley) %G%
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
#include "socket.h"
#include "syslog.h"

#include "../net/if.h"

#include "../netinet/in.h"
#include "../netinet/in_systm.h"

#include "if_imp.h"
#include "if_imphost.h"

extern struct imp_softc imp_softc[];

/*
 * Given an internet address
 * return a host structure (if it exists).
 */
struct host *
hostlookup(imp, host, unit)
	int imp, host, unit;
{
	register struct host *hp;
	register struct mbuf *m;
	register int hash = HOSTHASH(imp, host);

	for (m = imp_softc[unit].imp_hosts; m; m = m->m_next) {
		hp = &mtod(m, struct hmbuf *)->hm_hosts[hash];
	        if (hp->h_imp == imp && hp->h_host == host) {
			if ((hp->h_flags & HF_INUSE) == 0 && hp->h_timer == 0)
				mtod(dtom(hp), struct hmbuf *)->hm_count++;
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
hostenter(imp, host, unit)
	int imp, host, unit;
{
	register struct mbuf *m, **mprev;
	register struct host *hp, *hp0 = 0;
	register int hash = HOSTHASH(imp, host);

	mprev = &imp_softc[unit].imp_hosts;
	while (m = *mprev) {
		mprev = &m->m_next;
		hp = &mtod(m, struct hmbuf *)->hm_hosts[hash];
	        if (hp->h_imp == imp && hp->h_host == host) {
			if ((hp->h_flags & HF_INUSE) == 0 && hp->h_timer == 0)
				mtod(dtom(hp), struct hmbuf *)->hm_count++;
			goto foundhost;
		}
		if ((hp->h_flags & HF_INUSE) == 0) {
			if (hp0 == 0)
				hp0 = hp;
			continue;
		}
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
	hp->h_imp = imp;
	hp->h_host = host;
	hp->h_timer = 0;
	hp->h_flags = 0;

foundhost:
	hp->h_flags |= HF_INUSE;
	return (hp);
}

/*
 * Reset a given imp unit's host entries.
 */
hostreset(unit)
	int unit;
{
	register struct mbuf *m;
	register struct host *hp, *lp;
	struct hmbuf *hm;

	for (m = imp_softc[unit].imp_hosts; m; m = m->m_next) {
		hm = mtod(m, struct hmbuf *);
		hp = hm->hm_hosts; 
		lp = hp + HPMBUF;
		while (hm->hm_count > 0 && hp < lp) {
			hostrelease(hp);
			hp++;
		}
	}
	hostcompress(unit);
}

/*
 * Remove a host structure and release
 * any resources it's accumulated.
 */
hostrelease(hp)
	register struct host *hp;
{
	struct mbuf *mh = dtom(hp);

	hostflush(hp);
	hp->h_flags = 0;
	hp->h_rfnm = 0;
	--mtod(mh, struct hmbuf *)->hm_count;
}

/*
 * Flush the message queue for a host.
 */
hostflush(hp)
	register struct host *hp;
{
	register struct mbuf *m, **mprev;

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
		hp->h_qcnt = 0;
	}
}

hostcompress(unit)
	int unit;
{
	register struct mbuf *m, **mprev;

	mprev = &imp_softc[unit].imp_hosts;
	while (m = *mprev) {
		if (mtod(m, struct hmbuf *)->hm_count == 0)
			*mprev = m_free(m);
		else
			mprev = &m->m_next;
	}
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
	struct imp_softc *sc;
	struct hmbuf *hm;
	int s = splimp(), unit, any;

	for (unit = 0; unit < NIMP; unit++) {
	    any = 0;
	    sc = &imp_softc[unit];
	    for (m = sc->imp_hosts; m; m = m->m_next) {
		hm = mtod(m, struct hmbuf *);
		hp = hm->hm_hosts; 
		lp = hp + HPMBUF;
		for (; hm->hm_count > 0 && hp < lp; hp++) {
		    if (hp->h_timer && --hp->h_timer == 0) {
			if (hp->h_rfnm) {
				log(LOG_WARNING,
				    "imp%d: host %d/imp %d, lost rfnm\n",
				    unit, hp->h_host, ntohs(hp->h_imp));
				imprestarthost(unit, hp);
			}
			if (hp->h_rfnm == 0 && hp->h_qcnt == 0) {
				any = 1;
				hostrelease(hp);
				if (sc->imp_hostq == m)
					sc->imp_hostq = 0;
			}
		    }
		}
	    }
	    if (any) {
		hostcompress(unit);
	    }
	}
	splx(s);
}
#endif
