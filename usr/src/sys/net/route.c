/*	route.c	4.4	82/03/30	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/ioctl.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/af.h"
#include "../net/route.h"
#include <errno.h>

/*
 * Packet routing routines.
 */

rtalloc(ro)
	register struct route *ro;
{
	register struct rtentry *rt, *rtmin;
	register struct mbuf *m;
	register int hash;
	struct afhash h;
	struct sockaddr *dst = &ro->ro_dst;
	int af = dst->sa_family, doinghost;

COUNT(RTALLOC);
	if (ro->ro_rt && ro->ro_rt->rt_ifp)			/* XXX */
		return;
	(*afswitch[af].af_hash)(dst, &h);
	hash = h.afh_hosthash;
	rtmin = 0, doinghost = 1;
again:
	m = routehash[hash % RTHASHSIZ];
	for (; m; m = m->m_next) {
		rt = mtod(m, struct rtentry *);
		if (rt->rt_hash[doinghost] != hash)
			continue;
		if (doinghost) {
#define	equal(a1, a2) \
	(bcmp((caddr_t)(a1), (caddr_t)(a2), sizeof(struct sockaddr)) == 0)
			if (!equal(&rt->rt_dst, dst))
				continue;
		} else {
			if (rt->rt_dst.sa_family != af)
				continue;
			if ((*afswitch[af].af_netmatch)(&rt->rt_dst, dst) == 0)
				continue;
		}
		if (rtmin == 0 || rt->rt_use < rtmin->rt_use)
			rtmin = rt;
	}
	if (rtmin) {
		ro->ro_rt = rt;
		rt->rt_refcnt++;
		return;
	}
	if (doinghost) {
		doinghost = 0;
		hash = h.afh_nethash;
		goto again;
	}
	ro->ro_rt = 0;
	return;
}

rtfree(rt)
	register struct rtentry *rt;
{

COUNT(FREEROUTE);
	if (rt == 0)
		panic("freeroute");
	rt->rt_refcnt--;
	/* on refcnt == 0 reclaim? notify someone? */
}

/*
 * Carry out a request to change the routing table.  Called by
 * interfaces at boot time to make their ``local routes'' known
 * and for ioctl's.
 */
rtrequest(req, new)
	int req;
	register struct rtentry *new;
{
	register struct rtentry *rt;
	register struct mbuf *m, **mprev;
	register int hash;
	struct sockaddr *sa = &new->rt_dst;
	struct afhash h;
	int af = sa->sa_family, doinghost, s, error = 0;

COUNT(RTREQUEST);
	(*afswitch[af].af_hash)(sa, &h);
	hash = h.afh_hosthash;
	doinghost = 1;
	s = splimp();
again:
	mprev = &routehash[hash % RTHASHSIZ];
	for (; m = *mprev; mprev = &m->m_next) {
		rt = mtod(m, struct rtentry *);
		if (rt->rt_hash[doinghost] != hash)
			continue;
		if (doinghost) {
			if (!equal(&rt->rt_dst, &new->rt_dst))
				continue;
		} else {
			if (rt->rt_dst.sa_family != af)
				continue;
			if ((*afswitch[af].af_netmatch)(&rt->rt_dst, sa) == 0)
				continue;
		}
		/* require full match on deletions */
		if (req == SIOCDELRT &&
		    !equal(&rt->rt_gateway, &new->rt_gateway))
			continue;
		/* don't keep multiple identical entries */
		if (req == SIOCADDRT &&
		    equal(&rt->rt_gateway, &new->rt_gateway)) {
			error = EEXIST;
			goto bad;
		}
		break;
	}
	if (m == 0 && doinghost) {
		doinghost = 0;
		hash = h.afh_nethash;
		goto again;
	}
	if (m == 0 && req != SIOCADDRT) {
		error = ESRCH;
		goto bad;
	}
	switch (req) {

	case SIOCDELRT:
		rt->rt_flags &= ~RTF_UP;
		if (rt->rt_refcnt > 0)	/* should we notify protocols? */
			error = EBUSY;
		else
			*mprev = m_free(m);
		break;

	case SIOCCHGRT:
		rt->rt_flags = new->rt_flags;
		if (rt->rt_refcnt > 0)
			error = EBUSY;
		else if (!equal(&rt->rt_gateway, &new->rt_gateway))
			goto newneighbor;
		break;

	case SIOCADDRT:
		m = m_get(M_DONTWAIT);
		if (m == 0) {
			error = ENOBUFS;
			break;
		}
		m->m_off = MMINOFF;
		m->m_len = sizeof (struct rtentry);
		*mprev = m;
		rt = mtod(m, struct rtentry *);
		*rt = *new;
		rt->rt_hash[0] = h.afh_nethash;
		rt->rt_hash[1] = h.afh_hosthash;
newneighbor:
		rt->rt_ifp = if_ifwithnet(&new->rt_gateway);
		if (rt->rt_ifp == 0)
			rt->rt_flags &= ~RTF_UP;
		rt->rt_use = 0;
		rt->rt_refcnt = 0;
		break;
	}
bad:
	splx(s);
	return (error);
}

/*
 * Set up a routing table entry, normally
 * for an interface.
 */
rtinit(dst, gateway, flags)
	struct sockaddr *dst, *gateway;
	int flags;
{
	struct rtentry route;
	struct route ro;

	route.rt_dst = *dst;
	route.rt_gateway = *gateway;
	route.rt_flags = flags;
	route.rt_use = 0;
	(void) rtrequest(SIOCADDRT, &route);
	ro.ro_rt = 0;
	ro.ro_dst = *dst;
	rtalloc(&ro);
}
