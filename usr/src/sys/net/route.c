/*	route.c	4.5	82/03/30	*/

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
	register int hash, (*match)();
	struct afhash h;
	struct sockaddr *dst = &ro->ro_dst;
	int af = dst->sa_family;

COUNT(RTALLOC);
	if (ro->ro_rt && ro->ro_rt->rt_ifp)			/* XXX */
		return;
	(*afswitch[af].af_hash)(dst, &h);
	rtmin = 0, hash = h.afh_hosthash;
	for (m = rthost[hash % RTHASHSIZ]; m; m = m->m_next) {
		rt = mtod(m, struct rtentry *);
		if (rt->rt_hash != hash)
			continue;
		if (bcmp((caddr_t)&rt->rt_dst, (caddr_t)dst, sizeof (*dst)))
			continue;
		if (rtmin == 0 || rt->rt_use < rtmin->rt_use)
			rtmin = rt;
	}
	if (rtmin) 
		goto found;

	hash = h.afh_nethash;
	match = afswitch[af].af_netmatch;
	for (m = rtnet[hash % RTHASHSIZ]; m; m = m->m_next) {
		rt = mtod(m, struct rtentry *);
		if (rt->rt_hash != hash)
			continue;
		if (rt->rt_dst.sa_family != af || !(*match)(&rt->rt_dst, dst))
			continue;
		if (rtmin == 0 || rt->rt_use < rtmin->rt_use)
			rtmin = rt;
	}
found:
	ro->ro_rt = rtmin;
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

#define	equal(a1, a2) \
	(bcmp((caddr_t)(a1), (caddr_t)(a2), sizeof (struct sockaddr)) == 0)
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
	register int hash, (*match)();
	register struct sockaddr *sa = &new->rt_dst;
	register struct sockaddr *gate = &new->rt_gateway;
	struct afhash h;
	int af = sa->sa_family, doinghost, s, error = 0;

COUNT(RTREQUEST);
	(*afswitch[af].af_hash)(sa, &h);
	hash = h.afh_hosthash;
	mprev = &rthost[hash % RTHASHSIZ];
	doinghost = 1;
	s = splimp();
again:
	for (; m = *mprev; mprev = &m->m_next) {
		rt = mtod(m, struct rtentry *);
		if (rt->rt_hash != hash)
			continue;
		if (doinghost) {
			if (!equal(&rt->rt_dst, sa))
				continue;
		} else {
			if (rt->rt_dst.sa_family != sa->sa_family ||
			    (*match)(&rt->rt_dst, sa) == 0)
				continue;
		}
		/* require full match on deletions */
		if (req == SIOCDELRT && !equal(&rt->rt_gateway, gate))
			continue;
		/* don't keep multiple identical entries */
		if (req == SIOCADDRT && equal(&rt->rt_gateway, gate)) {
			error = EEXIST;
			goto bad;
		}
		break;
	}
	if (m == 0 && doinghost) {
		hash = h.afh_nethash;
		mprev = &rtnet[hash % RTHASHSIZ];
		match = afswitch[af].af_netmatch;
		doinghost = 0;
		goto again;
	}

	if (m == 0 && req != SIOCADDRT) {
		error = ESRCH;
		goto bad;
	}
found:
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
		else if (!equal(&rt->rt_gateway, gate))
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
		rt->rt_hash = new->rt_flags & RTF_HOST ?
			h.afh_hosthash : h.afh_nethash;
		rt->rt_use = 0;
		rt->rt_refcnt = 0;
newneighbor:
		rt->rt_ifp = if_ifwithnet(gate);
		if (rt->rt_ifp == 0)
			rt->rt_flags &= ~RTF_UP;
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
