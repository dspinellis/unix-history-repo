/*	route.c	4.3	82/03/29	*/

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

/*
 * With much ado about nothing...
 * route the cars that climb halfway to the stars...
 */
allocroute(ro)
	register struct route *ro;
{
	register struct rtentry *rt, *rtmin;
	register struct mbuf *m;
	register int key;
	struct afhash h;
	struct sockaddr *dst = &ro->ro_dst;
	int af = dst->sa_family, doinghost;

COUNT(ALLOCROUTE);
	if (ro->ro_rt && ro->ro_rt->rt_ifp)	/* can't happen */
		return;
	(*afswitch[af].af_hash)(dst, &h);
	m = routehash[h.afh_hosthash % RTHASHSIZ];
	key = h.afh_hostkey;
	rtmin = 0, doinghost = 1;
again:
	for (; m; m = m->m_next) {
		rt = mtod(m, struct rtentry *);
		if (rt->rt_key != key)
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
		ro->ro_dst = rt->rt_dst;
		ro->ro_rt = rt;
		rt->rt_refcnt++;
		return (rt->rt_flags & RTF_DIRECT);
	}
	if (doinghost) {
		doinghost = 0;
		m = routehash[h.afh_nethash % RTHASHSIZ];
		key = h.afh_netkey;
		goto again;
	}
	ro->ro_rt = 0;
	return (0);
}

freeroute(rt)
	register struct rtentry *rt;
{
COUNT(FREEROUTE);
	if (rt == 0)
		panic("freeroute");
	rt->rt_refcnt--;
	/* on refcnt == 0 reclaim? notify someone? */
}

#ifdef notdef
struct rtentry *
reroute(sa)
	register struct sockaddr *sa;
{
	register struct rtentry *rt;
	register struct mbuf *m;
	register int key;
	struct afhash h;

COUNT(REROUTE);
	(*afswitch[sa->sa_family].af_hash)(sa, &h);
	m = routehash[h.afh_hosthash];
	key = h.afh_hostkey;
	for (; m; m = m->m_next) {
		rt = mtod(m, struct rtentry *);
		if (rt->rt_key != key)
			continue;
		if (equal(&rt->rt_gateway, sa))
			return (rt);
	}
	return (0);
}
#endif

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
	register int key;
	struct sockaddr *sa = &new->rt_dst;
	struct afhash h;
	int af = sa->sa_family, doinghost, s, error = 0;

COUNT(RTREQUEST);
	(*afswitch[af].af_hash)(sa, &h);
	mprev = &routehash[h.afh_hosthash % RTHASHSIZ];
	key = h.afh_hostkey;
	doinghost = 1;
	s = splimp();
again:
	for (; m = *mprev; mprev = &m->m_next) {
		rt = mtod(m, struct rtentry *);
		if (rt->rt_key != key)
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
		mprev = &routehash[h.afh_nethash % RTHASHSIZ];
		key = h.afh_netkey;
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
		m = m_getclr(M_DONTWAIT);
		if (m == 0) {
			error = ENOBUFS;
			break;
		}
		m->m_off = MMINOFF;
		*mprev = m;
		rt = mtod(m, struct rtentry *);
		*rt = *new;
		rt->rt_key = h.afh_nethash | h.afh_hosthash;
newneighbor:
		rt->rt_ifp = if_ifonnetof(&new->rt_gateway);
		if (rt->rt_ifp == 0)
			rt->rt_flags &= ~RTF_UP;
		rt->rt_refcnt = 0;
		break;
	}
bad:
	splx(s);
	return (error);
}
