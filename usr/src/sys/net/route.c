/*	route.c	4.2	82/03/28	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/buf.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/ioctl.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/af.h"
#include "../net/route.h"

/*
 * Packet routing routines.
 */

/*
 * With much ado about nothing...
 * route the cars that climb halfway to the stars...
 */
route(ro)
	register struct route *ro;
{
	register struct rtentry *rt, *rtmin;
	register struct mbuf *m;
	register int key;
	struct afhash h;
	struct sockaddr *dst = &ro->ro_dst;
	int af = dst->sa_family, doinghost;

COUNT(ROUTE);
	if (ro && ro->ro_rt && ro->ro_rt->rt_ifp)	/* ??? */
		return;
	(*afswitch[af].af_hash)(dst, &h);
	m = routehash[h.afh_hosthash % RTHASHSIZ];
	key = h.afh_hostkey;
	rtmin = 0, doinghost = 1;
again:
	for (; m; m = m->m_next) {
		rt = mtod(m, struct rtentry *);
#define	equal(a1, a2) \
	(bcmp((caddr_t)(a1), (caddr_t)(a2), sizeof(struct sockaddr)) == 0)
		if (rt->rt_key != key)
			continue;
		if (doinghost) {
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
		return;
	}
	if (doinghost) {
		doinghost = 0;
		m = routehash[h.afh_nethash % RTHASHSIZ];
		key = h.afh_netkey;
		goto again;
	}
	ro->ro_rt = 0;
}

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

/*
 * Routing control calls allow a routing daemon
 * to consistenly access the routing data base for updates.
 */
rtcontrol(req, addr)
	caddr_t addr;
{
	register struct rtentry rq;
	int x = splimp(), err = 0;

COUNT(RTCONTROL);
	if (suser())
		goto bad;
	if (copyin(addr, (caddr_t)&rq, sizeof(struct rtentry))) {
		u.u_error = EFAULT;
		goto bad;
	}
	err = rtrequest(req, &rq);
bad:
	splx(x);
	return (err);
}

/*
 * Carry out a user request to modify the data base.
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
	int af = sa->sa_family, doinghost;

	(*afswitch[af].af_hash)(sa, &h);
	mprev = &routehash[h.afh_hosthash % RTHASHSIZ];
	key = h.afh_hostkey;
	doinghost = 1;
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
		break;
	}
	if (m == 0 && doinghost) {
		doinghost = 0;
		mprev = &routehash[h.afh_nethash % RTHASHSIZ];
		key = h.afh_netkey;
		goto again;
	}

	if (m == 0 && req != SIOCADDRT)
		return (ESRCH);
	switch (req) {

	case SIOCDELRT:
		rt->rt_flags &= ~RTF_UP;
		if (rt->rt_refcnt > 0)	/* should we notify protocols? */
			break;
		*mprev = m_free(m);
		break;

	case SIOCCHGRT:
		rt->rt_flags = new->rt_flags;
		if (rt->rt_refcnt > 0)
			return (EBUSY);
		if (!equal(&rt->rt_gateway, &new->rt_gateway))
			goto newneighbor;
		break;

	case SIOCADDRT:
		m = m_getclr(M_DONTWAIT);
		if (m == 0)
			return (ENOBUFS);
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
	return (0);
}
