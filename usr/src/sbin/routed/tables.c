#ifndef lint
static char sccsid[] = "@(#)tables.c	4.1 %G%";
#endif

/*
 * Routing Table Management Daemon
 */
#include "router.h"
#include <sys/ioctl.h>
#include <errno.h>

#ifndef DEBUG
#define	DEBUG	0
#endif

int	install = !DEBUG;		/* if 1 call kernel */

/*
 * Lookup dst in the tables for an exact match.
 */
struct rt_entry *
rtlookup(dst)
	struct sockaddr *dst;
{
	register struct rt_entry *rt;
	register struct rthash *rh;
	register int hash;
	struct afhash h;
	int doinghost = 1;

	if (dst->sa_family >= AF_MAX)
		return (0);
	(*afswitch[dst->sa_family].af_hash)(dst, &h);
	hash = h.afh_hosthash;
	rh = &hosthash[hash % ROUTEHASHSIZ];
again:
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
		if (rt->rt_hash != hash)
			continue;
		if (equal(&rt->rt_dst, dst))
			return (rt);
	}
	if (doinghost) {
		doinghost = 0;
		hash = h.afh_nethash;
		rh = &nethash[hash % ROUTEHASHSIZ];
		goto again;
	}
	return (0);
}

/*
 * Find a route to dst as the kernel would.
 */
struct rt_entry *
rtfind(dst)
	struct sockaddr *dst;
{
	register struct rt_entry *rt;
	register struct rthash *rh;
	register int hash;
	struct afhash h;
	int af = dst->sa_family;
	int doinghost = 1, (*match)();

	if (af >= AF_MAX)
		return (0);
	(*afswitch[af].af_hash)(dst, &h);
	hash = h.afh_hosthash;
	rh = &hosthash[hash % ROUTEHASHSIZ];

again:
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
		if (rt->rt_hash != hash)
			continue;
		if (doinghost) {
			if (equal(&rt->rt_dst, dst))
				return (rt);
		} else {
			if (rt->rt_dst.sa_family == af &&
			    (*match)(&rt->rt_dst, dst))
				return (rt);
		}
	}
	if (doinghost) {
		doinghost = 0;
		hash = h.afh_nethash;
		rh = &nethash[hash % ROUTEHASHSIZ];
		match = afswitch[af].af_netmatch;
		goto again;
	}
	return (0);
}

rtadd(dst, gate, metric, state)
	struct sockaddr *dst, *gate;
	int metric, state;
{
	struct afhash h;
	register struct rt_entry *rt;
	struct rthash *rh;
	int af = dst->sa_family, flags, hash;

	if (af >= AF_MAX)
		return;
	(*afswitch[af].af_hash)(dst, &h);
	flags = (*afswitch[af].af_checkhost)(dst) ? RTF_HOST : 0;
	if (flags & RTF_HOST) {
		hash = h.afh_hosthash;
		rh = &hosthash[hash % ROUTEHASHSIZ];
	} else {
		hash = h.afh_nethash;
		rh = &nethash[hash % ROUTEHASHSIZ];
	}
	rt = (struct rt_entry *)malloc(sizeof (*rt));
	if (rt == 0)
		return;
	rt->rt_hash = hash;
	rt->rt_dst = *dst;
	rt->rt_router = *gate;
	rt->rt_metric = metric;
	rt->rt_timer = 0;
	rt->rt_flags = RTF_UP | flags;
	rt->rt_state = state | RTS_CHANGED;
	rt->rt_ifp = if_ifwithnet(&rt->rt_router);
	if (metric)
		rt->rt_flags |= RTF_GATEWAY;
	insque(rt, rh);
	TRACE_ACTION(ADD, rt);
	if (install && ioctl(s, SIOCADDRT, (char *)&rt->rt_rt) < 0)
		perror("SIOCADDRT");
}

rtchange(rt, gate, metric)
	struct rt_entry *rt;
	struct sockaddr *gate;
	short metric;
{
	int doioctl = 0, metricchanged = 0;
	struct rtentry oldroute;

	if (!equal(&rt->rt_router, gate))
		doioctl++;
	if (metric != rt->rt_metric) {
		metricchanged++;
		rt->rt_metric = metric;
	}
	if (doioctl || metricchanged) {
		TRACE_ACTION(CHANGE, rt);
		rt->rt_state |= RTS_CHANGED;
	}
	if (doioctl) {
		oldroute = rt->rt_rt;
		rt->rt_router = *gate;
		if (install) {
			if (ioctl(s, SIOCADDRT, (char *)&rt->rt_rt) < 0)
				perror("SIOCADDRT");
			if (ioctl(s, SIOCDELRT, (char *)&oldroute) < 0)
				perror("SIOCDELRT");
		}
	}
}

rtdelete(rt)
	struct rt_entry *rt;
{
	TRACE_ACTION(DELETE, rt);
	if (install && ioctl(s, SIOCDELRT, (char *)&rt->rt_rt))
		perror("SIOCDELRT");
	remque(rt);
	free((char *)rt);
}

rtinit()
{
	register struct rthash *rh;

	for (rh = nethash; rh < &nethash[ROUTEHASHSIZ]; rh++)
		rh->rt_forw = rh->rt_back = (struct rt_entry *)rh;
	for (rh = hosthash; rh < &hosthash[ROUTEHASHSIZ]; rh++)
		rh->rt_forw = rh->rt_back = (struct rt_entry *)rh;
}
