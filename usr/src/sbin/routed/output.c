#ifndef lint
static char sccsid[] = "@(#)output.c	4.1 %G%";
#endif

/*
 * Routing Table Management Daemon
 */
#include "router.h"

/*
 * Apply the function "f" to all non-passive
 * interfaces.  If the interface supports the
 * use of broadcasting use it, otherwise address
 * the output to the known router.
 */
toall(f)
	int (*f)();
{
	register struct interface *ifp;
	register struct sockaddr *dst;
	extern struct interface *ifnet;

	for (ifp = ifnet; ifp; ifp = ifp->int_next) {
		if (ifp->int_flags & IFF_PASSIVE)
			continue;
		dst = ifp->int_flags & IFF_BROADCAST ? &ifp->int_broadaddr :
		      ifp->int_flags & IFF_POINTOPOINT ? &ifp->int_dstaddr :
		      &ifp->int_addr;
		(*f)(dst, ifp->int_flags & IFF_INTERFACE, ifp);
	}
}

/*
 * Output a preformed packet.
 */
/*ARGSUSED*/
sendmsg(dst, dontroute, ifp)
	struct sockaddr *dst;
	int dontroute;
	struct interface *ifp;
{

	(*afswitch[dst->sa_family].af_output)(dontroute ? snoroute : s,
		dst, sizeof (struct rip));
	TRACE_OUTPUT(ifp, dst, sizeof (struct rip));
}

/*
 * Supply dst with the contents of the routing tables.
 * If this won't fit in one packet, chop it up into several.
 */
supply(dst, dontroute, ifp)
	struct sockaddr *dst;
	struct interface *ifp;
{
	register struct rt_entry *rt;
	struct netinfo *n = msg->rip_nets;
	register struct rthash *rh;
	struct rthash *base = hosthash;
	int doinghost = 1, size;
	int (*output)() = afswitch[dst->sa_family].af_output;
	int sto = dontroute ? snoroute : s;

	msg->rip_cmd = RIPCMD_RESPONSE;
again:
	for (rh = base; rh < &base[ROUTEHASHSIZ]; rh++)
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
		size = (char *)n - packet;
		if (size > MAXPACKETSIZE - sizeof (struct netinfo)) {
			(*output)(sto, dst, size);
			TRACE_OUTPUT(ifp, dst, size);
			n = msg->rip_nets;
		}
		n->rip_dst = rt->rt_dst;
		n->rip_metric = min(rt->rt_metric + 1, HOPCNT_INFINITY);
		n++;
	}
	if (doinghost) {
		doinghost = 0;
		base = nethash;
		goto again;
	}
	if (n != msg->rip_nets) {
		size = (char *)n - packet;
		(*output)(sto, dst, size);
		TRACE_OUTPUT(ifp, dst, size);
	}
}
