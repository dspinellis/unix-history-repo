#ifndef lint
static char rcsid[] = "$Header$";
#endif

/*
 * XNS Routing Table Management Daemon
 */
#include "defs.h"

struct sockaddr *
xns_nettosa(net)
u_short *net;
{
	static struct sockaddr_xn sxn;
	
	bzero(&sxn, sizeof (struct sockaddr_xn));
	sxn.sxn_family = AF_XNS;
	xnnet(sxn.sxn_addr.xn_net) = xnnet(net);
	return( (struct sockaddr *)&sxn);
	
}

/*
 * Process a newly received packet.
 */
rip_input(from, size)
	struct sockaddr *from;
	int size;
{
	struct rt_entry *rt;
	struct netinfo *n;
	struct interface *ifp;
	int newsize;
	struct afswitch *afp;

	
	TRACE_INPUT(ifp, from, size);
	if (from->sa_family >= AF_MAX)
		return;
	afp = &afswitch[from->sa_family];
	
	/* are we talking to ourselves? */
	if (ifp = if_ifwithaddr(from)) {
		rt = rtfind(from);
		if (rt == 0 || (rt->rt_state & RTS_INTERFACE) == 0)
			addrouteforif(ifp);
		else
			rt->rt_timer = 0;
		return;
	}
	
	size -= sizeof (u_short)	/* command */;
	n = msg->rip_nets;

	switch (ntohs(msg->rip_cmd)) {

	case RIPCMD_REQUEST:
		/* Be quiet if we don't have anything interesting to talk about */
		if (!supplier) return;	
		newsize = 0;
		while (size > 0) {
			if (size < sizeof (struct netinfo))
				break;
			size -= sizeof (struct netinfo);

			/* 
			 * A single entry with rip_dst == DSTNETS_ALL and
			 * metric ``infinity'' means ``all routes''.
			 */
			if (ntohl(xnnet(n->rip_dst)) == DSTNETS_ALL &&
		            ntohs(n->rip_metric) == HOPCNT_INFINITY && size == 0) {
				if(ifp = if_ifwithnet(from))
				    supply(from, 0, ifp);
				return;
			}
			/*
			 * request for specific nets
			 */
			rt = rtlookup(xns_nettosa(n->rip_dst));
			n->rip_metric = htons( rt == 0 ? HOPCNT_INFINITY :
				min(rt->rt_metric+1, HOPCNT_INFINITY));
			n++, newsize += sizeof (struct netinfo);
		}
		if (newsize > 0) {
			msg->rip_cmd = htons(RIPCMD_RESPONSE);
			newsize += sizeof (u_short);
			/* should check for if with dstaddr(from) first */
			if(ifp = if_ifwithnet(from))
			    (*afp->af_output)(ifp->int_ripsock[0], 0, from, newsize);
		}
		return;

	case RIPCMD_RESPONSE:
		/* verify message came from a router */
		if ((*afp->af_portmatch)(from) == 0)
			return;
		/* update timer for interface on which the packet arrived */
		if ((rt = rtfind(from)) && (rt->rt_state & RTS_INTERFACE))
			rt->rt_timer = 0;
		for (; size > 0; size -= sizeof (struct netinfo), n++) {
			if (size < sizeof (struct netinfo))
				break;
			if ((unsigned) ntohs(n->rip_metric) > HOPCNT_INFINITY)
				continue;
			rt = rtlookup(xns_nettosa(n->rip_dst));
			if (rt == 0) {
				rtadd(xns_nettosa(n->rip_dst), from, ntohs(n->rip_metric), 0);
				continue;
			}

			/*
			 * Update if from gateway and different,
			 * from anywhere and shorter, or getting stale and equivalent.
			 */
			if ((equal(from, &rt->rt_router) &&
			    ntohs(n->rip_metric) != rt->rt_metric ) ||
			    (unsigned) ntohs(n->rip_metric) < rt->rt_metric ||
			    (rt->rt_timer > (EXPIRE_TIME/2) &&
			    rt->rt_metric == ntohs(n->rip_metric))) {
				rtchange(rt, from, ntohs(n->rip_metric));
				rt->rt_timer = 0;
			}
		}
		return;
	}
}
