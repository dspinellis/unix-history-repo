/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)input.c	5.12 (Berkeley) %G%";
#endif not lint

/*
 * Routing Table Management Daemon
 */
#include "defs.h"
#include <sys/syslog.h>

/*
 * Process a newly received packet.
 */
rip_input(from, size)
	struct sockaddr *from;
	int size;
{
	register struct rt_entry *rt;
	register struct netinfo *n;
	register struct interface *ifp;
	struct interface *if_ifwithdstaddr();
	int newsize;
	register struct afswitch *afp;
	static struct sockaddr badfrom;

	ifp = 0;
	TRACE_INPUT(ifp, from, size);
	if (from->sa_family >= af_max ||
	    (afp = &afswitch[from->sa_family])->af_hash == (int (*)())0) {
		syslog(LOG_INFO,
	 "\"from\" address in unsupported address family (%d), cmd %d\n",
		    from->sa_family, msg->rip_cmd);
		return;
	}
	switch (msg->rip_cmd) {

	case RIPCMD_REQUEST:
		newsize = 0;
		size -= 4 * sizeof (char);
		n = msg->rip_nets;
		while (size > 0) {
			if (size < sizeof (struct netinfo))
				break;
			size -= sizeof (struct netinfo);

			if (msg->rip_vers > 0) {
				n->rip_dst.sa_family =
					ntohs(n->rip_dst.sa_family);
				n->rip_metric = ntohl(n->rip_metric);
			}
			/* 
			 * A single entry with sa_family == AF_UNSPEC and
			 * metric ``infinity'' means ``all routes''.
			 * We respond to routers only if we are acting
			 * as a supplier, or to anyone other than a router
			 * (eg, query).
			 */
			if (n->rip_dst.sa_family == AF_UNSPEC &&
			    n->rip_metric == HOPCNT_INFINITY && size == 0) {
			    	if (supplier || (*afp->af_portmatch)(from) == 0)
					supply(from, 0, 0);
				return;
			}
			if (n->rip_dst.sa_family < af_max &&
			    afswitch[n->rip_dst.sa_family].af_hash)
				rt = rtlookup(&n->rip_dst);
			else
				rt = 0;
			n->rip_metric = rt == 0 ? HOPCNT_INFINITY :
				min(rt->rt_metric+1, HOPCNT_INFINITY);
			if (msg->rip_vers > 0) {
				n->rip_dst.sa_family =
					htons(n->rip_dst.sa_family);
				n->rip_metric = htonl(n->rip_metric);
			}
			n++, newsize += sizeof (struct netinfo);
		}
		if (newsize > 0) {
			msg->rip_cmd = RIPCMD_RESPONSE;
			newsize += sizeof (int);
			(*afp->af_output)(s, 0, from, newsize);
		}
		return;

	case RIPCMD_TRACEON:
	case RIPCMD_TRACEOFF:
		/* verify message came from a privileged port */
		if ((*afp->af_portcheck)(from) == 0)
			return;
		if ((ifp = if_iflookup(from)) == 0 || (ifp->int_flags &
		    (IFF_BROADCAST | IFF_POINTOPOINT | IFF_REMOTE)) == 0) {
			syslog(LOG_ERR, "trace command from unknown router, %s",
			    (*afswitch[from->sa_family].af_format)(from));
			return;
		}
		packet[size] = '\0';
		if (msg->rip_cmd == RIPCMD_TRACEON)
			traceon(msg->rip_tracefile);
		else
			traceoff();
		return;

	case RIPCMD_RESPONSE:
		/* verify message came from a router */
		if ((*afp->af_portmatch)(from) == 0)
			return;
		(*afp->af_canon)(from);
		/* are we talking to ourselves? */
		ifp = if_ifwithaddr(from);
		if (ifp) {
			rt = rtfind(from);
			if (rt == 0 || ((rt->rt_state & RTS_INTERFACE) == 0) &&
			    rt->rt_metric >= ifp->int_metric) 
				addrouteforif(ifp);
			else
				rt->rt_timer = 0;
			return;
		}
		/*
		 * Update timer for interface on which the packet arrived.
		 * If from other end of a point-to-point link that isn't
		 * in the routing tables, (re-)add the route.
		 */
		if ((rt = rtfind(from)) &&
		    (rt->rt_state & (RTS_INTERFACE | RTS_REMOTE)))
			rt->rt_timer = 0;
		else if ((ifp = if_ifwithdstaddr(from)) &&
		    (rt == 0 || rt->rt_metric >= ifp->int_metric))
			addrouteforif(ifp);
		/*
		 * "Authenticate" router from which message originated.
		 * We accept routing packets from routers directly connected
		 * via broadcast or point-to-point networks,
		 * and from those listed in /etc/gateways.
		 */
		if ((ifp = if_iflookup(from)) == 0 || (ifp->int_flags &
		    (IFF_BROADCAST | IFF_POINTOPOINT | IFF_REMOTE)) == 0) {
			if (bcmp((char *)from, (char *)&badfrom,
			    sizeof(badfrom)) != 0) {
				syslog(LOG_ERR,
				  "packet from unknown router, %s",
				  (*afswitch[from->sa_family].af_format)(from));
				badfrom = *from;
			}
			return;
		}
		size -= 4 * sizeof (char);
		n = msg->rip_nets;
		for (; size > 0; size -= sizeof (struct netinfo), n++) {
			if (size < sizeof (struct netinfo))
				break;
			if (msg->rip_vers > 0) {
				n->rip_dst.sa_family =
					ntohs(n->rip_dst.sa_family);
				n->rip_metric = ntohl(n->rip_metric);
			}
			if ((unsigned) n->rip_metric > HOPCNT_INFINITY)
				continue;
			if (n->rip_dst.sa_family >= af_max ||
			    (afp = &afswitch[n->rip_dst.sa_family])->af_hash ==
			    (int (*)())0) {
				syslog(LOG_INFO,
		"route in unsupported address family (%d), from %s (af %d)\n",
				   n->rip_dst.sa_family,
				   (*afswitch[from->sa_family].af_format)(from),
				   from->sa_family);
				continue;
			}
			if (((*afp->af_checkhost)(&n->rip_dst)) == 0) {
				syslog(LOG_DEBUG,
				    "bad host in route from %s (af %d)\n",
				   (*afswitch[from->sa_family].af_format)(from),
				   from->sa_family);
				continue;
			}
			/*
			 * Adjust metric according to incoming interface.
			 */
			if ((unsigned)n->rip_metric < HOPCNT_INFINITY)
				n->rip_metric += ifp->int_metric;
			rt = rtlookup(&n->rip_dst);
			if (rt == 0 ||
			    (rt->rt_state & (RTS_INTERNAL|RTS_INTERFACE)) ==
			    (RTS_INTERNAL|RTS_INTERFACE)) {
				/*
				 * If we're hearing a logical network route
				 * back from a peer to which we sent it,
				 * ignore it.
				 */
				if (rt && rt->rt_state & RTS_SUBNET &&
				    (*afp->af_sendroute)(rt, from))
					continue;
				/*
				 * Look for an equivalent route that includes
				 * this one before adding this route.
				 */
				rt = rtfind(&n->rip_dst);
				if (rt && equal(from, &rt->rt_router))
					continue;
				if (n->rip_metric < HOPCNT_INFINITY)
				    rtadd(&n->rip_dst, from, n->rip_metric, 0);
				continue;
			}

			/*
			 * Update if from gateway and different,
			 * shorter, or getting stale and equivalent.
			 */
			if (equal(from, &rt->rt_router)) {
				if (n->rip_metric != rt->rt_metric) {
					rtchange(rt, from, n->rip_metric);
					if (rt->rt_metric >= HOPCNT_INFINITY)
						rt->rt_timer =
						    GARBAGE_TIME - EXPIRE_TIME;
				} else if (rt->rt_metric < HOPCNT_INFINITY)
					rt->rt_timer = 0;
			} else if ((unsigned) (n->rip_metric) < rt->rt_metric ||
			    (rt->rt_timer > (EXPIRE_TIME/2) &&
			    rt->rt_metric == n->rip_metric &&
			    n->rip_metric < HOPCNT_INFINITY)) {
				rtchange(rt, from, n->rip_metric);
				rt->rt_timer = 0;
			}
		}
		return;
	}
}
