#ifndef lint
static char *sccsid[] = "@(#)inet.c	4.2 (Berkeley) %G%";
#endif
/*
 * Temporarily, copy these routines from the kernel,
 * as we need to know about subnets.
 */
#include "defs.h"

/*
 * Formulate an Internet address from network + host.  Used in
 * building addresses stored in the ifnet structure.
 */
struct in_addr
if_makeaddr(net, host)
	int net, host;
{
	u_long addr;

	if (net < IN_CLASSA_MAX)
		addr = (net << IN_CLASSA_NSHIFT) | host;
	else if (net < IN_CLASSB_MAX)
		addr = (net << IN_CLASSB_NSHIFT) | host;
	else
		addr = (net << IN_CLASSC_NSHIFT) | host;
	addr = htonl(addr);
	return (*(struct in_addr *)&addr);
}

/*
 * Return the network number from an internet address.
 */
inet_netof(in)
	struct in_addr in;
{
	register u_long i = ntohl(in.s_addr);
	register u_long net, subnet;
	register struct interface *ifp;
	extern struct interface *ifnet;

	if (IN_CLASSA(i)) {
		net = (i & IN_CLASSA_NET) >> IN_CLASSA_NSHIFT;
		if (IN_SUBNETA(i)) {
			subnet = (i & IN_CLASSA_SUBNET) >> IN_CLASSA_SUBNSHIFT;
			/* Fall through and check whether a subnet */
		} else
			return (net);
	} else if (IN_CLASSB(i)) {
		net = (i & IN_CLASSB_NET) >> IN_CLASSB_NSHIFT;
		if (IN_SUBNETB(i)) {
			subnet = (i & IN_CLASSB_SUBNET) >> IN_CLASSB_SUBNSHIFT;
			/* Fall through and check whether a subnet */
		} else
			return (net);
	} else {
		return ((i & IN_CLASSC_NET) >> IN_CLASSC_NSHIFT);
	}

	/*
	 * Check whether network is a subnet of a `local' network;
	 * if so, return subnet number.
	 */
	for (ifp = ifnet; ifp; ifp = ifp->int_next) {
		if (ifp->int_flags & IFF_LOCAL) {
			if (ifp->int_net == net)
				return (subnet);
			if ((ifp->int_net >> SUBNETSHIFT) == net)
				return (subnet);
		}
	}
	return (net);
}

/*
 * Return the host portion of an internet address.
 */
inet_lnaof(in)
	struct in_addr in;
{
	register u_long i = ntohl(in.s_addr);
	register u_long net, host, subhost;
	register struct interface *ifp;

	if (IN_CLASSA(i)) {
		if (IN_SUBNETA(i)) {
			net = (i & IN_CLASSA_NET) >> IN_CLASSA_NSHIFT;
			host = i & IN_CLASSA_HOST;
			subhost = i & IN_CLASSA_SUBHOST;
			/* Fall through and check whether a subnet */
		} else
			return (i & IN_CLASSA_HOST);
	} else if (IN_CLASSB(i)) {
		if (IN_SUBNETB(i)) {
			net = (i & IN_CLASSB_NET) >> IN_CLASSB_NSHIFT;
			host = i & IN_CLASSB_HOST;
			subhost = i & IN_CLASSB_SUBHOST;
			/* Fall through and check whether a subnet */
		} else
			return (i & IN_CLASSB_HOST);
	} else {
		return (i & IN_CLASSC_HOST);
	}

	/*
	 * Check whether network is a subnet of a `local' network;
	 * if so, use the modified interpretation of `host'.
	 */
	for (ifp = ifnet; ifp; ifp = ifp->int_next) {
		if (ifp->int_flags & IFF_LOCAL) {
			if (ifp->int_net == net)
				return (subhost);
			if ((ifp->int_net >> SUBNETSHIFT) == net)
				return (subhost);
		}
	}
	return (host);
}
