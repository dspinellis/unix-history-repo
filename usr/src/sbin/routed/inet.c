#ifndef lint
static char *sccsid[] = "@(#)inet.c	4.1 (Berkeley) %G%";
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
	register net;

	if (IN_CLASSA(i)) {
		net = ((i)&IN_CLASSA_NET) >> IN_CLASSA_NSHIFT;
		if (IN_SUBNETA(i) && inet_localnet(net))
			return (((i)&IN_CLASSA_SUBNET) >> IN_CLASSA_SUBNSHIFT);
		else
			return (net);
	} else if (IN_CLASSB(i)) {
		net = ((i)&IN_CLASSB_NET) >> IN_CLASSB_NSHIFT;
		if (IN_SUBNETB(i) && inet_localnet(net))
			return (((i)&IN_CLASSB_SUBNET) >> IN_CLASSB_SUBNSHIFT);
		else
			return (net);
	} else {
		return (((i)&IN_CLASSC_NET) >> IN_CLASSC_NSHIFT);
	}
}

/*
 * Return the host portion of an internet address.
 */
inet_lnaof(in)
	struct in_addr in;
{
	register u_long i = ntohl(in.s_addr);

	if (IN_CLASSA(i)) {
		if (IN_SUBNETA(i) &&
		    inet_localnet(((i)&IN_CLASSA_NET) >> IN_CLASSA_NSHIFT))
			return ((i)&IN_CLASSA_SUBHOST);
		else
			return ((i)&IN_CLASSA_HOST);
	} else if (IN_CLASSB(i)) {
		if (IN_SUBNETB(i) &&
		    inet_localnet(((i)&IN_CLASSB_NET) >> IN_CLASSB_NSHIFT))
			return ((i)&IN_CLASSB_SUBHOST);
		else
			return ((i)&IN_CLASSB_HOST);
	} else {
		return ((i)&IN_CLASSC_HOST);
	}
}

/*
 * Return true if the network is a ``local'' net
 * (one for which we can interpret the host part).
 */
inet_localnet(net)
	register int net;
{
	register struct interface *ifp;
	extern struct interface *ifnet;

	for (ifp = ifnet; ifp; ifp = ifp->int_next) {
		if (ifp->int_flags & IFF_LOCAL) {
			if (ifp->int_net == net)
				return (1);
			if ((ifp->int_net >> SUBNETSHIFT) == net)
				return (1);
		}
	}
	return (0);
}
