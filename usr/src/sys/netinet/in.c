/*	in.c	6.3	84/04/24	*/

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../net/if.h"
#include "../net/route.h"
#include "../net/af.h"

#ifdef INET
inet_hash(sin, hp)
	register struct sockaddr_in *sin;
	struct afhash *hp;
{

	hp->afh_nethash = in_netof(sin->sin_addr);
	hp->afh_hosthash = ntohl(sin->sin_addr.s_addr);
}

inet_netmatch(sin1, sin2)
	struct sockaddr_in *sin1, *sin2;
{

	return (in_netof(sin1->sin_addr) == in_netof(sin2->sin_addr));
}

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
in_netof(in)
	struct in_addr in;
{
	register u_long i = ntohl(in.s_addr);
	register u_long net, subnet;
	register struct ifnet *ifp;

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
	for (ifp = ifnet; ifp; ifp = ifp->if_next) {
		if (ifp->if_addr.sa_family != AF_INET)
			continue;
		if (ifp->if_flags & IFF_LOCAL) {
			if (ifp->if_net == net)
				return (subnet);
			if ((ifp->if_net >> SUBNETSHIFT) == net)
				return (subnet);
			/*
			 * Hack for use in setting if_net initially.
			 */
			if (ifp->if_net == 0) {
				register struct sockaddr_in *sin;
				sin = (struct sockaddr_in *) &ifp->if_addr;
				if (sin->sin_addr.s_addr == in.s_addr)
					return (subnet);
			}
		}
	}
	return (net);
}

/*
 * Return the host portion of an internet address.
 */
in_lnaof(in)
	struct in_addr in;
{
	register u_long i = ntohl(in.s_addr);
	register u_long net, host, subhost;
	register struct ifnet *ifp;

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
	for (ifp = ifnet; ifp; ifp = ifp->if_next) {
		if (ifp->if_addr.sa_family != AF_INET)
			continue;
		if (ifp->if_flags & IFF_LOCAL) {
			if (ifp->if_net == net)
			    return (subhost);
			if ((ifp->if_net >> SUBNETSHIFT) == net)
			    return (subhost);
		}
	}
	return (host);
}

/*
 * Initialize an interface's routing
 * table entry according to the network.
 * INTERNET SPECIFIC.
 */
if_rtinit(ifp, flags)
	register struct ifnet *ifp;
	int flags;
{
	struct sockaddr_in sin;

	if (ifp->if_flags & IFF_ROUTE)
		return;
	bzero((caddr_t)&sin, sizeof (sin));
	sin.sin_family = AF_INET;
	sin.sin_addr = if_makeaddr(ifp->if_net, INADDR_ANY);
	rtinit((struct sockaddr *)&sin, &ifp->if_addr, flags);
}
#endif
