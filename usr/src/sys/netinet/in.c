/*	in.c	6.2	84/04/12	*/

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
	register net;

	if (IN_CLASSA(i)) {
		net = ((i)&IN_CLASSA_NET) >> IN_CLASSA_NSHIFT;
		if (in_localnet(net) && IN_SUBNETA(i))
			return (((i)&IN_CLASSA_SUBNET) >> IN_CLASSA_SUBNSHIFT);
		else
			return (net);
	} else if (IN_CLASSB(i)) {
		net = ((i)&IN_CLASSB_NET) >> IN_CLASSB_NSHIFT;
		if (in_localnet(net) && IN_SUBNETB(i))
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
in_lnaof(in)
	struct in_addr in;
{
	register u_long i = ntohl(in.s_addr);

	if (IN_CLASSA(i)) {
		if (IN_SUBNETA(i) &&
		    in_localnet(((i)&IN_CLASSA_NET) >> IN_CLASSA_NSHIFT))
			return ((i)&IN_CLASSA_SUBHOST);
		else
			return ((i)&IN_CLASSA_HOST);
	} else if (IN_CLASSB(i)) {
		if (IN_SUBNETB(i) &&
		    in_localnet(((i)&IN_CLASSB_NET) >> IN_CLASSB_NSHIFT) )
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
in_localnet(net)
	register int net;
{
	register struct ifnet *ifp;

	for (ifp = ifnet; ifp; ifp = ifp->if_next) {
		if (ifp->if_addr.sa_family != AF_INET)
			continue;
		if (ifp->if_flags & IFF_LOCAL) {
			if (ifp->if_net == net)
				return (1);
			if ((ifp->if_net >> SUBNETSHIFT) == net)
				return (1);
		}
	}
	return (0);
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
