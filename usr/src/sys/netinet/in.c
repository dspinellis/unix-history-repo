/*	in.c	4.1	82/06/13	*/

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/af.h"

#ifdef INET
inet_hash(sin, hp)
	register struct sockaddr_in *sin;
	struct afhash *hp;
{
COUNT(INET_HASH);
	hp->afh_nethash = in_netof(sin->sin_addr);
	hp->afh_hosthash = ntohl(sin->sin_addr.s_addr);
	if (hp->afh_hosthash < 0)
		hp->afh_hosthash = -hp->afh_hosthash;
}

inet_netmatch(sin1, sin2)
	struct sockaddr_in *sin1, *sin2;
{
COUNT(INET_NETMATCH);
	return (sin1->sin_addr.s_net == sin2->sin_addr.s_net);
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

COUNT(IF_MAKEADDR);
	if (net < 128)
		addr = (net << 24) | host;
	else if (net < 65536)
		addr = (net << 16) | host;
	else
		addr = (net << 8) | host;
#ifdef vax
	addr = htonl(addr);
#endif
	return (*(struct in_addr *)&addr);
}

/*
 * Return the network number from an internet
 * address; handles class a/b/c network #'s.
 */
in_netof(in)
	struct in_addr in;
{

	return (IN_NETOF(in));
}

/*
 * Return the local network address portion of an
 * internet address; handles class a/b/c network
 * number formats.
 */
in_lnaof(in)
	struct in_addr in;
{

	return (IN_LNAOF(in));
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

COUNT(IF_RTINIT);
	if (ifp->if_flags & IFF_ROUTE)
		return;
	bzero((caddr_t)&sin, sizeof (sin));
	sin.sin_family = AF_INET;
	sin.sin_addr = if_makeaddr(ifp->if_net, 0);
	rtinit(&sin, &ifp->if_addr, flags);
}
#endif
