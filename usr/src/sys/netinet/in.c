/*	in.c	4.5	82/10/17	*/

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

	if (net < 128)
		addr = (net << IN_CLASSANSHIFT) | host;
	else if (net < 65536)
		addr = (net << IN_CLASSBNSHIFT) | host;
	else
		addr = (net << IN_CLASSCNSHIFT) | host;
#ifdef vax || pdp11 || ns16032
	addr = htonl(addr);
#endif
	return (*(struct in_addr *)&addr);
}

/*
 * Return the network number from an internet address.
 */
in_netof(in)
	struct in_addr in;
{
	register u_int i = in.s_addr;

#ifdef vax || pdp11 || ns16032
	i = ntohl(i);
#endif
	if (IN_CLASSA(i))
		return (((i)&IN_CLASSANET) >> IN_CLASSANSHIFT);
	else if (IN_CLASSB(i))
		return (((i)&IN_CLASSBNET) >> IN_CLASSBNSHIFT);
	else
		return (((i)&IN_CLASSCNET) >> IN_CLASSCNSHIFT);
}

/*
 * Return the host portion of an internet address.
 */
in_lnaof(in)
	struct in_addr in;
{
	register u_int i = in.s_addr;

#ifdef vax || pdp11 || ns16032
	i = ntohl(i);
#endif
	if (IN_CLASSA(i))
		return ((i)&IN_CLASSAHOST);
	else if (IN_CLASSB(i))
		return ((i)&IN_CLASSBHOST);
	else
		return ((i)&IN_CLASSCHOST);
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
	sin.sin_addr = if_makeaddr(ifp->if_net, 0);
	rtinit(&sin, &ifp->if_addr, flags);
}
#endif
