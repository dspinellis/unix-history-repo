/*	if.c	4.4	81/11/29	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"

/*ARGSUSED*/
struct ifnet *
if_ifwithaddr(in)
	struct in_addr in;
{
	register struct ifnet *ifp;

COUNT(IF_IFWITHADDR);
	for (ifp = ifnet; ifp; ifp = ifp->if_next)
		if (ifp->if_addr.s_addr == in.s_addr)
			break;
	return (ifp);
}

/*ARGSUSED*/
struct ifnet *
if_ifonnetof(in)
	struct in_addr in;
{
	register struct ifnet *ifp;
	int net;

COUNT(IF_IFONNETOF);
	net = in.s_net;			/* XXX */
	for (ifp = ifnet; ifp; ifp = ifp->if_next)
		if (ifp->if_net == net)
			break;
	return (ifp);
}

/*ARGSUSED*/
struct ifnet *
if_gatewayfor(addr)
	struct in_addr addr;
{

COUNT(IF_GATEWAYFOR);
	return (0);
}

struct in_addr
if_makeaddr(net, host)
	int net, host;
{
	u_long addr;

	if (net < 128)
		addr = (host << 8) | net;
	else if (net < 65536)
		addr = (host << 16) | net;
	else
		addr = (host << 24) | net;
	addr = htonl(addr);
	return (*(struct in_addr *)&addr);
}
