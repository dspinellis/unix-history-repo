/*	if.c	4.6	81/12/07	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"

ifinit()
{
	register struct ifnet *ifp;

	for (ifp = ifnet; ifp; ifp = ifp->if_next)
		if (ifp->if_init)
			(*ifp->if_init)();
}

ifubareset(uban)
	int uban;
{
	register struct ifnet *ifp;

	for (ifp = ifnet; ifp; ifp = ifp->if_next)
		if (ifp->if_ubareset)
			(*ifp->if_ubareset)(uban);
}

if_attach(ifp)
	struct ifnet *ifp;
{

COUNT(IF_ATTACH);
	ifp->if_next = ifnet;
	ifnet = ifp;
}

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
		addr = (net << 24) | host;
	else if (net < 65536)
		addr = (net << 16) | host;
	else
		addr = (net << 8) | host;
	addr = htonl(addr);
	return (*(struct in_addr *)&addr);
}
