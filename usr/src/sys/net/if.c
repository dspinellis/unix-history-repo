/*	if.c	4.2	81/11/20	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../net/inet.h"
#include "../net/inet_systm.h"
#include "../net/if.h"

/*ARGSUSED*/
struct ifnet *
if_ifwithaddr(in)
	struct in_addr in;
{
	register struct ifnet *ifp;

COUNT(IF_IFWITHADDR);
#if 0
	for (ifp = ifnet; ifp; ifp = ifp->if_next)
		if (ifp->if_addr.s_addr == in.s_addr)
			break;
#else
	ifp = ifnet;
#endif
	return (ifp);
}

/*ARGSUSED*/
struct ifnet *
if_ifonnetof(in)
	struct in_addr in;
{
	register struct ifnet *ifp;
#if 0
	int net;

COUNT(IF_IFONNETOF);
	net = 0;			/* XXX */
	for (ifp = ifnet; ifp; ifp = ifp->if_next)
		if (ifp->if_net == net)
			break;
#else
	ifp = ifnet;
#endif
	return (ifp);
}

struct	ifnet ifen;
struct	ifnet *ifnet = &ifen;
