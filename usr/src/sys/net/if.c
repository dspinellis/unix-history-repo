/*	if.c	4.1	81/11/18	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../net/inet.h"
#include "../net/inet_systm.h"
#include "../net/if.h"

if_ifwithaddr(in)
	struct in_addr in;
{
	register struct ifnet *ifp;

#if 0
	for (ifp = ifnet; ifp; ifp = ifp->if_next)
		if (ifp->if_addr.s_addr == in.s_addr)
			break;
#else
	ifp = ifnet;
#endif
	return (ifp);
}

if_ifonnetof(in)
	struct in_addr in;
{
	register struct ifnet *ifp;
#if 0
	int net;

	net = 0;			/* XXX */
	for (ifp = ifnet; ifp; ifp = ifp->if_next)
		if (ifp->if_net == net)
			break;
#else
	ifp = ifnet;
#endif
	return (ifp);
}

struct	ifnet ifen = { 0, 0, 1024, 0, 0 };
struct	ifnet *ifnet = &ifen;
