/*	if.c	4.11	82/03/28	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/socket.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/af.h"

int	ifqmaxlen = IFQ_MAXLEN;

/*
 * Network interface utility routines.
 *
 * Routines with if_ifwith* names take sockaddr *'s as
 * parameters.  Other routines take value parameters,
 * e.g. if_ifwithnet takes the network number.
 */

ifinit()
{
	register struct ifnet *ifp;

	for (ifp = ifnet; ifp; ifp = ifp->if_next)
		if (ifp->if_init) {
			(*ifp->if_init)(ifp->if_unit);
			if (ifp->if_snd.ifq_maxlen == 0)
				ifp->if_snd.ifq_maxlen = ifqmaxlen;
		}
}

/*
 * Call each interface on a Unibus reset.
 */
ifubareset(uban)
	int uban;
{
	register struct ifnet *ifp;

	for (ifp = ifnet; ifp; ifp = ifp->if_next)
		if (ifp->if_ubareset)
			(*ifp->if_ubareset)(uban);
}

/*
 * Attach an interface to the
 * list of "active" interfaces.
 */
if_attach(ifp)
	struct ifnet *ifp;
{
	register struct ifnet **p = &ifnet;

COUNT(IF_ATTACH);
	while (*p)
		p = &((*p)->if_next);
	*p = ifp;
}

/*
 * Locate an interface based on a complete address.
 */
/*ARGSUSED*/
struct ifnet *
if_ifwithaddr(addr)
	struct sockaddr *addr;
{
	register struct ifnet *ifp;

COUNT(IF_IFWITHADDR);
#define	equal(a1, a2) \
	(bcmp((caddr_t)((a1)->sa_data), (caddr_t)((a2)->sa_data), 14) == 0)
	for (ifp = ifnet; ifp; ifp = ifp->if_next) {
		if (ifp->if_addr.sa_family != addr->sa_family)
			continue;
		if (equal(&ifp->if_addr, addr))
			break;
		if ((ifp->if_flags & IFF_BROADCAST) &&
		    equal(&ifp->if_broadaddr, addr))
			break;
	}
	return (ifp);
}

/*
 * Find an interface on a specific network.  If many, choice
 * is first found.
 */
struct ifnet *
if_ifwithnet(addr)
	register struct sockaddr *addr;
{
	register struct ifnet *ifp;
	register int af = addr->sa_family;
	register int (*netmatch)() = afswitch[af].af_netmatch;

	for (ifp = ifnet; ifp; ifp = ifp->if_next) {
		if (af != ifp->if_addr.sa_family)
			continue;
		if ((*netmatch)(addr, &ifp->if_addr))
			break;
	}
	return (ifp);
}

/*
 * As above, but parameter is network number.
 */
struct ifnet *
if_ifonnetof(net)
	register int net;
{
	register struct ifnet *ifp;

	for (ifp = ifnet; ifp; ifp = ifp->if_next)
		if (ifp->if_net == net)
			break;
	return (ifp);
}

/*
 * Find an interface using a specific address family
 */
struct ifnet *
if_ifwithaf(af)
	register int af;
{
	register struct ifnet *ifp;

	for (ifp = ifnet; ifp; ifp = ifp->if_next)
		if (ifp->if_addr.sa_family == af)
			break;
	return (ifp);
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
