/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)in.c	7.7 (Berkeley) %G%
 */

#include "param.h"
#include "ioctl.h"
#include "mbuf.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "uio.h"
#include "dir.h"
#include "user.h"
#include "in_systm.h"
#include "../net/if.h"
#include "../net/route.h"
#include "../net/af.h"
#include "in.h"
#include "in_var.h"

#ifdef INET
inet_hash(sin, hp)
	register struct sockaddr_in *sin;
	struct afhash *hp;
{
	register u_long n;

	n = in_netof(sin->sin_addr);
	if (n)
	    while ((n & 0xff) == 0)
		n >>= 8;
	hp->afh_nethash = n;
	hp->afh_hosthash = ntohl(sin->sin_addr.s_addr);
}

inet_netmatch(sin1, sin2)
	struct sockaddr_in *sin1, *sin2;
{

	return (in_netof(sin1->sin_addr) == in_netof(sin2->sin_addr));
}

/*
 * Formulate an Internet address from network + host.
 */
struct in_addr
in_makeaddr(net, host)
	u_long net, host;
{
	register struct in_ifaddr *ia;
	register u_long mask;
	u_long addr;

	if (IN_CLASSA(net))
		mask = IN_CLASSA_HOST;
	else if (IN_CLASSB(net))
		mask = IN_CLASSB_HOST;
	else
		mask = IN_CLASSC_HOST;
	for (ia = in_ifaddr; ia; ia = ia->ia_next)
		if ((ia->ia_netmask & net) == ia->ia_net) {
			mask = ~ia->ia_subnetmask;
			break;
		}
	addr = htonl(net | (host & mask));
	return (*(struct in_addr *)&addr);
}

/*
 * Return the network number from an internet address.
 */
u_long
in_netof(in)
	struct in_addr in;
{
	register u_long i = ntohl(in.s_addr);
	register u_long net;
	register struct in_ifaddr *ia;

	if (IN_CLASSA(i))
		net = i & IN_CLASSA_NET;
	else if (IN_CLASSB(i))
		net = i & IN_CLASSB_NET;
	else if (IN_CLASSC(i))
		net = i & IN_CLASSC_NET;
	else
		return (0);

	/*
	 * Check whether network is a subnet;
	 * if so, return subnet number.
	 */
	for (ia = in_ifaddr; ia; ia = ia->ia_next)
		if (net == ia->ia_net)
			return (i & ia->ia_subnetmask);
	return (net);
}

/*
 * Return the host portion of an internet address.
 */
u_long
in_lnaof(in)
	struct in_addr in;
{
	register u_long i = ntohl(in.s_addr);
	register u_long net, host;
	register struct in_ifaddr *ia;

	if (IN_CLASSA(i)) {
		net = i & IN_CLASSA_NET;
		host = i & IN_CLASSA_HOST;
	} else if (IN_CLASSB(i)) {
		net = i & IN_CLASSB_NET;
		host = i & IN_CLASSB_HOST;
	} else if (IN_CLASSC(i)) {
		net = i & IN_CLASSC_NET;
		host = i & IN_CLASSC_HOST;
	} else
		return (i);

	/*
	 * Check whether network is a subnet;
	 * if so, use the modified interpretation of `host'.
	 */
	for (ia = in_ifaddr; ia; ia = ia->ia_next)
		if (net == ia->ia_net)
			return (host &~ ia->ia_subnetmask);
	return (host);
}

#ifndef SUBNETSARELOCAL
#define	SUBNETSARELOCAL	1
#endif
int subnetsarelocal = SUBNETSARELOCAL;
/*
 * Return 1 if an internet address is for a ``local'' host
 * (one to which we have a connection).  If subnetsarelocal
 * is true, this includes other subnets of the local net.
 * Otherwise, it includes only the directly-connected (sub)nets.
 */
in_localaddr(in)
	struct in_addr in;
{
	register u_long i = ntohl(in.s_addr);
	register struct in_ifaddr *ia;

	if (subnetsarelocal) {
		for (ia = in_ifaddr; ia; ia = ia->ia_next)
			if ((i & ia->ia_netmask) == ia->ia_net)
				return (1);
	} else {
		for (ia = in_ifaddr; ia; ia = ia->ia_next)
			if ((i & ia->ia_subnetmask) == ia->ia_subnet)
				return (1);
	}
	return (0);
}

/*
 * Determine whether an IP address is in a reserved set of addresses
 * that may not be forwarded, or whether datagrams to that destination
 * may be forwarded.
 */
in_canforward(in)
	struct in_addr in;
{
	register u_long i = ntohl(in.s_addr);
	register u_long net;

	if (IN_EXPERIMENTAL(i))
		return (0);
	if (IN_CLASSA(i)) {
		net = i & IN_CLASSA_NET;
		if (net == 0 || net == IN_LOOPBACKNET)
			return (0);
	}
	return (1);
}

int	in_interfaces;		/* number of external internet interfaces */
extern	struct ifnet loif;

/*
 * Generic internet control operations (ioctl's).
 * Ifp is 0 if not an interface-specific ioctl.
 */
/* ARGSUSED */
in_control(so, cmd, data, ifp)
	struct socket *so;
	int cmd;
	caddr_t data;
	register struct ifnet *ifp;
{
	register struct ifreq *ifr = (struct ifreq *)data;
	register struct in_ifaddr *ia = 0;
	struct ifaddr *ifa;
	struct mbuf *m;
	int error;

	/*
	 * Find address for this interface, if it exists.
	 */
	if (ifp)
		for (ia = in_ifaddr; ia; ia = ia->ia_next)
			if (ia->ia_ifp == ifp)
				break;

	switch (cmd) {

	case SIOCSIFADDR:
	case SIOCSIFNETMASK:
	case SIOCSIFDSTADDR:
		if (!suser())
			return (u.u_error);

		if (ifp == 0)
			panic("in_control");
		if (ia == (struct in_ifaddr *)0) {
			m = m_getclr(M_WAIT, MT_IFADDR);
			if (m == (struct mbuf *)NULL)
				return (ENOBUFS);
			if (ia = in_ifaddr) {
				for ( ; ia->ia_next; ia = ia->ia_next)
					;
				ia->ia_next = mtod(m, struct in_ifaddr *);
			} else
				in_ifaddr = mtod(m, struct in_ifaddr *);
			ia = mtod(m, struct in_ifaddr *);
			if (ifa = ifp->if_addrlist) {
				for ( ; ifa->ifa_next; ifa = ifa->ifa_next)
					;
				ifa->ifa_next = (struct ifaddr *) ia;
			} else
				ifp->if_addrlist = (struct ifaddr *) ia;
			ia->ia_ifp = ifp;
			IA_SIN(ia)->sin_family = AF_INET;
			if (ifp != &loif)
				in_interfaces++;
		}
		break;

	case SIOCSIFBRDADDR:
		if (!suser())
			return (u.u_error);
		/* FALLTHROUGH */

	default:
		if (ia == (struct in_ifaddr *)0)
			return (EADDRNOTAVAIL);
		break;
	}

	switch (cmd) {

	case SIOCGIFADDR:
		ifr->ifr_addr = ia->ia_addr;
		break;

	case SIOCGIFBRDADDR:
		if ((ifp->if_flags & IFF_BROADCAST) == 0)
			return (EINVAL);
		ifr->ifr_dstaddr = ia->ia_broadaddr;
		break;

	case SIOCGIFDSTADDR:
		if ((ifp->if_flags & IFF_POINTOPOINT) == 0)
			return (EINVAL);
		ifr->ifr_dstaddr = ia->ia_dstaddr;
		break;

	case SIOCGIFNETMASK:
#define	satosin(sa)	((struct sockaddr_in *)(sa))
		satosin(&ifr->ifr_addr)->sin_family = AF_INET;
		satosin(&ifr->ifr_addr)->sin_addr.s_addr = htonl(ia->ia_subnetmask);
		break;

	case SIOCSIFDSTADDR:
	    {
		struct sockaddr oldaddr;

		if ((ifp->if_flags & IFF_POINTOPOINT) == 0)
			return (EINVAL);
		oldaddr = ia->ia_dstaddr;
		ia->ia_dstaddr = ifr->ifr_dstaddr;
		if (ifp->if_ioctl &&
		    (error = (*ifp->if_ioctl)(ifp, SIOCSIFDSTADDR, ia))) {
			ia->ia_dstaddr = oldaddr;
			return (error);
		}
		if (ia->ia_flags & IFA_ROUTE) {
			rtinit(&oldaddr, &ia->ia_addr, (int)SIOCDELRT,
			    RTF_HOST);
			rtinit(&ia->ia_dstaddr, &ia->ia_addr, (int)SIOCADDRT,
			    RTF_HOST|RTF_UP);
		}
	    }
		break;

	case SIOCSIFBRDADDR:
		if ((ifp->if_flags & IFF_BROADCAST) == 0)
			return (EINVAL);
		ia->ia_broadaddr = ifr->ifr_broadaddr;
		break;

	case SIOCSIFADDR:
		return (in_ifinit(ifp, ia, &ifr->ifr_addr));

	case SIOCSIFNETMASK:
		ia->ia_subnetmask = ntohl(satosin(&ifr->ifr_addr)->sin_addr.s_addr);
		break;

	default:
		if (ifp == 0 || ifp->if_ioctl == 0)
			return (EOPNOTSUPP);
		return ((*ifp->if_ioctl)(ifp, cmd, data));
	}
	return (0);
}

/*
 * Initialize an interface's internet address
 * and routing table entry.
 */
in_ifinit(ifp, ia, sin)
	register struct ifnet *ifp;
	register struct in_ifaddr *ia;
	struct sockaddr_in *sin;
{
	register u_long i = ntohl(sin->sin_addr.s_addr);
	struct sockaddr oldaddr;
	struct sockaddr_in netaddr;
	int s = splimp(), error;

	oldaddr = ia->ia_addr;
	ia->ia_addr = *(struct sockaddr *)sin;

	/*
	 * Give the interface a chance to initialize
	 * if this is its first address,
	 * and to validate the address if necessary.
	 */
	if (ifp->if_ioctl && (error = (*ifp->if_ioctl)(ifp, SIOCSIFADDR, ia))) {
		splx(s);
		ia->ia_addr = oldaddr;
		return (error);
	}

	/*
	 * Delete any previous route for an old address.
	 */
	bzero((caddr_t)&netaddr, sizeof (netaddr));
	netaddr.sin_family = AF_INET;
	if (ia->ia_flags & IFA_ROUTE) {
		if (ifp->if_flags & IFF_LOOPBACK)
			rtinit(&oldaddr, &oldaddr, (int)SIOCDELRT, RTF_HOST);
		else if (ifp->if_flags & IFF_POINTOPOINT)
			rtinit(&ia->ia_dstaddr, &oldaddr, (int)SIOCDELRT,
			    RTF_HOST);
		else {
			netaddr.sin_addr = in_makeaddr(ia->ia_subnet,
			    INADDR_ANY);
			rtinit((struct sockaddr *)&netaddr, &oldaddr, 
			    (int)SIOCDELRT, 0);
		}
		ia->ia_flags &= ~IFA_ROUTE;
	}
	if (IN_CLASSA(i))
		ia->ia_netmask = IN_CLASSA_NET;
	else if (IN_CLASSB(i))
		ia->ia_netmask = IN_CLASSB_NET;
	else
		ia->ia_netmask = IN_CLASSC_NET;
	ia->ia_net = i & ia->ia_netmask;
	/*
	 * The subnet mask includes at least the standard network part,
	 * but may already have been set to a larger value.
	 */
	ia->ia_subnetmask |= ia->ia_netmask;
	ia->ia_subnet = i & ia->ia_subnetmask;
	if (ifp->if_flags & IFF_BROADCAST) {
		ia->ia_broadaddr.sa_family = AF_INET;
		((struct sockaddr_in *)(&ia->ia_broadaddr))->sin_addr =
			in_makeaddr(ia->ia_subnet, INADDR_BROADCAST);
		ia->ia_netbroadcast.s_addr =
		    htonl(ia->ia_net | (INADDR_BROADCAST &~ ia->ia_netmask));
	}
	/*
	 * Add route for the network.
	 */
	if (ifp->if_flags & IFF_LOOPBACK)
		rtinit(&ia->ia_addr, &ia->ia_addr, (int)SIOCADDRT,
		    RTF_HOST|RTF_UP);
	else if (ifp->if_flags & IFF_POINTOPOINT)
		rtinit(&ia->ia_dstaddr, &ia->ia_addr, (int)SIOCADDRT,
		    RTF_HOST|RTF_UP);
	else {
		netaddr.sin_addr = in_makeaddr(ia->ia_subnet, INADDR_ANY);
		rtinit((struct sockaddr *)&netaddr, &ia->ia_addr,
		    (int)SIOCADDRT, RTF_UP);
	}
	ia->ia_flags |= IFA_ROUTE;
	splx(s);
	return (0);
}

/*
 * Return address info for specified internet network.
 */
struct in_ifaddr *
in_iaonnetof(net)
	u_long net;
{
	register struct in_ifaddr *ia;

	for (ia = in_ifaddr; ia; ia = ia->ia_next)
		if (ia->ia_subnet == net)
			return (ia);
	return ((struct in_ifaddr *)0);
}

/*
 * Return 1 if the address might be a local broadcast address.
 */
in_broadcast(in)
	struct in_addr in;
{
	register struct in_ifaddr *ia;
	u_long t;

	/*
	 * Look through the list of addresses for a match
	 * with a broadcast address.
	 */
	for (ia = in_ifaddr; ia; ia = ia->ia_next)
	    if (ia->ia_ifp->if_flags & IFF_BROADCAST) {
		if (satosin(&ia->ia_broadaddr)->sin_addr.s_addr == in.s_addr)
		     return (1);
		/*
		 * Check for old-style (host 0) broadcast.
		 */
		if ((t = ntohl(in.s_addr)) == ia->ia_subnet || t == ia->ia_net)
		    return (1);
	}
	if (in.s_addr == INADDR_BROADCAST || in.s_addr == INADDR_ANY)
		return (1);
	return (0);
}
#endif
