/*
 * Copyright (c) 1984, 1985, 1986, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)ns.c	7.3 (Berkeley) %G%
 */

#include "param.h"
#include "mbuf.h"
#include "ioctl.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "uio.h"
#include "dir.h"
#include "user.h"


#include "../net/if.h"
#include "../net/route.h"
#include "../net/af.h"

#include "ns.h"
#include "ns_if.h"

#ifdef NS

struct ns_ifaddr *ns_ifaddr;

ns_hash(sns, hp)
	register struct sockaddr_ns *sns;
	struct afhash *hp;
{
	register long hash = 0;
	register u_short *s =  sns->sns_addr.x_host.s_host;
	union {
		union ns_net	net_e;
		long		long_e;
	} net;

	net.net_e = sns->sns_addr.x_net;
	hp->afh_nethash = net.long_e;
	hash = *s++; hash <<= 8; hash += *s++; hash <<= 8; hash += *s;
	hp->afh_hosthash =  hash;
}


ns_netmatch(sns1, sns2)
	struct sockaddr_ns *sns1, *sns2;
{

	return (ns_neteq(sns1->sns_addr, sns2->sns_addr));
}

/*
 * Generic internet control operations (ioctl's).
 */
/* ARGSUSED */
ns_control(so, cmd, data, ifp)
	struct socket *so;
	int cmd;
	caddr_t data;
	register struct ifnet *ifp;
{
	register struct ifreq *ifr = (struct ifreq *)data;
	register struct ns_ifaddr *ia;
	struct ifaddr *ifa;
	struct mbuf *m;

	/*
	 * Find address for this interface, if it exists.
	 */
	if (ifp == 0)
		return (EADDRNOTAVAIL);
	for (ia = ns_ifaddr; ia; ia = ia->ia_next)
		if (ia->ia_ifp == ifp)
			break;

	switch (cmd) {

	case SIOCGIFADDR:
		if (ia == (struct ns_ifaddr *)0)
			return (EADDRNOTAVAIL);
		ifr->ifr_addr = ia->ia_addr;
		return (0);


	case SIOCGIFBRDADDR:
		if (ia == (struct ns_ifaddr *)0)
			return (EADDRNOTAVAIL);
		if ((ifp->if_flags & IFF_BROADCAST) == 0)
			return (EINVAL);
		ifr->ifr_dstaddr = ia->ia_broadaddr;
		return (0);

	case SIOCGIFDSTADDR:
		if (ia == (struct ns_ifaddr *)0)
			return (EADDRNOTAVAIL);
		if ((ifp->if_flags & IFF_POINTOPOINT) == 0)
			return (EINVAL);
		ifr->ifr_dstaddr = ia->ia_dstaddr;
		return (0);
	}

	if (!suser())
		return (u.u_error);

	switch (cmd) {

	case SIOCSIFADDR:
	case SIOCSIFDSTADDR:
		if (ia == (struct ns_ifaddr *)0) {
			m = m_getclr(M_WAIT, MT_IFADDR);
			if (m == (struct mbuf *)NULL)
				return (ENOBUFS);
			if (ia = ns_ifaddr) {
				for ( ; ia->ia_next; ia = ia->ia_next)
					;
				ia->ia_next = mtod(m, struct ns_ifaddr *);
			} else
				ns_ifaddr = mtod(m, struct ns_ifaddr *);
			ia = mtod(m, struct ns_ifaddr *);
			if (ifa = ifp->if_addrlist) {
				for ( ; ifa->ifa_next; ifa = ifa->ifa_next)
					;
				ifa->ifa_next = (struct ifaddr *) ia;
			} else
				ifp->if_addrlist = (struct ifaddr *) ia;
			ia->ia_ifp = ifp;
			IA_SNS(ia)->sns_family = AF_NS;
		}
	}

	switch (cmd) {

	case SIOCSIFDSTADDR:
		if ((ifp->if_flags & IFF_POINTOPOINT) == 0)
			return (EINVAL);
		if (ia->ia_flags & IFA_ROUTE) {
			rtinit(&ia->ia_dstaddr, &ia->ia_addr,
				(int)SIOCDELRT, RTF_HOST);
			ia->ia_flags &= ~IFA_ROUTE;
		}
		if (ifp->if_ioctl) {
			int error = (*ifp->if_ioctl)(ifp, SIOCSIFDSTADDR, ia);
			if (error)
				return (error);
		}
		ia->ia_dstaddr = ifr->ifr_dstaddr;
		return (0);

	case SIOCSIFADDR:
		return
		    (ns_ifinit(ifp, ia, (struct sockaddr_ns *)&ifr->ifr_addr));

	default:
		if (ifp->if_ioctl == 0)
			return (EOPNOTSUPP);
		return ((*ifp->if_ioctl)(ifp, cmd, data));
	}
}

/*
 * Initialize an interface's internet address
 * and routing table entry.
 */
ns_ifinit(ifp, ia, sns)
	register struct ifnet *ifp;
	register struct ns_ifaddr *ia;
	struct sockaddr_ns *sns;
{
	struct sockaddr_ns netaddr;
	register union ns_host *h = &(IA_SNS(ia)->sns_addr.x_host);
	int s = splimp(), error;

	/*
	 * The convention we shall adopt for naming is that
	 * a supplied address of zero means that "we don't care".
	 * if there is a single interface, use the address of that
	 * interface as our 6 byte host address.
	 * if there are multiple interfaces, use any address already
	 * used.
	 *
	 * If we have gotten into trouble and want to reset back to
	 * virginity, we recognize a request of the broadcast address.
	 */
	if (ns_hosteqnh(sns->sns_addr.x_host, ns_broadhost)) {
		ns_thishost = ns_zerohost;
		splx(s);
		return (0);
	}

	/*
	 * Delete any previous route for an old address.
	 */
	bzero((caddr_t)&netaddr, sizeof (netaddr));
	netaddr.sns_family = AF_NS;
	netaddr.sns_addr.x_host = ns_broadhost;
	netaddr.sns_addr.x_net = ia->ia_net;
	if (ia->ia_flags & IFA_ROUTE) {
		if ((ifp->if_flags & IFF_POINTOPOINT) == 0) {
		    rtinit((struct sockaddr *)&netaddr, &ia->ia_addr,
				    (int)SIOCDELRT, 0);
		} else
		    rtinit(&ia->ia_dstaddr, &ia->ia_addr,
				    (int)SIOCDELRT, RTF_HOST);
	}

	/*
	 * Set up new addresses.
	 */
	ia->ia_addr = *(struct sockaddr *)sns;
	ia->ia_net = sns->sns_addr.x_net;
	netaddr.sns_addr.x_net = ia->ia_net;
	if (ifp->if_flags & IFF_BROADCAST) {
		ia->ia_broadaddr = * (struct sockaddr *) &netaddr;
	}

	/*
	 * Give the interface a chance to initialize
	 * if this is its first address,
	 * and to validate the address if necessary.
	 */
	if (ns_hosteqnh(ns_thishost, ns_zerohost)) {
		if (ifp->if_ioctl &&
		     (error = (*ifp->if_ioctl)(ifp, SIOCSIFADDR, ia))) {
			splx(s);
			return (error);
		}
		ns_thishost = *h;
	} else if (ns_hosteqnh(sns->sns_addr.x_host, ns_zerohost)
	    || ns_hosteqnh(sns->sns_addr.x_host, ns_thishost)) {
		*h = ns_thishost;
		if (ifp->if_ioctl &&
		     (error = (*ifp->if_ioctl)(ifp, SIOCSIFADDR, ia))) {
			splx(s);
			return (error);
		}
		if (!ns_hosteqnh(ns_thishost,*h)) {
			splx(s);
			return (EINVAL);
		}
	} else {
		splx(s);
		return (EINVAL);
	}

	/*
	 * Add route for the network.
	 */
	if (ifp->if_flags & IFF_POINTOPOINT)
		rtinit(&ia->ia_dstaddr, &ia->ia_addr, (int)SIOCADDRT,
			RTF_HOST|RTF_UP);
	else
		rtinit(&ia->ia_broadaddr, &ia->ia_addr, (int)SIOCADDRT,
			RTF_UP);
	ia->ia_flags |= IFA_ROUTE;
	return (0);
}

/*
 * Return address info for specified internet network.
 */
struct ns_ifaddr *
ns_iaonnetof(dst)
	register struct ns_addr *dst;
{
	register struct ns_ifaddr *ia;
	register struct ns_addr *compare;
	register struct ifnet *ifp;
	struct ns_ifaddr *ia_maybe = 0;
	union ns_net net = dst->x_net;

	for (ia = ns_ifaddr; ia; ia = ia->ia_next) {
		if (ifp = ia->ia_ifp) {
			if (ifp->if_flags & IFF_POINTOPOINT) {
				compare = &satons_addr(ia->ia_dstaddr);
				if (ns_hosteq(*dst, *compare))
					return (ia);
				if (ns_neteqnn(net, ia->ia_net))
					ia_maybe = ia;
			} else {
				if (ns_neteqnn(net, ia->ia_net))
					return (ia);
			}
		}
	}
	return (ia_maybe);
}
#endif
