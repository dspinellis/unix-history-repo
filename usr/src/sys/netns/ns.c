/*      ns.c     6.1     85/05/30     */

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

	hp->afh_nethash = ns_netof(sns->sns_addr);
	hash = *s++; hash <<= 8; hash += *s++; hash <<= 8; hash += *s;
	hp->afh_hosthash =  hash;
}


ns_netmatch(sns1, sns2)
	struct sockaddr_ns *sns1, *sns2;
{

	return (ns_netof(sns1->sns_addr) == ns_netof(sns2->sns_addr));
}

/*
 * Generic internet control operations (ioctl's).
 */
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
	int error;

	if (!suser())
		return (u.u_error);

	/*
	 * Find address for this interface, if it exists.
	 */
	for (ia = ns_ifaddr; ia; ia = ia->ia_next)
		if (ia->ia_ifp == ifp)
			break;

	switch (cmd) {

	case SIOCGIFADDR:
	case SIOCGIFBRDADDR:
	case SIOCGIFDSTADDR:
		if (ia == (struct ns_ifaddr *)0)
			return (EADDRNOTAVAIL);
		break;

	case SIOCSIFDSTADDR:
		return (EOPNOTSUPP);

	case SIOCSIFADDR:
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
		break;
	}

	switch (cmd) {

	case SIOCGIFADDR:
		ifr->ifr_addr = ia->ia_addr;
		break;

	case SIOCGIFBRDADDR:
		if (ia == (struct ns_ifaddr *)0)
			return (EADDRNOTAVAIL);
		if ((ifp->if_flags & IFF_BROADCAST) == 0)
			return (EINVAL);
		ifr->ifr_dstaddr = ia->ia_broadaddr;
		break;

	case SIOCGIFDSTADDR:
		if (ia == (struct ns_ifaddr *)0)
			return (EADDRNOTAVAIL);
		if ((ifp->if_flags & IFF_POINTOPOINT) == 0)
			return (EINVAL);
		ifr->ifr_dstaddr = ia->ia_dstaddr;
		break;

	case SIOCSIFADDR:
		return
		    (ns_ifinit(ifp, ia, (struct sockaddr_ns *)&ifr->ifr_addr));

	default:
		if (ifp->if_ioctl == 0)
			return (EOPNOTSUPP);
		return ((*ifp->if_ioctl)(ifp, cmd, data));
	}
	return (0);
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
		return(EINVAL);
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
		    rtinit((struct sockaddr *)&netaddr, &ia->ia_addr, -1);
		} else
		    rtinit((struct sockaddr *)&ia->ia_dstaddr, &ia->ia_addr, -1);
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
	 * Point to point links are a little touchier --
	 * We have to have an address of our own first,
	 * and will use the supplied address as that of the other end.
	 */
	if (ifp->if_flags & IFF_POINTOPOINT) {
		struct sockaddr_ns *sns2 = IA_SNS(ia);
		if (ns_hosteqnh(ns_zerohost,ns_thishost))
			return(EINVAL);
		ia->ia_dstaddr = ia->ia_addr;
		sns2->sns_addr.x_host = ns_thishost;
		sns->sns_addr.x_host = ns_thishost;
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
		if(!ns_hosteqnh(ns_thishost,*h)) {
			splx(s);
			return (EINVAL);
		}
	} else {
		splx(s);
		return(EINVAL);
	}
	/*
	 * Add route for the network.
	 */
	if ((ifp->if_flags & IFF_POINTOPOINT) == 0) {
		rtinit((struct sockaddr *)&netaddr, &ia->ia_addr, RTF_UP);
	} else
		rtinit((struct sockaddr *)&ia->ia_dstaddr, &ia->ia_addr,
			RTF_HOST|RTF_UP);
	ia->ia_flags |= IFA_ROUTE;
	return(0);
}

/*
 * Return address info for specified internet network.
 */
struct ns_ifaddr *
ns_iaonnetof(net)
	union ns_net net;
{
	register struct ns_ifaddr *ia;

#define	NtoL(x)	(*(long *)(&(x)))
	for (ia = ns_ifaddr; ia; ia = ia->ia_next)
		if (NtoL(ia->ia_net) == NtoL(net))
			return (ia);
	return ((struct ns_ifaddr *)0);
}
#endif
