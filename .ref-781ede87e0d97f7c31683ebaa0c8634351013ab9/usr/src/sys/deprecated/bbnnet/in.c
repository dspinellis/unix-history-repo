#ifdef	RCSIDENT
static char rcsident[] = "$Header: in.c,v 1.28 85/04/08 15:06:03 walsh Exp $";
#endif

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/errno.h"
#include "../h/ioctl.h"
#include "../h/dir.h"
#include "../h/user.h"

#include "../net/if.h"
#include "../net/route.h"
#include "../net/af.h"
#include "../net/netisr.h"
#include "../vax/mtpr.h"

#include "../bbnnet/in.h"
#include "../bbnnet/net.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/fsm.h"
#include "../bbnnet/tcp.h"
#include "../bbnnet/udp.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/icmp.h"

#ifdef BBNNET

struct in_ifaddr *in_ifaddr;	/* list of Internet addresses for interfaces */

#define	IN_CLASSA(i)		((((long)(i))&0x80000000)==0)
#define	IN_CLASSA_NET		0xff000000
#define	IN_CLASSA_NSHIFT	24
#define	IN_CLASSA_HOST		0x00ffffff

#define	IN_CLASSB(i)		((((long)(i))&0xc0000000)==0x80000000)
#define	IN_CLASSB_NET		0xffff0000
#define	IN_CLASSB_NSHIFT	16
#define	IN_CLASSB_HOST		0x0000ffff

#define	IN_CLASSC(i)		((((long)(i))&0xc0000000)==0xc0000000)
#define	IN_CLASSC_NET		0xffffff00
#define	IN_CLASSC_NSHIFT	8
#define	IN_CLASSC_HOST		0x000000ff
/*
 * little utility routines
 * cannot be macros because called from non-IP segments of the code.
 */

in_lnaof(ip_addr)
struct in_addr ip_addr;
{
    /*
     * 1/27/84 Berkeley interface of programs to kernel uses net ordering
     * This subr used for SIOCSIFADDR ioctl
     */
    register u_long i = ntohl(ip_addr.s_addr);

    if (IN_CLASSA(i))
	return ((i)&IN_CLASSA_HOST);
    else if (IN_CLASSB(i))
	return ((i)&IN_CLASSB_HOST);
    else
	return ((i)&IN_CLASSC_HOST);
}

/*
#ifdef unused
*/
in_netof(ip_addr)
struct in_addr ip_addr;
{
    register u_long i = ntohl(ip_addr.s_addr);

    if (IN_CLASSA(i))
	return (((i)&IN_CLASSA_NET) >> IN_CLASSA_NSHIFT);
    else if (IN_CLASSB(i))
	return (((i)&IN_CLASSB_NET) >> IN_CLASSB_NSHIFT);
    else
	return (((i)&IN_CLASSC_NET) >> IN_CLASSC_NSHIFT);
}
/*
#endif
*/

/*
 * hash an internet address for routing lookups
 * host part of the address is byte-swapped to put host-specific
 * bits in the low byte (only the low LOG2(RTHASHSIZ) bits are used by rtalloc)
 */

inet_hash(sin, hp)
register struct sockaddr_in *sin;
struct afhash *hp;
{
    hp->afh_nethash = NETHASH(sin->sin_addr);
    hp->afh_hosthash = HOSTHASH(sin->sin_addr.s_addr);
}

inet_netmatch(sin1, sin2)
struct sockaddr_in *sin1, *sin2;
{

    return (iptonet(sin1->sin_addr) == iptonet(sin2->sin_addr));
}

/*
 * Formulate an Internet address from network + host.
 */
struct in_addr in_makeaddr(net, host)
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
	if ((ia->ia_netmask & net) == ia->ia_net) 
	{
	    mask = ~ia->ia_subnetmask;
	    break;
	}

    addr = htonl(net | (host & mask));
    return (*(struct in_addr *)&addr);
}

/*
 * Return 1 if the address is a local broadcast address.
 */
in_broadcast(in)
struct in_addr in;
{
    register struct in_ifaddr *ia;

    /*
     * Look through the list of addresses for a match
     * with a broadcast address.
     */
    for (ia = in_ifaddr; ia; ia = ia->ia_next)
	if (((struct sockaddr_in *)&ia->ia_broadaddr)->sin_addr.s_addr ==
	in.s_addr && (ia->ia_ifp->if_flags & IFF_BROADCAST))
	return (TRUE);
    return (FALSE);
}

/*
 * Return the network number from an internet address.
 */
iptonet (in)
struct in_addr in;
{
    register u_long i = ntohl(in.s_addr);
    register u_long net;
    register struct in_ifaddr *ia;

    if (IN_CLASSA(i))
	net = i & IN_CLASSA_NET;
    else if (IN_CLASSB(i))
	net = i & IN_CLASSB_NET;
    else
	net = i & IN_CLASSC_NET;

    /*
     * Check whether network is a subnet;
     * if so, return subnet number.
     */
    for (ia = in_ifaddr; ia; ia = ia->ia_next)
	if (net == ia->ia_net)
	    return (i & ia->ia_subnetmask);

    return (net);
}

#ifdef unused
/*
 * Return the host portion of an internet address.
 */
iptohost (in)
struct in_addr in;
{
    register u_long i = ntohl(in.s_addr);
    register u_long net, host;
    register struct in_ifaddr *ia;

    if (IN_CLASSA(i)) 
    {
	net = i & IN_CLASSA_NET;
	host = i & IN_CLASSA_HOST;
    }
    else if (IN_CLASSB(i)) 
    {
	net = i & IN_CLASSB_NET;
	host = i & IN_CLASSB_HOST;
    }
    else 
    {
	net = i & IN_CLASSC_NET;
	host = i & IN_CLASSC_HOST;
    }

    /*
     * Check whether network is a subnet;
     * if so, use the modified interpretation of `host'.
     */
    for (ia = in_ifaddr; ia; ia = ia->ia_next)
	if (net == ia->ia_net)
	    return (host & ~ia->ia_subnetmask);

    return (host);
}
#endif

#ifdef unused
/*
 * Return TRUE if an internet address is for a ``local'' host
 * (one to which we have a connection through a local logical net).
 */
in_localaddr(in)
struct in_addr in;
{
    register u_long i = ntohl(in.s_addr);
    register u_long net;
    register struct in_ifaddr *ia;

    if (IN_CLASSA(i))
	net = i & IN_CLASSA_NET;
    else if (IN_CLASSB(i))
	net = i & IN_CLASSB_NET;
    else
	net = i & IN_CLASSC_NET;

    for (ia = in_ifaddr; ia; ia = ia->ia_next)
	if (net == ia->ia_net)
	    return (TRUE);

    return (FALSE);
}
#endif

/*
 * because defaults are a bit messy here, the ARP and interface layers
 * are both handled here....
 */

/* ARGSUSED */
in_ioctl(optname, optval)
int optname;
struct mbuf **optval;
{
    extern struct ifnet *ifunit();
    register struct ifreq *ifr = (struct ifreq *)optval;
    register struct ifnet *ifp = ifunit(ifr->ifr_name);
    register struct in_ifaddr *ia = NULL;
    struct ifaddr *ifa;
    struct mbuf *m;
    int error = 0;

    /*
     *  ARP
     */
    switch (optname)
    {
      case SIOCSARP:
      case SIOCDARP:
	if (!suser())
	    return(u.u_error);

	/* fall thru */

      case SIOCGARP:
	return(arpioctl(optname,(caddr_t)optval));
    }

    /*
     * Find address for this interface, if it exists.
     */
    if (ifp)
	for (ia = in_ifaddr; ia; ia = ia->ia_next)
	    if (ia->ia_ifp == ifp)
		break;

    /* 
     * Interface stuff
     */

    switch (optname) 
    {

      case SIOCGIFADDR:
      case SIOCGIFBRDADDR:
      case SIOCGIFDSTADDR:
      case SIOCGIFNETMASK:
	if (ia == (struct in_ifaddr *) NULL)
	    return(EADDRNOTAVAIL);
	break;

      case SIOCSIFADDR:
      case SIOCSIFDSTADDR:
      case SIOCSIFBRDADDR:
      case SIOCSIFNETMASK:
	if (!suser())
	    return(u.u_error);

	if (ifp == NULL)
	    panic("in_control");
	if (ia == (struct in_ifaddr *) NULL) 
	{
	    m = m_getclr(M_WAIT, MT_IFADDR);
	    if (m == (struct mbuf *)NULL)
		return(ENOBUFS);
	    if (ia = in_ifaddr) 
	    {
		for ( ; ia->ia_next; ia = ia->ia_next)
		    ;
		ia->ia_next = mtod(m, struct in_ifaddr *);
	    }
	    else
	        in_ifaddr = mtod(m, struct in_ifaddr *);
	    ia = mtod(m, struct in_ifaddr *);
	    if (ifa = ifp->if_addrlist) 
	    {
		for ( ; ifa->ifa_next; ifa = ifa->ifa_next)
		    ;
		ifa->ifa_next = (struct ifaddr *) ia;
	    }
	    else
	        ifp->if_addrlist = (struct ifaddr *) ia;
	    ia->ia_ifp = ifp;
	    IA_SIN(ia)->sin_family = AF_INET;
	}
	break;
    }

    switch (optname) 
    {

      case SIOCGIFADDR:
	ifr->ifr_addr = ia->ia_addr;
	break;

      case SIOCGIFBRDADDR:
	if ((ifp->if_flags & IFF_BROADCAST) == 0)
	    return(EINVAL);
	ifr->ifr_dstaddr = ia->ia_broadaddr;
	break;

      case SIOCGIFDSTADDR:
	if ((ifp->if_flags & IFF_POINTOPOINT) == 0)
	    return(EINVAL);
	ifr->ifr_dstaddr = ia->ia_dstaddr;
	break;

      case SIOCGIFNETMASK:
#define	satosin(sa)	((struct sockaddr_in *)(sa))
	satosin(&ifr->ifr_addr)->sin_family = AF_INET;
	satosin(&ifr->ifr_addr)->sin_addr.s_addr = htonl(ia->ia_subnetmask);
	break;

      case SIOCSIFDSTADDR:
	if ((ifp->if_flags & IFF_POINTOPOINT) == 0)
	    return(EINVAL);

	if (ifp->if_ioctl &&
	    (error = (*ifp->if_ioctl)(ifp, SIOCSIFDSTADDR, ia)))
	    return(error);

	ia->ia_dstaddr = ifr->ifr_dstaddr;
	break;

      case SIOCSIFBRDADDR:
	if ((ifp->if_flags & IFF_BROADCAST) == 0)
	    return(EINVAL);

	ia->ia_broadaddr = ifr->ifr_broadaddr;
	break;

      case SIOCSIFADDR:
	return(in_ifinit(ifp, ia, (struct sockaddr_in *)&ifr->ifr_addr));

      case SIOCSIFNETMASK:
	ia->ia_subnetmask =
	    ntohl(satosin(&ifr->ifr_addr)->sin_addr.s_addr);
	break;

      default:
	if (ifp == NULL || ifp->if_ioctl == 0)
	    return(EOPNOTSUPP);

	return(((*ifp->if_ioctl)(ifp, optname, (caddr_t)optval)));
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
    struct sockaddr_in netaddr;
    int s = splimp(), error;

    bzero((caddr_t)&netaddr, sizeof (netaddr));
    netaddr.sin_family = AF_INET;
    /*
     * Delete any previous route for an old address.
     */
    if (ia->ia_flags & IFA_ROUTE) 
    {
	if ((ifp->if_flags & IFF_POINTOPOINT) == 0) 
	{
	    netaddr.sin_addr = in_makeaddr(ia->ia_subnet, INADDR_ANY);
	    rtinit((struct sockaddr *)&netaddr, &ia->ia_addr, -1);
	}
	else
	    rtinit((struct sockaddr *)&ia->ia_dstaddr, &ia->ia_addr, -1);
	ia->ia_flags &= ~IFA_ROUTE;
    }
    ia->ia_addr = *(struct sockaddr *)sin;
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
    if (ifp->if_flags & IFF_BROADCAST) 
    {
	ia->ia_broadaddr.sa_family = AF_INET;
	satoipa(&ia->ia_broadaddr) = in_makeaddr(ia->ia_subnet, INADDR_BROADCAST);
    }

    /*
     * Give the interface a chance to initialize
     * if this is its first address,
     * and to validate the address if necessary.
     */
    if (ifp->if_ioctl && (error = (*ifp->if_ioctl)(ifp, SIOCSIFADDR, ia))) 
    {
	splx(s);
	bzero((caddr_t)&ia->ia_addr, sizeof(ia->ia_addr));
	return (error);
    }
    splx(s);
    /*
     * Add route for the network.
     */
    if ((ifp->if_flags & IFF_POINTOPOINT) == 0) 
    {
	netaddr.sin_addr = in_makeaddr(ia->ia_subnet, INADDR_ANY);
	rtinit((struct sockaddr *)&netaddr, &ia->ia_addr, RTF_UP);
    }
    else
	rtinit((struct sockaddr *)&ia->ia_dstaddr, &ia->ia_addr, RTF_HOST|RTF_UP);
    ia->ia_flags |= IFA_ROUTE;
    return (0);
}

/*
 * return address info for specified address
 */

struct in_ifaddr *in_iawithaddr(addr,bcast)
struct in_addr addr;
int bcast;	/* look for broadcast addrs */
{
    register struct in_ifaddr *ia;

    for (ia = in_ifaddr; ia; ia = ia->ia_next)
    {
	if (IA_INADDR(ia).s_addr == addr.s_addr)
	    return(ia);

	if (bcast)
	    if (IA_B_INADDR(ia).s_addr == addr.s_addr)
	        return(ia);
    }

    return(ia);
}

struct in_ifaddr *in_iawithnet(addr)
struct in_addr addr;
{
    register struct in_ifaddr *ia;
    register u_long net;

    net = iptonet(addr);

    for(ia = in_ifaddr; ia; ia = ia->ia_next)
    {
	if (iptonet(IA_INADDR(ia)) == net)
	    return(ia);
    }

    return(ia);
}

struct in_ifaddr *in_iafromif(ifp)
struct ifnet *ifp;
{
    register struct ifaddr *ifa;

    for(ifa = ifp->if_addrlist; ifa != NULL; ifa = ifa->ifa_next)
	if (ifa->ifa_addr.sa_family == AF_INET)
	    break;

    return((struct in_ifaddr *)ifa);
}

/*
#ifdef unused
*/
/*
 * Return address info for specified internet network.
 */
struct in_ifaddr *in_iaonnetof(net)
u_long net;
{
    register struct in_ifaddr *ia;

    for (ia = in_ifaddr; ia; ia = ia->ia_next)
	if (ia->ia_subnet == net)
	    return (ia);

    return ((struct in_ifaddr *) NULL);
}
/*
#endif
*/
#endif
