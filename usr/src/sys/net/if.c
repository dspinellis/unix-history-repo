/*	if.c	4.28	83/05/27	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/socket.h"
#include "../h/protosw.h"
#include "../h/time.h"
#include "../h/kernel.h"
#include "../h/ioctl.h"
#include "../h/errno.h"

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
	if_slowtimo();
}

#if vax
/*
 * Call each interface on a Unibus reset.
 */
ifubareset(uban)
	int uban;
{
	register struct ifnet *ifp;

	for (ifp = ifnet; ifp; ifp = ifp->if_next)
		if (ifp->if_reset)
			(*ifp->if_reset)(uban);
}
#endif

/*
 * Attach an interface to the
 * list of "active" interfaces.
 */
if_attach(ifp)
	struct ifnet *ifp;
{
	register struct ifnet **p = &ifnet;

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
	register u_int af = addr->sa_family;
	register int (*netmatch)();

	if (af >= AF_MAX)
		return (0);
	netmatch = afswitch[af].af_netmatch;
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
 * Mark an interface down and notify protocols of
 * the transition.
 * NOTE: must be called at splnet or eqivalent.
 */
if_down(ifp)
	register struct ifnet *ifp;
{

	ifp->if_flags &= ~IFF_UP;
	pfctlinput(PRC_IFDOWN, (caddr_t)&ifp->if_addr);
}

/*
 * Handle interface watchdog timer routines.  Called
 * from softclock, we decrement timers (if set) and
 * call the appropriate interface routine on expiration.
 */
if_slowtimo()
{
	register struct ifnet *ifp;

	for (ifp = ifnet; ifp; ifp = ifp->if_next) {
		if (ifp->if_timer == 0 || --ifp->if_timer)
			continue;
		if (ifp->if_watchdog)
			(*ifp->if_watchdog)(ifp->if_unit);
	}
	timeout(if_slowtimo, (caddr_t)0, hz / IFNET_SLOWHZ);
}

/*
 * Service a socket ioctl request directed
 * to an interface.
 */
ifrequest(cmd, data)
	int cmd;
	caddr_t data;
{
	register struct ifnet *ifp;
	register struct ifreq *ifr;
	register char *cp;
	int unit, s;

	ifr = (struct ifreq *)data;
	for (cp = ifr->ifr_name; *cp; cp++)
		if (*cp >= '0' && *cp <= '9')
			break;
	if (*cp == 0)
		return (ENXIO);		/* couldn't find unit */
	unit = *cp - '0', *cp = 0;
	for (ifp = ifnet; ifp; ifp = ifp->if_next) {
		if (bcmp(ifp->if_name, ifr->ifr_name,
		    (unsigned)(cp - ifr->ifr_name)))
			continue;
		if (unit == ifp->if_unit)
			goto found;
	}
	return (ENXIO);

found:
	switch (cmd) {

	case SIOCGIFADDR:
		ifr->ifr_addr = ifp->if_addr;
		break;

	case SIOCSIFADDR:
		if_rtinit(ifp, -1);	/* delete previous route */
		s = splimp();
		ifp->if_addr = ifr->ifr_addr;
		(*ifp->if_init)(unit);
		splx(s);
		break;

	case SIOCGIFDSTADDR:
		if ((ifp->if_flags & IFF_POINTOPOINT) == 0)
			return (EINVAL);
		ifr->ifr_dstaddr = ifp->if_dstaddr;
		break;

	case SIOCSIFDSTADDR:
		if ((ifp->if_flags & IFF_POINTOPOINT) == 0)
			return (EINVAL);
		s = splimp();
		ifp->if_dstaddr = ifr->ifr_dstaddr;
		splx(s);
		break;

	case SIOCGIFFLAGS:
		ifr->ifr_flags = ifp->if_flags;
		break;

	case SIOCSIFFLAGS:
		if ((ifr->ifr_flags & IFF_UP) == 0 &&
		    (ifp->if_flags & IFF_UP)) {
			s = splimp();
			if_down(ifp);
			splx(s);
		}
		ifp->if_flags = ifr->ifr_flags;
		break;

	default:
		return (EINVAL);
	}
	return (0);
}

/*
 * Return interface configuration
 * of system.  List may be used
 * in later ioctl's (above) to get
 * other information.
 */
/*ARGSUSED*/
ifconf(cmd, data)
	int cmd;
	caddr_t data;
{
	register struct ifconf *ifc = (struct ifconf *)data;
	register struct ifnet *ifp = ifnet;
	register char *cp, *ep;
	struct ifreq ifr, *ifrp;
	int space = ifc->ifc_len, error = 0;

	ifrp = ifc->ifc_req;
	ep = ifr.ifr_name + sizeof (ifr.ifr_name) - 2;
	for (; space > sizeof (ifr) && ifp; ifp = ifp->if_next) {
		bcopy(ifp->if_name, ifr.ifr_name, sizeof (ifr.ifr_name) - 2);
		for (cp = ifr.ifr_name; cp < ep && *cp; cp++)
			;
		*cp++ = '0' + ifp->if_unit; *cp = '\0';
		ifr.ifr_addr = ifp->if_addr;
		error = copyout((caddr_t)&ifr, (caddr_t)ifrp, sizeof (ifr));
		if (error)
			break;
		space -= sizeof (ifr), ifrp++;
	}
	ifc->ifc_len -= space;
	return (error);
}
