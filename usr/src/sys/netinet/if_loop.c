/*	if_loop.c	6.1	83/07/29	*/

/*
 * Loopback interface driver for protocol testing and timing.
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/errno.h"
#include "../h/ioctl.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"

#ifdef vax
#include "../vax/mtpr.h"
#endif

#define	LONET	127
#define	LOHOST	1			/* can't be 0, that's broadcast */
#define	LOMTU	(1024+512)

struct	ifnet loif;
int	looutput(), loioctl();

loattach()
{
	register struct ifnet *ifp = &loif;
	register struct sockaddr_in *sin;

	ifp->if_name = "lo";
	ifp->if_mtu = LOMTU;
	ifp->if_net = LONET;
	ifp->if_host[0] = LOHOST;
	sin = (struct sockaddr_in *)&ifp->if_addr;
	sin->sin_family = AF_INET;
	sin->sin_addr = if_makeaddr(LONET, LOHOST);
	ifp->if_flags = IFF_UP | IFF_RUNNING;
	ifp->if_ioctl = loioctl;
	ifp->if_output = looutput;
	if_attach(ifp);
	if_rtinit(ifp, RTF_UP);
}

looutput(ifp, m0, dst)
	struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	int s = splimp();
	register struct ifqueue *ifq;

	ifp->if_opackets++;
	switch (dst->sa_family) {

#ifdef INET
	case AF_INET:
		ifq = &ipintrq;
		if (IF_QFULL(ifq)) {
			IF_DROP(ifq);
			m_freem(m0);
			splx(s);
			return (ENOBUFS);
		}
		IF_ENQUEUE(ifq, m0);
		schednetisr(NETISR_IP);
		break;
#endif
	default:
		splx(s);
		printf("lo%d: can't handle af%d\n", ifp->if_unit,
			dst->sa_family);
		m_freem(m0);
		return (EAFNOSUPPORT);
	}
	ifp->if_ipackets++;
	splx(s);
	return (0);
}

/*
 * Process an ioctl request.
 */
loioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	struct ifreq *ifr = (struct ifreq *)data;
	struct sockaddr_in *sin;
	int s = splimp(), error = 0;

	switch (cmd) {

	case SIOCSIFADDR:
		if (ifp->if_flags & IFF_RUNNING)
			if_rtinit(ifp, -1);	/* delete previous route */
		ifp->if_addr = ifr->ifr_addr;
		sin = (struct sockaddr_in *)&ifp->if_addr;
		ifp->if_net = in_netof(sin->sin_addr);
		ifp->if_host[0] = in_lnaof(sin->sin_addr);
		if_rtinit(ifp, RTF_UP);
		break;

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}
