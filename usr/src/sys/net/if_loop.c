/*	if_loop.c	4.8	82/03/28	*/

/*
 * Loopback interface driver for protocol testing and timing.
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../h/mtpr.h"

#define	LONET	127
#define	LOMTU	(1024+512)

struct	ifnet loif;
int	looutput();

loattach()
{
	register struct ifnet *ifp = &loif;
	register struct sockaddr_in *sin;

	ifp->if_name = "lo";
	ifp->if_mtu = LOMTU;
	ifp->if_net = LONET;
	sin = (struct sockaddr_in *)&ifp->if_addr;
	sin->sin_family = AF_INET;
	sin->sin_addr = if_makeaddr(ifp->if_net, 0);
	ifp->if_flags = IFF_UP;
	ifp->if_output = looutput;
	if_attach(ifp);
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
			return (0);
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
		return (0);
	}
	ifp->if_ipackets++;
	splx(s);
	return (1);
}
