/*	if_loop.c	4.1	81/11/29	*/

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

#define	LONET	254
#define	LOMTU	1024

struct	ifnet loif;
int	looutput();

loattach()
{
	register struct ifnet *ifp = &loif;

	ifp->if_mtu = LOMTU;
	ifp->if_net = LONET;
	ifp->if_output = looutput;
	ifp->if_next = ifnet;
	ifnet = ifp;
}

looutput(ifp, m0, pf)
	struct ifnet *ifp;
	struct mbuf *m0;
	int pf;
{
	int s = splimp();

	switch (pf) {

#ifdef INET
	case PF_INET:
		IF_ENQUEUE(&ipintrq, m0);
		setipintr();
		break;
#endif

	default:
		splx(s);
		printf("lo%d: can't encapsulate pf%d\n", ifp->if_unit, pf);
		m_freem(m0);
		return (0);
	}
	splx(s);
	return (1);
}
