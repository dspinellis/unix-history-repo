/*	raw_cb.c	4.4	82/03/05	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/mtpr.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/raw_cb.h"
#include "../errno.h"

/*
 * Routines to manage the raw protocol control blocks. 
 *
 * TODO:
 *	hash lookups by protocol family/protocol + address family
 *	take care of unique address problems per AF
 *	redo address binding to allow wildcards
 */

/*
 * Allocate a control block and a nominal amount
 * of buffer space for the socket.
 */
raw_attach(so, addr)
	register struct socket *so;
	struct sockaddr *addr;
{
	struct mbuf *m;
	register struct rawcb *rp;
	struct ifnet *ifp = ifnet;

COUNT(RAW_ATTACH);
	/*
	 * Should we verify address not already in use?
	 * Some say yes, others no.
	 */
	if (addr) switch (addr->sa_family) {

	case AF_IMPLINK:
	case AF_INET: {
		register struct sockaddr_in *sin = (struct sockaddr_in *)addr;

		if (ifnet && sin->sin_addr.s_addr == 0)
			sin->sin_addr = ifnet->if_addr;
		ifp = if_ifwithaddr(sin->sin_addr);
		break;
		}

	case AF_PUP:
		ifp = ifnet;
		break;

	default:
		return (EAFNOSUPPORT);
	}
	if (ifp == 0)
		return (EADDRNOTAVAIL);
	m = m_getclr(M_DONTWAIT);
	if (m == 0)
		return (ENOBUFS);
	if (sbreserve(&so->so_snd, RAWSNDQ) == 0)
		goto bad;
	if (sbreserve(&so->so_rcv, RAWRCVQ) == 0)
		goto bad2;
	rp = mtod(m, struct rawcb *);
	rp->rcb_socket = so;
	insque(rp, &rawcb);
	so->so_pcb = (caddr_t)rp;
	rp->rcb_pcb = 0;

	if (addr)
		bcopy(addr, &so->so_addr, sizeof(*addr));
	return (0);
bad2:
	sbrelease(&so->so_snd);
bad:
	(void) m_free(m);
	return (ENOBUFS);
}

/*
 * Detach the raw connection block and discard
 * socket resources.
 */
raw_detach(rp)
	register struct rawcb *rp;
{
	struct socket *so = rp->rcb_socket;

COUNT(RAW_DETACH);
	so->so_pcb = 0;
	sofree(so);
	remque(rp);
	(void) m_freem(dtom(rp));
}

/*
 * Disconnect and possibly release resources.
 */
raw_disconnect(rp)
	struct rawcb *rp;
{
COUNT(RAW_DISCONNECT);
	rp->rcb_flags &= ~RAW_ADDR;
	if (rp->rcb_socket->so_state & SS_USERGONE)
		raw_detach(rp);
}

/*
 * Associate a peer's address with a
 * raw connection block.
 */
raw_connaddr(rp, addr)
	struct rawcb *rp;
	struct sockaddr *addr;
{
COUNT(RAW_CONNADDR);
	bcopy(addr, &rp->rcb_addr, sizeof(struct sockaddr));
	rp->rcb_flags |= RAW_ADDR;
}
