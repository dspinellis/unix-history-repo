/*	raw_cb.c	4.8	82/04/11	*/

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
#include "../net/pup.h"
#include "../errno.h"

/*
 * Routines to manage the raw protocol control blocks. 
 *
 * TODO:
 *	hash lookups by protocol family/protocol + address family
 *	take care of unique address problems per AF?
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

COUNT(RAW_ATTACH);
	if (ifnet == 0)
		return (EADDRNOTAVAIL);
	/*
	 * Should we verify address not already in use?
	 * Some say yes, others no.
	 */
	if (addr) switch (addr->sa_family) {

	case AF_IMPLINK:
	case AF_INET:
		if (((struct sockaddr_in *)addr)->sin_addr.s_addr &&
		    if_ifwithaddr(addr) == 0)
			return (EADDRNOTAVAIL);
		break;

#ifdef PUP
	/*
	 * Curious, we convert PUP address format to internet
	 * to allow us to verify we're asking for an Ethernet
	 * interface.  This is wrong, but things are heavily
	 * oriented towards the internet addressing scheme, and
	 * converting internet to PUP would be very expensive.
	 */
	case AF_PUP: {
		struct sockaddr_pup *spup = (struct sockaddr_pup *)addr;
		struct sockaddr_in inpup;

		bzero((caddr_t)&inpup, sizeof(inpup));
		inpup.sin_family = AF_INET;
		inpup.sin_addr.s_net = spup->sp_net;
		inpup.sin_addr.s_impno = spup->sp_host;
		if (inpup.sin_addr.s_addr &&
		    if_ifwithaddr((struct sockaddr *)&inpup) == 0)
			return (EADDRNOTAVAIL);
		break;
	}
#endif

	default:
		return (EAFNOSUPPORT);
	}
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
	if (addr) {
		bcopy((caddr_t)addr, (caddr_t)&rp->rcb_laddr, sizeof(*addr));
		rp->rcb_flags |= RAW_LADDR;
	}
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
	rp->rcb_flags &= ~RAW_FADDR;
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
	bcopy((caddr_t)addr, (caddr_t)&rp->rcb_faddr, sizeof(*addr));
	rp->rcb_flags |= RAW_FADDR;
}
