/*	raw_usrreq.c	4.6	82/02/01	*/

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/mtpr.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/raw_cb.h"
#include "/usr/include/errno.h"

/*
 * Initialize raw connection block q.
*/
raw_init()
{
COUNT(RAW_INIT);
	rawcb.rcb_next = rawcb.rcb_prev = &rawcb;
}

/*
 * Raw protocol interface.
 */
raw_input(m0, pf, daf, saf)
	struct mbuf *m0;
	struct sockproto pf;
	struct sockaddr daf, saf;
{
	register struct mbuf *m;
	struct raw_header *rh;
	int s;

	/*
	 * Rip off an mbuf for a generic header.
	 */
	m = m_get(M_DONTWAIT);
	if (m == 0) {
		m_freem(m0);
		return;
	}
	m->m_next = m0;
	m->m_off = MMINOFF;
	m->m_len = sizeof(struct raw_header);
	rh = mtod(m, struct raw_header *);
	rh->raw_dst = daf;
	rh->raw_src = saf;
	rh->raw_protocol = pf;

	/*
	 * Header now contains enough info to decide
	 * which socket to place packet in (if any).
	 * Queue it up for the raw protocol process
	 * running at software interrupt level.
	 */
	s = splimp();
	IF_ENQUEUE(&rawintrq, m);
	splx(s);
	setrawintr();
}

/*
 * Raw protocol input routine.  Process packets entered
 * into the queue at interrupt time.  Find the socket
 * associated with the packet(s) and move them over.  If
 * nothing exists for this packet, drop it.
 */
rawintr()
{
	int s;
	struct mbuf *m;
	register struct rawcb *rp;
	register struct socket *so;
	register struct protosw *pr;
	register struct sockproto *sp;
	register struct sockaddr *sa;
	struct raw_header *rawp;
	struct socket *last;

COUNT(RAWINTR);
next:
	s = splimp();
	IF_DEQUEUE(&rawintrq, m);
	splx(s);
	if (m == 0)
		return;
	rawp = mtod(m, struct raw_header *);
	sp = &rawp->raw_protocol;
	sa = &rawp->raw_dst;

	/*
	 * Find the appropriate socket(s) in which to place this
	 * packet.  This is done by matching the protocol and
	 * address information prepended by raw_input against
	 * the info stored in the control block structures.
	 */
	last = 0;
	for (rp = rawcb.rcb_next; rp != &rawcb; rp = rp->rcb_next) {
		so = rp->rcb_socket;
		pr = so->so_proto;
		if (pr->pr_family != sp->sp_family ||
		    pr->pr_protocol != sp->sp_protocol)
			continue;
		if (sa->sa_family != so->so_addr.sa_family)
			continue;
		/*
		 * We assume the lower level routines have
		 * placed the address in a canonical format
		 * suitable for a structure comparison. Packets
		 * are duplicated for each receiving socket.
		 */
		if ((rp->rcb_flags & RAW_ADDR) &&
		    bcmp(sa->sa_data, so->so_addr.sa_data, 14) != 0)
			continue;
		/*
		 * To avoid extraneous packet copies, we keep
		 * track of the last socket the packet should be
		 * placed in, and make copies only after finding a
		 * socket which "collides".
		 */
		if (last) {
			struct mbuf *n;

			if (n = m_copy(m->m_next, 0, M_COPYALL))
				goto nospace;
			if (sbappendaddr(&last->so_rcv, &rawp->raw_src, n) == 0) {
				/*
				 * Should drop notification of lost packet
				 * into this guy's queue, but...
				 */
				m_freem(n);
				goto nospace;
			}
			sorwakeup(last);
		}
nospace:
		last = so;
	}
	if (last == 0)
		goto drop;
	if (sbappendaddr(&last->so_rcv, &rawp->raw_src, m->m_next) == 0)
		goto drop;
	(void) m_free(m);	/* generic header */
	sorwakeup(last);
	goto next;
drop:
	m_freem(m);
	goto next;
}

/*ARGSUSED*/
raw_usrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	caddr_t addr;
{
	register struct rawcb *rp = sotorawcb(so);
	int error = 0;

COUNT(RAW_USRREQ);
	if (rp == 0 && req != PRU_ATTACH)
		return (EINVAL);

	switch (req) {

	/*
	 * Allocate a raw control block and fill in the
	 * necessary info to allow packets to be routed to
	 * the appropriate raw interface routine.
	 */
	case PRU_ATTACH:
		if (rp)
			return (EINVAL);;
		error = raw_attach(so, (struct sockaddr *)addr);
		break;

	/*
	 * Destroy state just before socket deallocation.
	 * Flush data or not depending on the options.
	 */
	case PRU_DETACH:
		if (rp == 0)
			return (ENOTCONN);
		raw_detach(rp);
		break;

	/*
	 * If a socket isn't bound to a single address,
	 * the raw input routine will hand it anything
	 * within that protocol family (assuming there's
	 * nothing else around it should go to). 
	 */
	case PRU_CONNECT:
		if (rp->rcb_flags & RAW_ADDR)
			return (EISCONN);
		raw_connaddr(rp, (struct sockaddr *)addr);
		soisconnected(so);
		break;

	case PRU_DISCONNECT:
		if ((rp->rcb_flags & RAW_ADDR) == 0)
			return (ENOTCONN);
		raw_disconnect(rp);
		soisdisconnected(so);
		break;

	/*
	 * Mark the connection as being incapable of further input.
	 */
	case PRU_SHUTDOWN:
		socantsendmore(so);
		break;

	/*
	 * Ship a packet out.  The appropriate raw output
	 * routine handles any massaging necessary.
	 */
	case PRU_SEND:
		if (addr) {
			if (rp->rcb_flags & RAW_ADDR)
				return (EISCONN);
			raw_connaddr(rp, (struct sockaddr *)addr);
		} else if ((rp->rcb_flags & RAW_ADDR) == 0)
			return (ENOTCONN);
		(void) (*so->so_proto->pr_output)(m, so);
		if (addr)
			rp->rcb_flags &= ~RAW_ADDR;
		break;

	case PRU_ABORT:
		raw_disconnect(rp);
		sofree(so);
		soisdisconnected(so);
		break;

	/*
	 * Not supported.
	 */
	case PRU_ACCEPT:
	case PRU_RCVD:
	case PRU_CONTROL:
	case PRU_SENSE:
	case PRU_RCVOOB:
	case PRU_SENDOOB:
		error = EOPNOTSUPP;
		break;

	default:
		panic("raw_usrreq");
	}
	return (error);
}
