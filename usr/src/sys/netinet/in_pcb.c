/* in_pcb.c 4.11 81/12/02 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/in_pcb.h"

/*
 * Routines to manage internet protocol control blocks.
 *
 * At PRU_ATTACH time a protocol control block is allocated in
 * in_pcballoc() and inserted on a doubly-linked list of such blocks
 * for the protocol.  A port address is either requested (and verified
 * to not be in use) or assigned at this time.  We also allocate
 * space in the socket sockbuf structures here, although this is
 * not a clearly correct place to put this function.
 *
 * A connectionless protocol will have its protocol control block
 * removed at PRU_DETACH time, when the socket will be freed (freeing
 * the space reserved) and the block will be removed from the list of
 * blocks for its protocol.
 *
 * A connection-based protocol may be connected to a remote peer at
 * PRU_CONNECT time through the routine in_pcbconnect().  In the normal
 * case a PRU_DISCONNECT occurs causing a in_pcbdisconnect().
 * It is also possible that higher-level routines will opt out of the
 * relationship with the connection before the connection shut down
 * is complete.  This often occurs in protocols like TCP where we must
 * hold on to the protocol control block for a unreasonably long time
 * after the connection is used up to avoid races in later connection
 * establishment.  To handle this we allow higher-level routines to
 * disassociate themselves from the socket, marking it SS_USERGONE while
 * the disconnect is in progress.  We notice that this has happened
 * when the disconnect is complete, and perform the PRU_DETACH operation,
 * freeing the socket.
 */

/*
 * Allocate a protocol control block, space
 * for send and receive data, and local host information.
 * Return error.  If no error make socket point at pcb.
 */
in_pcbattach(so, head, sndcc, rcvcc, sin)
	struct socket *so;
	struct inpcb *head;
	int sndcc, rcvcc;
	struct sockaddr_in *sin;
{
	struct mbuf *m;
	register struct inpcb *inp, *xp;
	struct ifnet *ifp;
	u_long lport;

COUNT(IN_PCBATTACH);
	if (sin) {
		if (sin->sin_family != AF_INET)
			return (EAFNOSUPPORT);
		ifp = if_ifwithaddr(sin->sin_addr);
		if (ifp == 0)
			return (EADDRNOTAVAIL);
		lport = sin->sin_port;
		if (lport) {
			xp = head->inp_next;
			for (; xp != head; xp = xp->inp_next) 
				if (xp->inp_laddr.s_addr ==
				    sin->sin_addr.s_addr &&
				    xp->inp_lport == lport &&
				    xp->inp_faddr.s_addr == 0)
					return (EADDRINUSE);
		}
	} else {
		ifp = if_ifwithaddr(ifnet->if_addr);
		lport = 0;
	}
	m = m_getclr(M_WAIT);
	if (m == 0)
		return (ENOBUFS);
	if (sbreserve(&so->so_snd, sndcc) == 0)
		goto bad;
	if (sbreserve(&so->so_rcv, rcvcc) == 0)
		goto bad2;
	inp = mtod(m, struct inpcb *);
	inp->inp_laddr = ifp->if_addr;
	if (lport)
		goto gotport;
again:
	if (head->inp_lport++ < 1024)
		head->inp_lport = 1024;
	for (xp = head->inp_next; xp != head; xp = xp->inp_next)
		if (xp->inp_lport == head->inp_lport)
			goto again;
	lport = htons(head->inp_lport);
gotport:
	inp->inp_socket = so;
	inp->inp_lport = lport;
	insque(inp, head);
	so->so_pcb = (caddr_t)inp;
	sin = (struct sockaddr_in *)&so->so_addr;
	sin->sin_family = AF_INET;
	sin->sin_addr = inp->inp_laddr;
	sin->sin_port = inp->inp_lport;
	return (0);
bad2:
	sbrelease(&so->so_snd);
bad:
	(void) m_free(m);
	return (ENOBUFS);
}

in_pcbconnect(inp, sin)
	struct inpcb *inp;
	struct sockaddr_in *sin;
{

COUNT(IN_PCBCONNECT);
	if (sin->sin_family != AF_INET)
		return (EAFNOSUPPORT);
	if (sin->sin_addr.s_addr == 0 || sin->sin_port == 0)
		return (EADDRNOTAVAIL);
	/* should check not already in use... */
	inp->inp_faddr = sin->sin_addr;
	inp->inp_fport = sin->sin_port;
	return (0);
}

in_pcbdisconnect(inp)
	struct inpcb *inp;
{

COUNT(IN_PCBDISCONNECT);
	inp->inp_faddr.s_addr = 0;
	if (inp->inp_socket->so_state & SS_USERGONE)
		in_pcbdetach(inp);
}

in_pcbdetach(inp)
	struct inpcb *inp;
{
	struct socket *so = inp->inp_socket;

	so->so_pcb = 0;
	sofree(so);
	remque(inp);
	(void) m_free(dtom(inp));
}

/*
 * Look for a control block to accept a segment.
 * First choice is an exact address match.
 * Second choice is a match of local address, with
 * unspecified foreign address.
 */
struct inpcb *
in_pcblookup(head, faddr, fport, laddr, lport)
	struct inpcb *head;
	struct in_addr faddr, laddr;
	u_short fport, lport;
{
	register struct inpcb *inp;
	struct inpcb *match = 0;

	for (inp = head->inp_next; inp != head; inp = inp->inp_next) {
		if (inp->inp_laddr.s_addr != laddr.s_addr ||
		    inp->inp_lport != lport)
			continue;
		if (inp->inp_faddr.s_addr == 0) {
			match = inp;
			continue;
		}
		if (inp->inp_faddr.s_addr == faddr.s_addr &&
		    inp->inp_fport == fport)
			return (inp);
	}
	return (match);
}
