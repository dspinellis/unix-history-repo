/* in_pcb.c 4.10 81/11/29 */

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
 * Allocate a protocol control block, space
 * for send and receive data, and local host information.
 * Return error.  If no error make socket point at pcb.
 */
in_pcballoc(so, head, sndcc, rcvcc, sin)
	struct socket *so;
	struct inpcb *head;
	int sndcc, rcvcc;
	struct sockaddr_in *sin;
{
	struct mbuf *m;
	register struct inpcb *inp, *xp;
	struct ifnet *ifp;
	u_long lport;

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
	lport = head->inp_lport;
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

in_pcbsetpeer(inp, sin)
	struct inpcb *inp;
	struct sockaddr_in *sin;
{

	if (sin->sin_family != AF_INET)
		return (EAFNOSUPPORT);
	if (sin->sin_addr.s_addr == 0 || sin->sin_port == 0)
		return (EADDRNOTAVAIL);
	/* should check not already in use... */
	inp->inp_faddr = sin->sin_addr;
	inp->inp_fport = sin->sin_port;
	return (0);
}

in_pcbfree(inp)
	struct inpcb *inp;
{
	struct socket *so = inp->inp_socket;

	so->so_pcb = 0;
	sofree(so);
	remque(inp);
	(void) m_free(dtom(inp));
}

struct inpcb *
in_pcblookup(head, faddr, fport, laddr, lport)
	struct inpcb *head;
	struct in_addr faddr, laddr;
	u_short fport, lport;
{
	register struct inpcb *inp;

	for (inp = head->inp_next; inp != head; inp = inp->inp_next)
		if (inp->inp_faddr.s_addr == faddr.s_addr &&
		    inp->inp_fport == fport &&
		    inp->inp_laddr.s_addr == laddr.s_addr &&
		    inp->inp_lport == lport)
			return (inp);
	for (inp = head->inp_next; inp != head; inp = inp->inp_next)
		if ((inp->inp_faddr.s_addr == faddr.s_addr ||
		     inp->inp_faddr.s_addr == 0) &&
		    (inp->inp_fport == fport || inp->inp_fport == 0) &&
		     inp->inp_laddr.s_addr == laddr.s_addr &&
		    (inp->inp_lport == lport || inp->inp_lport == 0))
			return (inp);
	return (0);
}
