/*	in_pcb.c	4.35	82/10/30	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../net/if.h"
#include "../net/route.h"
#include "../netinet/in_pcb.h"
#include "../h/protosw.h"

struct	in_addr zeroin_addr;

in_pcbreserve(so, sndcc, rcvcc)
	struct socket *so;
	int sndcc, rcvcc;
{

	if (sbreserve(&so->so_snd, sndcc) == 0)
		goto bad;
	if (sbreserve(&so->so_rcv, rcvcc) == 0)
		goto bad2;
	return (0);
bad2:
	sbrelease(&so->so_snd);
bad:
	return (ENOBUFS);
}

in_pcballoc(so, head)
	struct socket *so;
	struct inpcb *head;
{
	struct mbuf *m;
	register struct inpcb *inp;

	m = m_getclr(M_DONTWAIT);
	if (m == 0)
		return (ENOBUFS);
	inp = mtod(m, struct inpcb *);
	inp->inp_head = head;
	inp->inp_socket = so;
	insque(inp, head);
	so->so_pcb = (caddr_t)inp;
	return (0);
}
	
in_pcbbind(inp, nam)
	register struct inpcb *inp;
	struct mbuf *nam;
{
	register struct socket *so = inp->inp_socket;
	register struct inpcb *head = inp->inp_head;
	register struct sockaddr_in *sin;
	u_short lport = 0;

	if (ifnet == 0)
		return (EADDRNOTAVAIL);
	if (inp->inp_lport || inp->inp_laddr.s_addr)
		return (EINVAL);
	if (nam == 0)
		goto noname;
	sin = mtod(nam, struct sockaddr_in *);
	if (nam->m_len != sizeof (*sin))
		return (EINVAL);
	if (sin->sin_addr.s_addr) {
		int tport = sin->sin_port;

		sin->sin_port = 0;		/* yech... */
		if (if_ifwithaddr((struct sockaddr *)sin) == 0)
			return (EADDRNOTAVAIL);
		sin->sin_port = tport;
	}
	lport = sin->sin_port;
	if (lport) {
		u_short aport = htons(lport);
		int wild = 0;

		/* GROSS */
		if (aport < IPPORT_RESERVED && u.u_uid != 0)
			return (EACCES);
		if ((so->so_proto->pr_flags & PR_CONNREQUIRED) == 0)
			wild = INPLOOKUP_WILDCARD;
		if (in_pcblookup(head,
		    zeroin_addr, 0, sin->sin_addr, lport, wild))
			return (EADDRINUSE);
	}
	inp->inp_laddr = sin->sin_addr;
noname:
	if (lport == 0)
		do {
			if (head->inp_lport++ < IPPORT_RESERVED)
				head->inp_lport = IPPORT_RESERVED;
			lport = htons(head->inp_lport);
		} while (in_pcblookup(head,
			    zeroin_addr, 0, inp->inp_laddr, lport, 0));
	inp->inp_lport = lport;
	return (0);
}

/*
 * Connect from a socket to a specified address.
 * Both address and port must be specified in argument sin.
 * If don't have a local address for this socket yet,
 * then pick one.
 */
in_pcbconnect(inp, nam)
	struct inpcb *inp;
	struct mbuf *nam;
{
	struct ifnet *ifp;
	struct sockaddr_in *ifaddr;
	register struct sockaddr_in *sin = mtod(nam, struct sockaddr_in *);

	if (nam->m_len != sizeof (*sin))
		return (EINVAL);
	if (sin->sin_family != AF_INET)
		return (EAFNOSUPPORT);
	if (sin->sin_addr.s_addr == 0 || sin->sin_port == 0)
		return (EADDRNOTAVAIL);
	if (inp->inp_laddr.s_addr == 0) {
		ifp = if_ifonnetof(in_netof(sin->sin_addr));
		if (ifp == 0) {
			/*
			 * We should select the interface based on
			 * the route to be used, but for udp this would
			 * result in two calls to rtalloc for each packet
			 * sent; hardly worthwhile...
			 */
			ifp = if_ifwithaf(AF_INET);
			if (ifp == 0)
				return (EADDRNOTAVAIL);
		}
		ifaddr = (struct sockaddr_in *)&ifp->if_addr;
	}
	if (in_pcblookup(inp->inp_head,
	    sin->sin_addr,
	    sin->sin_port,
	    inp->inp_laddr.s_addr ? inp->inp_laddr : ifaddr->sin_addr,
	    inp->inp_lport,
	    0))
		return (EADDRINUSE);
	if (inp->inp_laddr.s_addr == 0)
		inp->inp_laddr = ifaddr->sin_addr;
	inp->inp_faddr = sin->sin_addr;
	inp->inp_fport = sin->sin_port;
	return (0);
}

in_pcbdisconnect(inp)
	struct inpcb *inp;
{

	inp->inp_faddr.s_addr = 0;
	inp->inp_fport = 0;
	if (inp->inp_socket->so_state & SS_NOFDREF)
		in_pcbdetach(inp);
}

in_pcbdetach(inp)
	struct inpcb *inp;
{
	struct socket *so = inp->inp_socket;

	so->so_pcb = 0;
	sofree(so);
	if (inp->inp_route.ro_rt)
		rtfree(inp->inp_route.ro_rt);
	remque(inp);
	(void) m_free(dtom(inp));
}

in_setsockaddr(inp, nam)
	register struct inpcb *inp;
	struct mbuf *nam;
{
	register struct sockaddr_in *sin = mtod(nam, struct sockaddr_in *);
	
	nam->m_len = sizeof (*sin);
	sin = mtod(nam, struct sockaddr_in *);
	bzero((caddr_t)sin, sizeof (*sin));
	sin->sin_family = AF_INET;
	sin->sin_port = inp->inp_lport;
	sin->sin_addr = inp->inp_laddr;
}

/*
 * Pass an error to all internet connections
 * associated with address sin.  Call the
 * protocol specific routine to clean up the
 * mess afterwards.
 */
in_pcbnotify(head, dst, errno, abort)
	struct inpcb *head;
	register struct in_addr *dst;
	int errno, (*abort)();
{
	register struct inpcb *inp, *oinp;
	int s = splimp();

	for (inp = head->inp_next; inp != head;) {
		if (inp->inp_faddr.s_addr != dst->s_addr) {
	next:
			inp = inp->inp_next;
			continue;
		}
		if (inp->inp_socket == 0)
			goto next;
		inp->inp_socket->so_error = errno;
		oinp = inp;
		inp = inp->inp_next;
		(*abort)(oinp);
	}
	splx(s);
}

struct inpcb *
in_pcblookup(head, faddr, fport, laddr, lport, flags)
	struct inpcb *head;
	struct in_addr faddr, laddr;
	u_short fport, lport;
	int flags;
{
	register struct inpcb *inp, *match = 0;
	int matchwild = 3, wildcard;

	for (inp = head->inp_next; inp != head; inp = inp->inp_next) {
		if (inp->inp_lport != lport)
			continue;
		wildcard = 0;
		if (inp->inp_laddr.s_addr != 0) {
			if (laddr.s_addr == 0)
				wildcard++;
			else if (inp->inp_laddr.s_addr != laddr.s_addr)
				continue;
		} else {
			if (laddr.s_addr != 0)
				wildcard++;
		}
		if (inp->inp_faddr.s_addr != 0) {
			if (faddr.s_addr == 0)
				wildcard++;
			else if (inp->inp_faddr.s_addr != faddr.s_addr ||
			    inp->inp_fport != fport)
				continue;
		} else {
			if (faddr.s_addr != 0)
				wildcard++;
		}
		if (wildcard && (flags & INPLOOKUP_WILDCARD) == 0)
			continue;
		if (wildcard < matchwild) {
			match = inp;
			matchwild = wildcard;
			if (matchwild == 0)
				break;
		}
	}
	return (match);
}
