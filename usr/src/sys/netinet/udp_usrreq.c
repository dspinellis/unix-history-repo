/*	udp_usrreq.c	4.3	81/11/14	*/

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/inet.h"
#include "../net/inet_host.h"
#include "../net/inet_pcb.h"
#include "../net/inet_systm.h"
#include "../net/udp.h"
#include "../net/udp_var.h"

udp_init()
{

	udb.inp_next = udb.inp_prev = &udp;
}

udp_input(m)
	struct mbuf *m;
{
	register struct inpcb *inp;
	int raddr, rport;
	int addr, port;

	inp = inpcb_lookup(&udb, addr, port);
	if (inp == 0)
		goto bad;
	/* sostuff(inp->inp_socket, m, raddr, rport); */
	return;
bad:
	m_freem(m);
	/* gen icmp? */
}

udp_ctlinput(m)
	struct mbuf *m;
{

	m_freem(m);
}

udp_advise(m)
	struct mbuf *m;
{

	m_freem(m);
}

udp_output(raddr, rport, m)
	int raddr, rport;
	struct mbuf *m;
{

	/* setup header */
	ip_output(m);
}

udp_usrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	struct in_addr *addr;
{
	struct inpcb *inp = sotoinpcb(so);
	int error;

	switch (req) {

	case PRU_ATTACH:
		if (inp != 0)
			return (EINVAL);
		inp = in_pcballoc();
		if (inp == NULL)
			return (ENOBUFS);
		so->so_pcb = (caddr_t)inp;
		break;

	case PRU_DETACH:
		if (inp == 0)
			return (ENOTCONN);
		sofree(inp->inp_socket);
		udp_detach(inp);
		break;

	case PRU_CONNECT:
		if (inp->inp_fhost)
			return (EISCONN);
		inp->inp_fhost = in_hmake((struct in_addr *)addr, &error);
		if (inp->inp_fhost == 0)
			return (error);
		soisconnected(so);
		break;

	case PRU_DISCONNECT:
		if (inp->inp_fhost == 0)
			return (ENOTCONN);
		h_free(inp->inp_fhost);
		inp->inp_fhost = 0;
		soisdisconnected(so);
		break;

	case PRU_SEND:
#if 0
		if (addr) {
			if (inp->inp_fhost)
				return (EISCONN);
			udp_output(addr->in_fhost, addr->in_fport, m);
		} else
			udp_output(inp->inp_fhost->h_addr, ip->inp_fport, m);
#endif
		break;

	case PRU_ABORT:
		in_pcbfree(inp);
		sofree(so);
		soisdisconnected(so);
		break;

	case PRU_CONTROL:
		return (EOPNOTSUPP);

	default:
		panic("udp_usrreq");
	}
	return (0);
}

udp_sense()
{

}
