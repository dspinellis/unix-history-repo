/*	udp_usrreq.c	4.5	81/11/16	*/

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/inaddr.h"
#include "../net/inet.h"
#include "../net/inet_host.h"
#include "../net/inet_pcb.h"
#include "../net/inet_systm.h"
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../net/udp.h"
#include "../net/udp_var.h"

udp_init()
{

	udb.inp_next = udb.inp_prev = &udb;
}

int	udpcksum;

udp_input(m)
	struct mbuf *m;
{
	register struct udpiphdr *ui;
	register struct inpcb *inp;
	u_short lport, fport, ulen;

	ui = mtod(m, struct udpiphdr *);
	if (ui->ui_len > sizeof (struct ip))		/* XXX */
		ip_stripoptions((struct ip *)ui);
	ulen = ((struct ip *)ui)->ip_len;
	ui->ui_len = htons(ulen);
	ui->ui_prev = ui->ui_next = 0;
	ui->ui_x1 = 0;
	lport = ntohs(ui->ui_dport);
	fport = ntohs(ui->ui_sport);
	if (sizeof (struct udpiphdr) > m->m_len)
	    { printf("udp header overflow\n"); m_freem(m); return; }
	if (udpcksum) {
		(void) inet_cksum(m, sizeof (struct ip) + ulen);
		if (ui->ui_sum) {
			printf("udp cksum %x\n", ui->ui_sum);
			m_freem(m);
			return;
		}
	}
	inp = in_pcblookup(&udb, &ui->ui_src, fport, &ui->ui_dst, lport);
	if (inp == 0)
		goto notwanted;
	/* stuff on queue using some subroutine */
	return;
notwanted:
	m_freem(m);
	/* gen icmp? */
}

udp_ctlinput(m)
	struct mbuf *m;
{

	m_freem(m);
}

udp_ctlinput(m)
	struct mbuf *m;
{

	m_freem(m);
}

/*ARGSUSED*/
udp_output(raddr, rport, m)
	int raddr, rport;
	struct mbuf *m;
{

	/* setup header */
	ip_output(m);
}

/*ARGSUSED*/
udp_usrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	caddr_t addr;
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
		in_pcbfree(inp);
		break;

	case PRU_CONNECT:
		if (inp->inp_fhost)
			return (EISCONN);
		inp->inp_fhost = in_hosteval((struct inaddr *)addr, &error);
		if (inp->inp_fhost == 0)
			return (error);
		soisconnected(so);
		break;

	case PRU_DISCONNECT:
		if (inp->inp_fhost == 0)
			return (ENOTCONN);
		in_hostfree(inp->inp_fhost);
		inp->inp_fhost = 0;
		soisdisconnected(so);
		break;

	case PRU_FLUSH:
		return (EOPNOTSUPP);

	case PRU_SHUTDOWN:
		socantsendmore(so);
		break;

	case PRU_SEND:
#if 0
		if (addr) {
			if (inp->inp_fhost)
				return (EISCONN);
			udp_output(addr->in_fhost, addr->in_fport, m);
		} else
#endif
			udp_output(inp->inp_fhost->h_addr, ip->inp_fport, m);
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
