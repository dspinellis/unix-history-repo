/*	udp_usrreq.c	4.6	81/11/18	*/

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
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../net/udp.h"
#include "../net/udp_var.h"

/*
 * UDP protocol implementation.
 * Per RFC 768, August, 1980.
 */
udp_init()
{

	udb.inp_next = udb.inp_prev = &udb;
}

int	udpcksum;
struct	sockaddr_in udp_in = { AF_INET };

udp_input(m0)
	struct mbuf *m0;
{
	register struct udpiphdr *ui;
	register struct inpcb *inp;
	register struct mbuf *m;
	int len, ulen;

	/*
	 * Get ip and udp header together in first mbuf.
	 */
	m = m0;
	ui = mtod(m, struct udpiphdr *);
	if (ui->ui_len > sizeof (struct ip))
		ip_stripoptions((struct ip *)ui);
	if (m->m_len < sizeof (struct udpiphdr) &&
	    m_pullup(m, sizeof (struct udpiphdr)) == 0) {
		udpstat.udps_hdrops++;
		goto bad;
	}

	/*
	 * Make mbuf data length reflect udp length.
	 * If not enough data to reflect udp length, drop.
	 */
	ulen = ntohs(ui->ui_ulen);
	len = sizeof (struct udpiphdr) + ulen;
	if (((struct ip *)ui)->ip_len != len) {
		if (len > ((struct ip *)ui)->ip_len) {
			udpstat.udps_badlen++;
			goto bad;
		}
		m_adj(m, ((struct ip *)ui)->ip_len - len);
		/* (struct ip *)ui->ip_len = len; */
	}

	/*
	 * Checksum extended udp header and data.
	 */
	if (udpcksum) {
		ui->ui_next = ui->ui_prev = 0;
		ui->ui_x1 = 0;
		ui->ui_len = htons(sizeof (struct udpiphdr) + ulen);
		if (ui->ui_sum = inet_cksum(m, len)) {
			udpstat.udps_badsum++;
			printf("udp cksum %x\n", ui->ui_sum);
			m_freem(m);
			return;
		}
	}

	/*
	 * Convert addresses and ports to host format.
	 * Locate pcb for datagram.
	 */
	ui->ui_src.s_addr = ntohl(ui->ui_src.s_addr);
	ui->ui_dst.s_addr = ntohl(ui->ui_dst.s_addr);
	ui->ui_sport = ntohs(ui->ui_sport);
	ui->ui_dport = ntohs(ui->ui_dport);
	inp = in_pcblookup(&udb,
	    ui->ui_src, ui->ui_sport, ui->ui_dst, ui->ui_dport);
	if (inp == 0)
		goto bad;

	/*
	 * Construct sockaddr format source address.
	 * Stuff source address and datagram in user buffer.
	 */
	udp_in.sin_port = ui->ui_sport;
	udp_in.sin_addr = ui->ui_src;
	if (sbappendaddr(inp->inp_socket, &udp_in, m) == 0)
		goto bad;
	return;
bad:
	m_freem(m);
}

udp_ctlinput(m)
	struct mbuf *m;
{

	m_freem(m);
}

/*ARGSUSED*/
udp_output(inp, raddr, rport, m0)
	struct inpcb *inp;
	struct in_addr *raddr;
	u_short rport;
	struct mbuf *m0;
{
	register struct mbuf *m;
	register struct udpiphdr *ui;
	register int len = 0;

	/*
	 * Calculate data length and get a mbuf
	 * for udp and ip headers.
	 */
	for (m = m0; m; m = m->m_next)
		len += m->m_len;
	m = m_get(0);
	if (m == 0)
		goto bad;

	/*
	 * Fill in mbuf with extended udp header
	 * and addresses and length put into network format.
	 */
	m->m_off = MMAXOFF - sizeof (struct udpiphdr);
	m->m_len = sizeof (struct udpiphdr);
	m->m_next = m0;
	ui = mtod(m, struct udpiphdr *);
	ui->ui_next = ui->ui_prev = 0;
	ui->ui_x1 = 0;
	ui->ui_pr = IPPROTO_UDP;
	ui->ui_len = htons(sizeof (struct udphdr) + len);
	ui->ui_src.s_addr = htonl(inp->inp_lhost);
	ui->ui_dst.s_addr = htonl(raddr->s_addr);
	ui->ui_sport = htons(inp->inp_lport);
	ui->ui_dport = htons(inp->inp_fport);
	ui->ui_ulen = htons(len);

	/*
	 * Stuff checksum and output datagram.
	 */
	ui->ui_sum = 0;
	ui->ui_sum = inet_cksum(m, sizeof (struct udpiphdr) + len);
	ip_output(m);
	return;
bad:
	m_freem(m);
}

/*ARGSUSED*/
udp_usrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	caddr_t addr;
{
	struct inpcb *inp = sotoinpcb(so);
	struct sockaddr_in *sin;
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
		in_hosteval(inp, (struct sockaddr *)addr, &error);
		if (inp->inp_fhost == 0)
			return (error);
		soisconnected(so);
		break;

	case PRU_ACCEPT:
		return (EOPNOTSUPP);

	case PRU_DISCONNECT:
		if (inp->inp_fhost == 0)
			return (ENOTCONN);
		in_hostfree(inp->inp_fhost);
		inp->inp_fhost = 0;
		soisdisconnected(so);
		break;

	case PRU_SHUTDOWN:
		socantsendmore(so);
		break;

	case PRU_SEND:
		if (addr) {
			if (inp->inp_fhost)
				return (EISCONN);
			sin = (struct sockaddr_in *)addr;
			if (sin->sin_family != AF_INET)
				return (EAFNOSUPPORT);
			udp_output(inp, sin->sin_addr, sin->sin_port, m);
		} else
			udp_output(inp,
			    inp->inp_fhost->h_addr, inp->inp_fport, m);
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

udp_sense(m)
	struct mbuf *m;
{
	return (EOPNOTSUPP);

}
