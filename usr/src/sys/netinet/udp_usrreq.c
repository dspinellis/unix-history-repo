/*	udp_usrreq.c	4.29	82/06/12	*/

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/in.h"
#include "../net/if.h"
#include "../net/route.h"
#include "../net/in_pcb.h"
#include "../net/in_systm.h"
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../net/ip_icmp.h"
#include "../net/udp.h"
#include "../net/udp_var.h"
#include <errno.h>

/*
 * UDP protocol implementation.
 * Per RFC 768, August, 1980.
 */
udp_init()
{

COUNT(UDP_INIT);
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

COUNT(UDP_INPUT);
	/*
	 * Get IP and UDP header together in first mbuf.
	 */
	m = m0;
	if ((m->m_off > MMAXOFF || m->m_len < sizeof (struct udpiphdr)) &&
	    (m = m_pullup(m, sizeof (struct udpiphdr))) == 0) {
		udpstat.udps_hdrops++;
		return;
	}
	ui = mtod(m, struct udpiphdr *);
	if (((struct ip *)ui)->ip_hl > (sizeof (struct ip) >> 2))
		ip_stripoptions((struct ip *)ui, (struct mbuf *)0);

	/*
	 * Make mbuf data length reflect UDP length.
	 * If not enough data to reflect UDP length, drop.
	 */
	ulen = ntohs((u_short)ui->ui_ulen);
	len = sizeof (struct udphdr) + ulen;
	if (((struct ip *)ui)->ip_len != len) {
		if (len > ((struct ip *)ui)->ip_len) {
			udpstat.udps_badlen++;
			goto bad;
		}
		m_adj(m, ((struct ip *)ui)->ip_len - len);
		/* (struct ip *)ui->ip_len = len; */
	}

	/*
	 * Checksum extended UDP header and data.
	 */
	if (udpcksum) {
		ui->ui_next = ui->ui_prev = 0;
		ui->ui_x1 = 0;
		ui->ui_len = htons((u_short)(sizeof (struct udphdr) + ulen));
		if (ui->ui_sum = in_cksum(m, len)) {
			udpstat.udps_badsum++;
			printf("udp cksum %x\n", ui->ui_sum);
			m_freem(m);
			return;
		}
	}

	/*
	 * Locate pcb for datagram.  On wildcard match, update
	 * control block to anchor network and host address.
	 */
	inp = in_pcblookup(&udb,
	    ui->ui_src, ui->ui_sport, ui->ui_dst, ui->ui_dport,
		INPLOOKUP_WILDCARD);
	if (inp == 0) {
		struct in_addr broadcastaddr;

		broadcastaddr = if_makeaddr(ui->ui_dst.s_net, INADDR_ANY);
		if (ui->ui_dst.s_addr == broadcastaddr.s_addr)
			goto bad;
		icmp_error((struct ip *)ui, ICMP_UNREACH, ICMP_UNREACH_PORT);
		return;
	}

	/*
	 * Construct sockaddr format source address.
	 * Stuff source address and datagram in user buffer.
	 */
	udp_in.sin_port = ui->ui_sport;
	udp_in.sin_addr = ui->ui_src;
	m->m_len -= sizeof (struct udpiphdr);
	m->m_off += sizeof (struct udpiphdr);
	if (sbappendaddr(&inp->inp_socket->so_rcv, (struct sockaddr *)&udp_in, m) == 0)
		goto bad;
	sorwakeup(inp->inp_socket);
	return;
bad:
	m_freem(m);
}

udp_abort(inp)
	struct inpcb *inp;
{
	struct socket *so = inp->inp_socket;

	in_pcbdisconnect(inp);
	soisdisconnected(so);
}

udp_ctlinput(cmd, arg)
	int cmd;
	caddr_t arg;
{
	struct in_addr *sin;
	extern u_char inetctlerrmap[];
COUNT(UDP_CTLINPUT);

	if (cmd < 0 || cmd > PRC_NCMDS)
		return;
	switch (cmd) {

	case PRC_ROUTEDEAD:
		break;

	case PRC_QUENCH:
		break;

	/* these are handled by ip */
	case PRC_IFDOWN:
	case PRC_HOSTDEAD:
	case PRC_HOSTUNREACH:
		break;

	default:
		sin = &((struct icmp *)arg)->icmp_ip.ip_dst;
		in_pcbnotify(&udb, sin, inetctlerrmap[cmd], udp_abort);
	}
}

udp_output(inp, m0)
	struct inpcb *inp;
	struct mbuf *m0;
{
	register struct mbuf *m;
	register struct udpiphdr *ui;
	register struct socket *so;
	register int len = 0;

COUNT(UDP_OUTPUT);
	/*
	 * Calculate data length and get a mbuf
	 * for UDP and IP headers.
	 */
	for (m = m0; m; m = m->m_next)
		len += m->m_len;
	m = m_get(M_DONTWAIT);
	if (m == 0) {
		m_freem(m0);
		return (ENOBUFS);
	}

	/*
	 * Fill in mbuf with extended UDP header
	 * and addresses and length put into network format.
	 */
	m->m_off = MMAXOFF - sizeof (struct udpiphdr);
	m->m_len = sizeof (struct udpiphdr);
	m->m_next = m0;
	ui = mtod(m, struct udpiphdr *);
	ui->ui_next = ui->ui_prev = 0;
	ui->ui_x1 = 0;
	ui->ui_pr = IPPROTO_UDP;
	ui->ui_len = sizeof (struct udpiphdr) + len;
	ui->ui_src = inp->inp_laddr;
	ui->ui_dst = inp->inp_faddr;
	ui->ui_sport = inp->inp_lport;
	ui->ui_dport = inp->inp_fport;
	ui->ui_ulen = len;
#if vax
	ui->ui_ulen = htons(ui->ui_ulen);
#endif

	/*
	 * Stuff checksum and output datagram.
	 */
	ui->ui_sum = 0;
	ui->ui_sum = in_cksum(m, sizeof (struct udpiphdr) + len);
	((struct ip *)ui)->ip_len = sizeof (struct udpiphdr) + len;
	((struct ip *)ui)->ip_ttl = MAXTTL;
	so = inp->inp_socket;
	return (ip_output(m, (struct mbuf *)0,
	    (so->so_options & SO_DONTROUTE) ? &routetoif : (struct route *)0,
	    so->so_state & SS_PRIV));
}

udp_usrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	caddr_t addr;
{
	struct inpcb *inp = sotoinpcb(so);
	int error = 0;

COUNT(UDP_USRREQ);
	if (inp == 0 && req != PRU_ATTACH)
		return (EINVAL);
	switch (req) {

	case PRU_ATTACH:
		if (inp != 0)
			return (EINVAL);
		error = in_pcbattach(so, &udb, 2048, 2048,
				(struct sockaddr_in *)addr);
		break;

	case PRU_DETACH:
		if (inp == 0)
			return (ENOTCONN);
		in_pcbdetach(inp);
		break;

	case PRU_CONNECT:
		if (inp->inp_faddr.s_addr)
			return (EISCONN);
		error = in_pcbconnect(inp, (struct sockaddr_in *)addr);
		if (error == 0)
			soisconnected(so);
		break;

	case PRU_ACCEPT:
		return (EOPNOTSUPP);

	case PRU_DISCONNECT:
		if (inp->inp_faddr.s_addr == 0)
			return (ENOTCONN);
		in_pcbdisconnect(inp);
		soisdisconnected(so);
		break;

	case PRU_SHUTDOWN:
		socantsendmore(so);
		break;

	case PRU_SEND: {
		struct in_addr laddr;

		if (addr) {
			laddr = inp->inp_laddr;
			if (inp->inp_faddr.s_addr)
				return (EISCONN);
			error = in_pcbconnect(inp, (struct sockaddr_in *)addr);
			if (error)
				break;
		} else {
			if (inp->inp_faddr.s_addr == 0)
				return (ENOTCONN);
		}
		error = udp_output(inp, m);
		if (addr) {
			in_pcbdisconnect(inp);
			inp->inp_laddr = laddr;
		}
		}
		break;

	case PRU_ABORT:
		in_pcbdetach(inp);
		sofree(so);
		soisdisconnected(so);
		break;

	case PRU_CONTROL:
		return (EOPNOTSUPP);

	case PRU_SOCKADDR:
		in_setsockaddr((struct sockaddr_in *)addr, inp);
		break;

	default:
		panic("udp_usrreq");
	}
	return (error);
}
