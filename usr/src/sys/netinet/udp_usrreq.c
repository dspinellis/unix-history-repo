/*	udp_usrreq.c	4.17	81/12/12	*/

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/in.h"
#include "../net/in_pcb.h"
#include "../net/in_systm.h"
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
	if (m->m_len < sizeof (struct udpiphdr) &&
	    m_pullup(m, sizeof (struct udpiphdr)) == 0) {
		udpstat.udps_hdrops++;
		goto bad;
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
	 * Locate pcb for datagram.
	 */
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
	m->m_len -= sizeof (struct udpiphdr);
	m->m_off += sizeof (struct udpiphdr);
	if (sbappendaddr(&inp->inp_socket->so_rcv, (struct sockaddr *)&udp_in, m) == 0)
		goto bad;
	sorwakeup(inp->inp_socket);
	return;
bad:
	m_freem(m);
}

udp_ctlinput(m)
	struct mbuf *m;
{

COUNT(UDP_CTLINPUT);
	m_freem(m);
}

udp_output(inp, m0)
	struct inpcb *inp;
	struct mbuf *m0;
{
	register struct mbuf *m;
	register struct udpiphdr *ui;
	register int len = 0;

COUNT(UDP_OUTPUT);
	/*
	 * Calculate data length and get a mbuf
	 * for UDP and IP headers.
	 */
	for (m = m0; m; m = m->m_next)
		len += m->m_len;
	m = m_get(0);
	if (m == 0)
		goto bad;

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
	ui->ui_ulen = htons((u_short)len);

	/*
	 * Stuff checksum and output datagram.
	 */
	ui->ui_sum = 0;
	ui->ui_sum = in_cksum(m, sizeof (struct udpiphdr) + len);
	((struct ip *)ui)->ip_len = sizeof (struct udpiphdr) + len;
	((struct ip *)ui)->ip_ttl = MAXTTL;
	(void) ip_output(m, (struct mbuf *)0);
	return;
bad:
	m_freem(m);
}

udp_usrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	caddr_t addr;
{
	struct inpcb *inp = sotoinpcb(so);
	int error;

COUNT(UDP_USRREQ);
	if (inp == 0 && req != PRU_ATTACH)
		return (EINVAL);
	switch (req) {

	case PRU_ATTACH:
		if (inp != 0)
			return (EINVAL);
		error = in_pcbattach(so, &udb, 2048, 2048, (struct sockaddr_in *)addr);
		if (error)
			return (error);
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
		if (error)
			return (error);
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

	case PRU_SEND:
		if (addr) {
			if (inp->inp_faddr.s_addr)
				return (EISCONN);
			error = in_pcbconnect(inp, (struct sockaddr_in *)addr);
			if (error)
				return (error);
		} else {
			if (inp->inp_faddr.s_addr == 0)
				return (ENOTCONN);
		}
		udp_output(inp, m);
		if (addr)
			in_pcbdisconnect(inp);
		break;

	case PRU_ABORT:
		in_pcbdetach(inp);
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
