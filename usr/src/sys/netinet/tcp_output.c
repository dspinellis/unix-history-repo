/*	tcp_output.c	4.18	81/11/25	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/inet.h"
#include "../net/inet_pcb.h"
#include "../net/inet_systm.h"
#include "../net/imp.h"
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../net/tcp.h"
#include "../net/tcp_var.h"
#include "../net/tcp_fsm.h"
#include "/usr/include/errno.h"

/*
 * Tcp output routine: figure out what should be sent
 * and, if nothing, send a null segment anyways if force is nonzero
 * (e.g. to be sure to send an ACK).
 *
 * This routine can be called only after SYNs have been exchanged.
 */
tcp_output(tp)
	register struct tcpcb *tp;
{
	register struct socket *so = tp->t_inpcb->inp_socket;
	register int len;
	struct mbuf *m0;
	int off, flags;
	register struct mbuf *m;
	register struct tcpiphdr *ti;
	int win;

COUNT(TCP_OUTPUT);

	/*
	 * Determine length of data that can be transmitted.
	 * If will transmit to end of data and no more data
	 * is coming, then send FIN also.
	 * Make a copy of the data (if any).  If no data
	 * and not forced to transmit, just return.
	 */
	off = tp->snd_nxt - tp->snd_una;
	len = MIN(so->so_snd.sb_cc, tp->snd_wnd) - off;
	if (len > tp->mtu)
		len = tp->mtu;
	if (len == so->so_snd.sb_cc && (so->so_state & SS_CANTSNDMORE))
		flags = TH_FIN;
	else
		flags = 0;
	if (len)
		goto send;

	/*
	 * No data to send: see if something else makes us want to send.
	 * First check if we owe peer and ack or have a unacked FIN to send.
	 */
	if (tp->t_flags & TF_OWEACK)
		goto send;
	if ((so->so_state & SS_CANTSNDMORE) &&
	    TCPS_OURFINISACKED(tp->t_state) == 0)
		goto send;
	if (tp->t_state == TCPS_SYN_SENT) {
		flags = TH_SYN;
		goto send;
	}
	if (tp->t_state == TCPS_CLOSED) {
		flags = TH_RST;
		goto send;
	}

	/*
	 * Calculate available window in i, and also amount
	 * of window known to peer (as advertised window less
	 * next expected input.)  If this is 35% or more of the
	 * maximum possible window, then want to send a segment to peer.
	 */
	i = sbspace(&so->so_rcv) - tp->seqcnt;
	if (i > 0 &&
	    ((100*(i-(tp->rcv_adv-tp->rcv_nxt))/so->so_rcv.sb_hiwat) >= 35))
		goto send;

	/*
	 * No reason to send a segment, just return.
	 */
	return;

send:
	/*
	 * Grab a header mbuf, attaching a copy of data to
	 * be transmitted, and initialize the header from
	 * the template for sends on this connection.
	 */
	MGET(m, 0);
	if (m == 0)
		return (0);
	m->m_off = MMAXOFF - sizeof(struct tcpiphdr);
	m->m_len = sizeof (struct tcpiphdr);
	if (len) {
		m->m_next = m_copy(so->so_snd.sb_mb, off, len);
		if (m->m_next == 0)
			len = 0;
	}
	ti = mtod(m, struct tcpiphdr *);
	if (tp->t_template == 0)
		panic("tcp_output");
	bcopy((caddr_t)tp->t_template, ti, sizeof (struct tcpiphdr));

	/*
	 * Fill in fields, remembering maximum advertised
	 * window for use in delaying messages about window sizes.
	 */
	ti->ti_seq = htonl(tp->snd_nxt);
	ti->ti_ackno = htonl(tp->rcv_nxt);
	/* OPTIONS */
	if (flags & TH_SYN)
		ti->ti_flags = flags;
	else
		ti->ti_flags = flags | TH_ACK;
	win = sbspace(&so->so_rcv);
	if (win > 0)
		ti->ti_win = htons(win);
	if (SEQ_GT(tp->snd_urp, tp->snd_nxt))
		ti->ti_urp = htons((u_short)(tp->snd_urp - tp->snd_nxt));
		ti->ti_flags |= TH_URG;
	} else
		/*
		 * If no urgent pointer to send, then we pull
		 * the urgent pointer to the left edge of the send window
		 * so that it doesn't drift into the send window on sequence
		 * number wraparound.
		 */
		tp->snd_urp = tp->snd_una;		/* drag it along */

	/*
	 * Put TCP length in extended header, and then
	 * checksum extended header and data.
	 */
	if (len)
		ti->ti_len = htons((u_short)(len + sizeof (struct tcphdr)));
	ti->ti_sum = inet_cksum(m, sizeof (struct tcpiphdr) + len);

	/*
	 * Fill in IP length and desired time to live and
	 * send to IP level.
	 */
	((struct ip *)ti)->ip_len = len + sizeof (struct tcpiphdr);
	((struct ip *)ti)->ip_ttl = TCP_TTL;
	if (ip_output(m) == 0)
		return;

	/*
	 * Data sent (as far as we can tell).
	 * If this advertises a larger window than any other segment,
	 * then record its sequence to be used in suppressing messages.
	 * Advance snd_nxt to reflect transmitted sequence space,
	 * drop send for purpose of ACK requirements,
	 * and time transmission if not a retransmit.
	 */
	if (win > 0 && SEQ_GT(tp->rcv_nxt+win, tp->rcv_adv))
		tp->rcv_adv = tp->rcv_nxt + win;
	tp->snd_nxt += len;
	tp->t_flags &= ~(TF_OWEACK|TF_DELACK);
	if (flags & TH_FIN)
		tp->snd_nxt++;
	if (SEQ_GT(tp->snd_nxt, tp->snd_hi)) {
		tp->snd_hi = tp->snd_nxt;
		/* TIME TRANSMIT */
	}
	return;
}
