/*	tcp_output.c	4.22	81/12/12	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/in.h"
#include "../net/in_pcb.h"
#include "../net/in_systm.h"
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../net/tcp.h"
#define	TCPOUTFLAGS
#include "../net/tcp_fsm.h"
#include "../net/tcp_seq.h"
#include "../net/tcp_timer.h"
#include "../net/tcp_var.h"
#include "../net/tcpip.h"
#include "../errno.h"

char *tcpstates[]; /* XXX */
/*
 * Tcp output routine: figure out what should be sent and send it.
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
	 * Determine length of data that can be transmitted,
	 * and flags that will be used.
	 * If there is some data or critical controls (SYN, RST)
	 * to send, then transmit; otherwise, investigate further.
	 */
	off = tp->snd_nxt - tp->snd_una;
	len = MIN(so->so_snd.sb_cc, tp->snd_wnd+tp->t_force) - off;
	if (len > tp->t_maxseg)
		len = tp->t_maxseg;
	if (len < 0)
		len = 0;		/* FIN can cause -1 */
	flags = tcp_outflags[tp->t_state];
	if (len < so->so_snd.sb_cc)
		flags &= ~TH_FIN;
	if (len || (flags & (TH_SYN|TH_RST)))
		goto send;

	/*
	 * See if we owe peer an ACK or have a unacked FIN to send.
	 */
	if (tp->t_flags & TF_ACKNOW)
		goto send;
	if ((so->so_state & SS_CANTSENDMORE) &&
	    TCPS_OURFINNOTACKED(tp->t_state))
		goto send;

	/*
	 * Calculate available window in i, and also amount
	 * of window known to peer (as advertised window less
	 * next expected input.)  If this is 35% or more of the
	 * maximum possible window, then want to send a segment to peer.
	 */
	win = sbspace(&so->so_rcv);
	if (win > 0 &&
	    ((100*(win-(tp->rcv_adv-tp->rcv_nxt))/so->so_rcv.sb_hiwat) >= 35))
		goto send;

	/*
	 * No reason to send a segment, just return.
	 */
printf("tcp_output: nothing to send\n");
	return (0);

send:
	/*
	 * Grab a header mbuf, attaching a copy of data to
	 * be transmitted, and initialize the header from
	 * the template for sends on this connection.
	 */
	MGET(m, 0);
	if (m == 0)
		return (0);
	m->m_off = MMAXOFF - sizeof (struct tcpiphdr);
	m->m_len = sizeof (struct tcpiphdr);
	if (len) {
		m->m_next = m_copy(so->so_snd.sb_mb, off, len);
		if (m->m_next == 0)
			len = 0;
if (m->m_next) printf("copy *mtod()=%x\n", *mtod(m->m_next, char *));
	}
	ti = mtod(m, struct tcpiphdr *);
	if (tp->t_template == 0)
		panic("tcp_output");
	bcopy((caddr_t)tp->t_template, (caddr_t)ti, sizeof (struct tcpiphdr));

	/*
	 * Fill in fields, remembering maximum advertised
	 * window for use in delaying messages about window sizes.
	 */
	ti->ti_seq = tp->snd_nxt;
	ti->ti_ack = tp->rcv_nxt;
#if vax
	ti->ti_seq = htonl(ti->ti_seq);
	ti->ti_ack = htonl(ti->ti_ack);
#endif
	if (tp->t_tcpopt) {
		m0 = m->m_next;
		m->m_next = m_get(0);
		if (m->m_next == 0) {
			(void) m_free(m);
			m_freem(m);
			return (0);
		}
		m->m_next->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = tp->t_tcpopt->m_len;
		bcopy(mtod(tp->t_tcpopt, caddr_t), mtod(m, caddr_t),
		    (unsigned)tp->t_tcpopt->m_len);
		ti->ti_off = (sizeof (struct tcphdr)+tp->t_tcpopt->m_len) >> 2;
	}
	ti->ti_flags = flags;
	win = sbspace(&so->so_rcv);
	if (win > 0)
		ti->ti_win = htons((u_short)win);
	if (SEQ_GT(tp->snd_up, tp->snd_nxt)) {
		ti->ti_urp = htons((u_short)(tp->snd_up - tp->snd_nxt));
		ti->ti_flags |= TH_URG;
	} else
		/*
		 * If no urgent pointer to send, then we pull
		 * the urgent pointer to the left edge of the send window
		 * so that it doesn't drift into the send window on sequence
		 * number wraparound.
		 */
		tp->snd_up = tp->snd_una;		/* drag it along */
	/* PUSH */

	/*
	 * Put TCP length in extended header, and then
	 * checksum extended header and data.
	 */
	if (len)
		ti->ti_len = htons((u_short)(len + sizeof (struct tcphdr)));
	ti->ti_sum = in_cksum(m, sizeof (struct tcpiphdr) + len);

printf("tcp_output: ti %x flags %x seq %x ack %x win %d len %d sum %x\n",
ti, ti->ti_flags, htonl(ti->ti_seq), htonl(ti->ti_ack), htons(ti->ti_win), ti->ti_len, ti->ti_sum);

	/*
	 * Advance snd_nxt over sequence space of this segment
	 */
	if (flags & (TH_SYN|TH_FIN))
		tp->snd_nxt++;
	tp->snd_nxt += len;

	/*
	 * If this transmission closes the window,
	 * start persistance timer at 2 round trip times
	 * but at least TCPTV_PERSMIN ticks.
	 */
	if (SEQ_GT(tp->snd_nxt, tp->snd_una+tp->snd_wnd) &&
	    tp->t_timer[TCPT_PERSIST] == 0)
		TCPT_RANGESET(tp->t_timer[TCPT_PERSIST],
		    2 * tp->t_srtt, TCPTV_PERSMIN, TCPTV_MAX);

	/*
	 * Time this transmission if not a retransmission and
	 * not currently timing anything.
	 */
	if (SEQ_GT(tp->snd_nxt, tp->snd_max) && tp->t_rtt == 0) {
		tp->t_rtt = 1;
		tp->t_rtseq = tp->snd_nxt - len;
	}

	/*
	 * Set retransmit timer if not currently set.
	 * Initial value for retransmit timer to tcp_beta*tp->t_srtt.
	 * Initialize shift counter which is used for exponential
	 * backoff of retransmit time.
	 */
	if (tp->t_timer[TCPT_REXMT] == 0 && tp->snd_nxt != tp->snd_una) {
		TCPT_RANGESET(tp->t_timer[TCPT_REXMT],
		    tcp_beta * tp->t_srtt, TCPTV_MIN, TCPTV_MAX);
printf("rexmt timer set to %d\n", tp->t_timer[TCPT_REXMT]);
		tp->t_rxtshift = 0;
	}
else printf("REXMT timer is already %d, snd_nxt %x snd_una %x\n", tp->t_timer[TCPT_REXMT], tp->snd_nxt, tp->snd_una);

	/*
	 * Fill in IP length and desired time to live and
	 * send to IP level.
	 */
	((struct ip *)ti)->ip_len = len + sizeof (struct tcpiphdr);
	((struct ip *)ti)->ip_ttl = TCP_TTL;
	if (ip_output(m, tp->t_ipopt) == 0) {
printf("ip_output failed\n");
		return (0);
	}

	/*
	 * Data sent (as far as we can tell).
	 * If this advertises a larger window than any other segment,
	 * then remember the size of the advertised window.
	 * Drop send for purpose of ACK requirements.
	 */
	if (win > 0 && SEQ_GT(tp->rcv_nxt+win, tp->rcv_adv)) {
		tp->rcv_adv = tp->rcv_nxt + win;
	}
	tp->t_flags &= ~(TF_ACKNOW|TF_DELACK);
	if (SEQ_GT(tp->snd_nxt, tp->snd_max))
		tp->snd_max = tp->snd_nxt;
	return (1);
}
