/*	tcp_output.c	6.8	85/05/27	*/

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "errno.h"

#include "../net/route.h"

#include "in.h"
#include "in_pcb.h"
#include "in_systm.h"
#include "ip.h"
#include "ip_var.h"
#include "tcp.h"
#define	TCPOUTFLAGS
#include "tcp_fsm.h"
#include "tcp_seq.h"
#include "tcp_timer.h"
#include "tcp_var.h"
#include "tcpip.h"
#include "tcp_debug.h"

/*
 * Initial options.
 */
u_char	tcp_initopt[4] = { TCPOPT_MAXSEG, 4, 0x0, 0x0, };

/*
 * Tcp output routine: figure out what should be sent and send it.
 */
tcp_output(tp)
	register struct tcpcb *tp;
{
	register struct socket *so = tp->t_inpcb->inp_socket;
	register int len;
	struct mbuf *m0;
	int off, flags, win, error;
	register struct mbuf *m;
	register struct tcpiphdr *ti;
	u_char *opt;
	unsigned optlen = 0;
	int sendalot;

	/*
	 * Determine length of data that should be transmitted,
	 * and flags that will be used.
	 * If there is some data or critical controls (SYN, RST)
	 * to send, then transmit; otherwise, investigate further.
	 */
again:
	sendalot = 0;
	off = tp->snd_nxt - tp->snd_una;
	win = MIN(tp->snd_wnd, tp->snd_cwnd);
	/*
	 * If in persist timeout with window of 0, send 1 byte.
	 * Otherwise, window is small but nonzero
	 * and timer expired, go to transmit state.
	 */
	if (tp->t_force) {
		if (win == 0) 
			win = 1;
		else {
			tp->t_timer[TCPT_PERSIST] = 0;
			tp->t_rxtshift = 0;
		}
	}
	len = MIN(so->so_snd.sb_cc, win) - off;
	if (len < 0)
		return (0);	/* ??? */	/* past FIN */
	if (len > tp->t_maxseg) {
		len = tp->t_maxseg;
		/*
		 * Don't send more than one segment if retransmitting
		 * (or persisting, but then we shouldn't be here).
		 */
		if (tp->t_rxtshift == 0)
			sendalot = 1;
	}

	win = sbspace(&so->so_rcv);
	flags = tcp_outflags[tp->t_state];
	if (tp->snd_nxt + len < tp->snd_una + so->so_snd.sb_cc)
		flags &= ~TH_FIN;
	if (flags & (TH_SYN|TH_RST|TH_FIN))
		goto send;
	if (SEQ_GT(tp->snd_up, tp->snd_una))
		goto send;

	/*
	 * Sender silly window avoidance.  If connection is idle
	 * and can send all data, a maximum segment,
	 * at least a maximum default-size segment do it,
	 * or are forced, do it; otherwise don't bother.
	 * If retransmitting (possibly after persist timer forced us
	 * to send into a small window), then must resend.
	 */
	if (len) {
		if (len == tp->t_maxseg || len >= so->so_snd.sb_cc) /* off = 0*/
			goto send;
		if (len >= TCP_MSS)	/* a lot */
			goto send;
		if (tp->t_force)
			goto send;
		if (SEQ_LT(tp->snd_nxt, tp->snd_max))
			goto send;
	}

	/*
	 * Send if we owe peer an ACK.
	 */
	if (tp->t_flags&TF_ACKNOW)
		goto send;


	/*
	 * Calculate available window, and also amount
	 * of window known to peer (as advertised window less
	 * next expected input.)  If the difference is 35% or more of the
	 * maximum possible window, then want to send a window update to peer.
	 */
	win = sbspace(&so->so_rcv);
	if (win > 0 &&
	    ((100*(win-(tp->rcv_adv-tp->rcv_nxt))/so->so_rcv.sb_hiwat) >= 35))
		goto send;

	/*
	 * TCP window updates are not reliable, rather a polling protocol
	 * using ``persist'' packets is used to insure receipt of window
	 * updates.  The three ``states'' for the output side are:
	 *	idle			not doing retransmits or persists
	 *	persisting		to move a zero window
	 *	(re)transmitting	and thereby not persisting
	 *
	 * tp->t_timer[TCPT_PERSIST]
	 *	is set when we are in persist state.
	 * tp->t_force
	 *	is set when we are called to send a persist packet.
	 * tp->t_timer[TCPT_REXMT]
	 *	is set when we are retransmitting
	 * The output side is idle when both timers are zero.
	 *
	 * If send window is too small, there is data to transmit, and no
	 * retransmit or persist is pending, then go to persist state.
	 * If nothing happens soon, send when timer expires:
	 * if window is nonzero, transmit what we can,
	 * otherwise force out a byte.
	 */
	if (so->so_snd.sb_cc && tp->t_timer[TCPT_REXMT] == 0 &&
	    tp->t_timer[TCPT_PERSIST] == 0) {
		tp->t_rxtshift = 0;
		tcp_setpersist(tp);
	}

	/*
	 * No reason to send a segment, just return.
	 */
	return (0);

send:
	/*
	 * Grab a header mbuf, attaching a copy of data to
	 * be transmitted, and initialize the header from
	 * the template for sends on this connection.
	 */
	MGET(m, M_DONTWAIT, MT_HEADER);
	if (m == NULL)
		return (ENOBUFS);
	m->m_off = MMAXOFF - sizeof (struct tcpiphdr);
	m->m_len = sizeof (struct tcpiphdr);
	if (len) {
		m->m_next = m_copy(so->so_snd.sb_mb, off, len);
		if (m->m_next == 0)
			len = 0;
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
	ti->ti_seq = htonl(ti->ti_seq);
	ti->ti_ack = htonl(ti->ti_ack);
	/*
	 * Before ESTABLISHED, force sending of initial options
	 * unless TCP set to not do any options.
	 */
	if (tp->t_state < TCPS_ESTABLISHED) {
		int mss;

		if (tp->t_flags&TF_NOOPT)
			goto noopt;
		mss = MIN(so->so_rcv.sb_hiwat / 2, tcp_mss(tp));
		if (mss <= IP_MSS - sizeof(struct tcpiphdr))
			goto noopt;
		opt = tcp_initopt;
		optlen = sizeof (tcp_initopt);
		*(u_short *)(opt + 2) = htons(mss);
	} else {
		if (tp->t_tcpopt == 0)
			goto noopt;
		opt = mtod(tp->t_tcpopt, u_char *);
		optlen = tp->t_tcpopt->m_len;
	}
	if (opt) {
		m0 = m->m_next;
		m->m_next = m_get(M_DONTWAIT, MT_DATA);
		if (m->m_next == 0) {
			(void) m_free(m);
			m_freem(m0);
			return (ENOBUFS);
		}
		m->m_next->m_next = m0;
		m0 = m->m_next;
		m0->m_len = optlen;
		bcopy((caddr_t)opt, mtod(m0, caddr_t), optlen);
		opt = (u_char *)(mtod(m0, caddr_t) + optlen);
		while (m0->m_len & 0x3) {
			*opt++ = TCPOPT_EOL;
			m0->m_len++;
		}
		optlen = m0->m_len;
		ti->ti_off = (sizeof (struct tcphdr) + optlen) >> 2;
	}
noopt:
	ti->ti_flags = flags;
	if (win >= so->so_rcv.sb_hiwat / 4)	/* avoid silly window */
		ti->ti_win = htons((u_short)win);
	if (SEQ_GT(tp->snd_up, tp->snd_nxt)) {
		ti->ti_urp = tp->snd_up - tp->snd_nxt;
		ti->ti_urp = htons(ti->ti_urp);
		ti->ti_flags |= TH_URG;
	} else
		/*
		 * If no urgent pointer to send, then we pull
		 * the urgent pointer to the left edge of the send window
		 * so that it doesn't drift into the send window on sequence
		 * number wraparound.
		 */
		tp->snd_up = tp->snd_una;		/* drag it along */
	/*
	 * If anything to send and we can send it all, set PUSH.
	 * (This will keep happy those implementations which only
	 * give data to the user when a buffer fills or a PUSH comes in.)
	 */
	if (len && off+len == so->so_snd.sb_cc)
		ti->ti_flags |= TH_PUSH;

	/*
	 * Put TCP length in extended header, and then
	 * checksum extended header and data.
	 */
	if (len + optlen) {
		ti->ti_len = sizeof (struct tcphdr) + optlen + len;
		ti->ti_len = htons((u_short)ti->ti_len);
	}
	ti->ti_sum = in_cksum(m, sizeof (struct tcpiphdr) + (int)optlen + len);

	/*
	 * In transmit state, time the transmission and arrange for
	 * the retransmit.  In persist state, just set snd_max.
	 */
	if (tp->t_force == 0 || tp->t_timer[TCPT_PERSIST] == 0) {
		/*
		 * Advance snd_nxt over sequence space of this segment.
		 */
		if (flags & (TH_SYN|TH_FIN))
			tp->snd_nxt++;
		tp->snd_nxt += len;
		if (SEQ_GT(tp->snd_nxt, tp->snd_max)) {
			tp->snd_max = tp->snd_nxt;
			/*
			 * Time this transmission if not a retransmission and
			 * not currently timing anything.
			 */
			if (tp->t_rtt == 0) {
				tp->t_rtt = 1;
				tp->t_rtseq = tp->snd_nxt - len;
			}
		}

		/*
		 * Set retransmit timer if not currently set,
		 * and not doing a keep-alive probe.
		 * Initial value for retransmit timer to tcp_beta*tp->t_srtt.
		 * Initialize shift counter which is used for exponential
		 * backoff of retransmit time.
		 */
		if (tp->t_timer[TCPT_REXMT] == 0 &&
		    tp->snd_nxt != tp->snd_una) {
			TCPT_RANGESET(tp->t_timer[TCPT_REXMT],
			    tcp_beta * tp->t_srtt, TCPTV_MIN, TCPTV_MAX);
			tp->t_rxtshift = 0;
		}
		tp->t_timer[TCPT_PERSIST] = 0;
	} else {
		if (SEQ_GT(tp->snd_una+1, tp->snd_max))
			tp->snd_max = tp->snd_una+1;
	}

	/*
	 * Trace.
	 */
	if (so->so_options & SO_DEBUG)
		tcp_trace(TA_OUTPUT, tp->t_state, tp, ti, 0);

	/*
	 * Fill in IP length and desired time to live and
	 * send to IP level.
	 */
	((struct ip *)ti)->ip_len = sizeof (struct tcpiphdr) + optlen + len;
	((struct ip *)ti)->ip_ttl = TCP_TTL;
	if (so->so_options & SO_DONTROUTE)
		error =
		   ip_output(m, tp->t_ipopt, (struct route *)0, IP_ROUTETOIF);
	else
		error = ip_output(m, tp->t_ipopt, &tp->t_inpcb->inp_route, 0);
	if (error)
		return (error);

	/*
	 * Data sent (as far as we can tell).
	 * If this advertises a larger window than any other segment,
	 * then remember the size of the advertised window.
	 * Drop send for purpose of ACK requirements.
	 */
	if (win > 0 && SEQ_GT(tp->rcv_nxt+win, tp->rcv_adv))
		tp->rcv_adv = tp->rcv_nxt + win;
	tp->t_flags &= ~(TF_ACKNOW|TF_DELACK);
	if (sendalot && tp->t_force == 0)
		goto again;
	return (0);
}

tcp_setpersist(tp)
	register struct tcpcb *tp;
{

	if (tp->t_timer[TCPT_REXMT])
		panic("tcp_output REXMT");
	/*
	 * Start/restart persistance timer.
	 */
	TCPT_RANGESET(tp->t_timer[TCPT_PERSIST],
	    ((int)(tcp_beta * tp->t_srtt)) << tp->t_rxtshift,
	    TCPTV_PERSMIN, TCPTV_MAX);
	tp->t_rxtshift++;
	if (tp->t_rxtshift >= TCP_MAXRXTSHIFT)
		tp->t_rxtshift = 0;
}
