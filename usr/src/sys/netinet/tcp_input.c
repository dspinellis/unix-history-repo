/*	tcp_input.c	1.31	81/11/25	*/

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
#include "../net/tcp_fsm.h"
#include "../net/tcp_var.h"
#include "/usr/include/errno.h"

int	tcpcksum = 1;

struct	sockaddr_in tcp_sockaddr = { AF_INET };

/*
 * TCP input routine, follows pages 65-76 of the
 * protocol specification dated September, 1981 very closely.
 */
tcp_input(m0)
	struct mbuf *m0;
{
	register struct tcpiphdr *ti;
	struct inpcb *inp;
	register struct mbuf *m;
	int len, tlen, off;
	register struct tcpcb *tp;
	register int tiflags;
	struct socket *so;
	seq_t segend;
	int acceptable;

COUNT(TCP_INPUT);
	/*
	 * Get ip and tcp header together in first mbuf.
	 */
	m = m0;
	if (m->m_len < sizeof (struct tcpiphdr) &&
	    m_pullup(m, sizeof (struct tcpiphdr)) == 0) {
		tcpstat.tcps_hdrops++;
		goto bad;
	}
	ti = mtod(m, struct tcpiphdr *);
	if (ti->ti_len > sizeof (struct ip))
		ip_stripoptions((struct ip *)ti, (char *)0);

	/*
	 * Checksum extended tcp header and data.
	 */
	tlen = ((struct ip *)ti)->ip_len;
	len = sizeof (struct ip) + tlen;
	if (tcpcksum) {
		ti->ti_next = ti->ti_prev = 0;
		ti->ti_x1 = 0;
		ti->ti_len = htons((u_short)tlen);
		if ((ti->ti_sum = inet_cksum(m, len)) != 0xffff) {
			tcpstat.tcps_badsum++;
			printf("tcp cksum %x\n", ti->ti_sum);
			goto bad;
		}
	}

	/*
	 * Check that tcp offset makes sense,
	 * process tcp options and adjust length.
	 */
	off = ti->ti_off << 2;
	if (off < sizeof (struct tcphdr) || off > ti->ti_len) {
		tcpstat.tcps_badoff++;
		goto bad;
	}
	ti->ti_len = tlen - off;
	/* PROCESS OPTIONS */
	tiflags = ti->ti_flags;

	/*
	 * Locate pcb for segment.
	 */
	inp = in_pcblookup
		(&tcb, ti->ti_src, ti->ti_sport, ti->ti_dst, ti->ti_dport);

	/*
	 * If the state is CLOSED (i.e., TCB does not exist) then
	 * all data in the incoming segment is discarded.  (p. 65).
	 */
	if (inp == 0)
		goto sendreset;
	tp = intotcpcb(inp);
	if (tp == 0)
		goto sendreset;

	/*
	 * Convert tcp protocol specific fields to host format.
	 */
	ti->ti_seq = ntohl(ti->ti_seq);
	ti->ti_ackno = ntohl((n_long)ti->ti_ackno);
	ti->ti_win = ntohs(ti->ti_win);
	ti->ti_urp = ntohs(ti->ti_urp);

	/*
	 * Discard ip header, and do tcp input processing.
	 */
	off += sizeof (struct ip);
	m->m_off += off;
	m->m_len -= off;

	switch (tp->t_state) {

	/*
	 * If the state is LISTEN then ignore segment if it contains an RST.
	 * If the segment contains an ACK then it is bad and send a RST.
	 * If it does not contain a SYN then it is not interesting; drop it.
	 * Otherwise initialize tp->rcv_next, and tp->irs, select an initial
	 * tp->iss, and send a segment:
	 *     <SEQ=ISS><ACK=RCV>NXT><CTL=SYN,ACK>
	 * Also initialize tp->snd_nxt to tp->iss+1 and tp->snd_una to tp->iss.
	 * Fill in remote peer address fields if not previously specified.
	 * Enter SYN_RECEIVED state, and process any other fields of this
	 * segment in this state.  (p. 65)
	 */
	case TCPS_LISTEN:
		if (tiflags & TH_RST)
			goto drop;
		if (tiflags & TH_ACK)
			goto sendrst;
		if ((tiflags & TH_SYN) == 0)
			goto drop;
		tp->rcv_nxt = ti->ti_seq + 1;
		tp->irs = ti->ti_seq;
		tp->iss = tcp_selectiss();
		tcp_reflect(ti, tp->iss, tp->rcv_next, TH_SYN|TH_ACK);
		tp->t_state = TCPS_SYN_RECEIVED;
		tiflags &= ~TH_SYN; tiflags |= TH_RST;
		if (inp->inp_faddr.s_addr == 0) {
			inp->inp_faddr = ti->ti_src;
			inp->inp_fport = ti->ti_sport;
		}
		break;

	/*
	 * If the state is SYN_SENT:
	 *	if seg contains an ACK, but not for our SYN, drop the input.
	 *	if seg contains a RST, then drop the connection.
	 *	if seg does not contain SYN, then drop it.
	 * Otherwise this is an acceptable SYN segment
	 *	initialize tp->rcv_nxt and tp->irs
	 *	if seg contains ack then advance tp->snd_una
	 *	if SYN has been acked change to ESTABLISHED else SYN_RCVD state
	 *	arrange for segment to be acked (eventually)
	 *	continue processing rest of data/controls, beginning with URG
	 */
	case TCPS_SYN_SENT:
		if ((tiflags & TH_ACK) &&
		    (SEQ_LEQ(ti->ti_ack, tp->iss) ||
		     SEQ_GT(ti->ti_ack, tp->snd.nxt))) {
			tcp_reflect(ti, ti->ti_ack, 0, TH_RST);
			goto drop;
		}
		if (tiflags & TH_RST) {
			if (tiflags & TH_ACK)
				tcp_drop(tp, ENETRESET);
			goto drop;
		}
		if ((tiflags & TH_SYN) == 0)
			goto drop;
		tp->rcv_nxt = ti->ti_seq + 1;
		tp->irs = ti->ti_seq;
		tp->snd_una = ti->ti_seq;
		if (SEQ_GT(tp->snd_una, tp->iss)) {
			tp->t_state = TCPS_ESTABLISHED;
			tp->t_flags |= TF_OWEACK;
			goto step6;
		}
		tp->t_state = TCPS_SYN_RECEIVED;
		tcp_reflect(ti, tp->iss, tp->rcv_nxt, TH_SYN|TH_ACK);
		break;
	}

	/*
	 * States other than LISTEN or SYN_SENT.
	 * First check that at least some bytes of segment are within 
	 * receive window.
	 */
	if (tp->rcv_wnd == 0) {
		/*
		 * If window is closed can only take segments at
		 * window edge, and have to drop data and EOL from
		 * incoming segments.
		 */
		if (tp->rcv_nxt != ti->ti_seq)
			goto dropafterack;
		if (tp->ti_len > 0) {
			tp->ti_len = 0;
			tp->ti_flags &= ~(TH_EOL|TH_FIN);
		}
	} else {
		/*
		 * If segment begins before rcv_next, drop leading
		 * data (and SYN); if nothing left, just ack.
		 */
		if (SEQ_GT(tp->rcv_nxt, ti->ti_seq)) {
			tcpseq_t todrop = tp->rcv_nxt - ti->ti_seq;
			if (todrop > ti->ti_len)
				goto dropafterack;
			m_adj(m, todrop);
			ti->ti_seq += todrop;
			ti->ti_len -= todrop;
			ti->ti_flags &= ~TH_SYN;
		}
		/*
		 * If segment ends after window, drop trailing data
		 * (and EOL and FIN); if there would be nothing left, just ack.
		 */
		if (SEQ_GT(ti->ti_seq+ti->ti_len, tp->rcv_nxt+tp->rcv_wnd)) {
			tcpseq_t todrop =
			     ti->ti_seq+ti->ti_len - (tp->rcv_nxt+tp->rcv_wnd);
			if (todrop > ti->ti_len)
				goto dropafterack;
			m_adj(m, -todrop);
			ti->ti_len -= todrop;
			ti->ti_flags &= ~(TH_EOL|TH_FIN);
		}
	}

	/*
	 * If the RST bit is set examine the state:
	 *    SYN_RECEIVED STATE:
	 *	If passive open, return to LISTEN state.
	 *	If active open, inform user that connection was refused.
	 *    ESTABLISHED, FIN_WAIT_1, FIN_WAIT2, CLOSE_WAIT STATES:
	 *	Inform user that connection was reset, and close tcb.
	 *    CLOSING, LAST_ACK, TIME_WAIT STATES
	 *	Close the tcb.
	 */
	if (tiflags&TH_RST) switch (tp->t_state) {
		
	case TCPS_SYN_RECEIVED:
		if (inp->inp_socket->so_options & SO_ACCEPTCONN) {
			tp->t_state = LISTEN;
			inp->inp_fhost->s_addr = 0;
			goto drop;
		}
		tcp_drop(tp, EREFUSED);
		goto drop;

	case TCPS_ESTABLISHED:
	case TCPS_FIN_WAIT_1:
	case TCPS_FIN_WAIT_2:
	case TCPS_CLOSE_WAIT:
		tcp_drop(tp, ECONNRESET);
		goto drop;

	case TCPS_CLOSING:
	case TCPS_LAST_ACK:
	case TCPS_TIME_WAIT:
		tcp_close(tp);
		goto drop;
	}

	/*
	 * If a SYN is in the window, then this is an
	 * error and we send an RST and drop the connection.
	 */
	if (tiflags & TH_SYN) {
		tcp_drop(tp, ECONNRESET);
		goto sendreset;
	}

	/*
	 * If the ACK bit is off we drop the segment and return.
	 */
	if ((tiflags & TI_ACK) == 0)
		goto drop;
	
	/*
	 * Ack processing.
	 */
	switch (tp->t_state) {

	/*
	 * In SYN_RECEIVED state if the ack ACKs our SYN then enter
	 * ESTABLISHED state and continue processing, othewise
	 * send an RST.
	 */
	case TCPS_SYN_RECEIVED:
		if (SEQ_LEQ(tp->snd_una, ti->ti_ack) &&
		    SEQ_LEQ(ti->ti_ack, tp->snd_nxt))
			tp->t_state = TCPS_ESTABLISHED;
		else
			goto sendreset;
		/* fall into next case, below... */

	/*
	 * In ESTABLISHED state: drop duplicate ACKs; ACK out of range
	 * ACKs.  If the ack is in the range
	 *	tp->snd_una < ti->ti_ack <= tp->snd_nxt
	 * then advance tp->snd_una to ti->ti_ack and drop
	 * data from the retransmission queue.  If this ACK reflects
	 * more up to date window information we update our window information.
	 */
	case TCPS_ESTABLISHED:
	case TCPS_FIN_WAIT_1:
	case TCPS_FIN_WAIT_2:
	case TCPS_CLOSE_WAIT:
	case TCPS_CLOSING:
		if (SEQ_LT(ti->ti_ack, tp->snd_una))
			break;
		if (SEQ_GT(ti->ti_ack, tp->snd_nxt))
			goto dropafterack;
		sbdrop(&so->so_snd, ti->ti_ack - tp->snd_una);
		tp->snd_una = ti->ti_ack;
		if (SEQ_LT(tp->snd_wl1, ti->ti_seq) ||
		    tp->snd_wl1==ti-ti_seq && SEQ_LEQ(tp->snd_wl2,ti->ti_seq)) {
			tp->snd_wnd = ti->ti_win;
			tp->snd_wl1 = ti->ti_seq;
			tp->snd_wl2 = ti->ti_ack;
		}

		switch (tp->t_state) {

		/*
		 * In FIN_WAIT_1 STATE in addition to the processing
		 * for the ESTABLISHED state if our FIN is now acknowledged
		 * then enter FIN_WAIT_2 and continue processing in that state.
		 */
		case TCPS_FIN_WAIT_1:
			if (tcp_finisacked(tp) == 0)
				break;
			tp->t_state = TCPS_FIN_WAIT_2;
			/* fall into ... */

	 	/*
		 * In FIN_WAIT_2 STATE in addition to the processing for
		 * the ESTABLISHED state allow the user to close when
		 * the data has drained.
		 */
		case TCPS_FIN_WAIT_2:
			tcp_usrcanclose(tp);
			break;

	 	/*
		 * In CLOSING STATE in addition to the processing for
		 * the ESTABLISHED state if the ACK acknowledges our FIN
		 * then enter the TIME-WAIT state, otherwise ignore
		 * the segment.
		 */
		case TCPS_CLOSING:
			if (tcp_finisacked(tp))
				tp->t_state = TCPS_TIME_WAIT;
			break;

		/*
		 * In LAST_ACK state if our FIN is now acknowledged
		 * then enter the TIME_WAIT state, otherwise ignore the
		 * segment.
		 */
		case TCPS_LAST_ACK:
			if (tcp_finisacked(tp))
				tcp_close(tp);
			goto drop;

		/*
		 * In TIME_WAIT state the only thing that should arrive
		 * is a retransmission of the remote FIN.  Acknowledge
		 * it and restart the finack timer.
		 */
		case TCPS_TIME_WAIT:
			tp->t_finack = 2 * TCP_MSL;
			goto dropafterack;
		}

step6:
	/*
	 * If an URG bit is set in the segment and is greater than the
	 * current known urgent pointer, then signal the user that the
	 * remote side has urgent data.  This should not happen
	 * in CLOSE_WAIT, CLOSING, LAST-ACK or TIME_WAIT STATES since
	 * a FIN has been received from the remote side.  In these states
	 * we ignore the URG.
	 */
	if ((tiflags & TH_URG) == 0 && (TCPS_RCVDFIN(tp->t_state) == 0) {
		if (SEQ_GT(ti->ti_urp, tp->rcv_up) {
			tp->rcv_up = ti->ti_urp;
			soisurgendata(so);		/* XXX */
		}
	}

	/*
	 * Process the segment text, merging it into the TCP sequencing queue,
	 * and arranging for acknowledgment of receipt if necessary.
	 * This process logically involves adjusting tp->rcv_wnd as data
	 * is presented to the user (this happens in tcp_usrreq.c,
	 * case PRU_RCVD).  If a FIN has already been received on this
	 * connection then we just ignore the text.
	 */
	if (ti->ti_len) {
		if (TCPS_RCVDFIN(tp->t_state))
			goto drop;
		tiflags = tcp_reass(tp, ti);
	else
		m_freem(m);

	/*
	 * If FIN is received then if we haven't received SYN and
	 * therefore can't validate drop the segment.  Otherwise ACK
	 * the FIN and let the user know that the connection is closing.
	 */
	if (tiflags & TH_FIN) {
		if (TCPS_HAVERCVDSYN(tp->t_state) == 0)
			goto drop;
		socantrcvmore(so);
		tp->t_flags |= TF_ACKNOW;
		tp->rcv_nxt++;
		switch (tp->t_state) {

	 	/*
		 * In SYN_RECEIVED and ESTABLISHED STATES
		 * enter the CLOSE_WAIT state.
		 */
		case TCPS_SYN_RECEIVED:
		case TCPS_ESTABLISHED:
			tp->t_state = TCPS_CLOSE_WAIT;
			break;

	 	/*
		 * In FIN_WAIT_1 STATE if our FIN has been acked then
		 * enter TIME_WAIT state, starting the associated timer
		 * and turning off all other standard timers.
	 	 * If FIN has not been acked enter the CLOSING state.
		 */
		case TCPS_FIN_WAIT_1:
			if (tcp_finisacked(tp)) {
				tp->t_state = TCPS_TIME_WAIT;
				tcp_canceltimers(tp, 0);
				tp->t_timer[TCPT_FINACK] = TCPSC_2MSL;
			} else
				tp->t_state = TCPS_CLOSING;
			break;
		}

	 	/*
		 * In FIN_WAIT_2 state enter the TIME_WAIT state,
		 * starting the time-wait timer, turning off the other 
		 * standard timers.
		 */
		case TCPS_FIN_WAIT_2:
			tp->t_state = TCPS_FIN_WAIT_2;
			tcp_canceltimers(tp);
			tp->t_timer[TCPT_FINACK] = TCPSC_2MSL;
			break;

		/*
		 * In TIME_WAIT state restart the 2 MSL time_wait timer.
		 */
		case TCPS_TIME_WAIT:
			tp->t_timer[TCPT_FINACK] = TCPSC_2MSL;
			break;
	}
	return;
dropafterack:
	if ((tiflags & TH_RST) == 0)
		tcp_reflect(ti, tp->rcv_nxt, tp->snd_nxt, TH_ACK);
drop:
	m_freem(m);
	return;
}

/*
 * Insert segment ti into reassembly queue of tcp with
 * control block tp.  Return TH_FIN if reassembly now includes
 * a segment with FIN.
 */
tcp_reass(tp, ti, endp)
	register struct tcpcb *tp;
	register struct tcpiphdr *ti;
	int *endp;
{
	register struct tcpiphdr *q;
	int flags = 0;		/* no FIN */
	int overage;

	/*
	 * If no data in this segment may want
	 * to move data up to socket structure (if
	 * connection is now established).
	 */
	if (ti->ti_len == 0) {
		m_freem(dtom(ti));
		goto present;
	}

	/*
	 * Find a segment which begins after this one does.
	 */
	for (q = tp->seg_next; q != (struct tcpiphdr *)tp;
	    q = (struct tcpiphdr *)q->ti_next)
		if (SEQ_GT(q->ti_seq, ti->ti_seq))
			break;

	/*
	 * If there is a preceding segment, it may provide some of
	 * our data already.  If so, drop the data from the incoming
	 * segment.  If it provides all of our data, drop us.
	 */
	if ((struct tcpiphdr *)q->ti_prev != (struct tcpiphdr *)tp) {
		register int i;
		q = (struct tcpiphdr *)(q->ti_prev);
		/* conversion to int (in i) handles seq wraparound */
		i = q->ti_seq + q->ti_len - ti->ti_seq;
		if (i > 0) {
			if (i >= ti->ti_len)
				goto drop;
			m_adj(dtom(tp), i);
			ti->ti_len -= i;
			ti->ti_seq += i;
		}
		q = (struct tcpiphdr *)(q->ti_next);
	}

	/*
	 * While we overlap succeeding segments trim them or,
	 * if they are completely covered, dequeue them.
	 */
	while (q != (struct tcpiphdr *)tp &&
	    SEQ_GT(ti->ti_seq + ti->ti_len, q->ti_seq)) {
		register int i = (ti->ti_seq + ti->ti_len) - q->ti_seq;
		if (i < q->ti_len) {
			q->ti_len -= i;
			m_adj(dtom(q), i);
			break;
		}
		q = (struct tcpiphdr *)q->ti_next;
		m_freem(dtom(q->ti_prev));
		remque(q->ti_prev);
	}

	/*
	 * Stick new segment in its place.
	 */
	insque(ti, q->ti_prev);
	tp->seqcnt += ti->ti_len;

	/*
	 * Calculate available space and discard segments for
	 * which there is too much.
	 */
	overage = 
	    (so->so_rcv.sb_cc + tp->seqcnt) - so->so_rcv.sb_hiwat;
	if (overage > 0) {
		q = tp->seg_prev;
		for (;;) {
			register int i = MIN(q->ti_len, overage);
			overage -= i;
			tp->seqcnt -= i;
			q->ti_len -= i;
			m_adj(dtom(q), -i);
			if (q->ti_len)
				break;
			if (q == ti)
				panic("tcp_reass dropall");
			q = (struct tcpiphdr *)q->ti_prev;
			remque(q->ti_next);
		}
	}

	/*
	 * Advance rcv_next through newly completed sequence space.
	 */
	while (ti->ti_seq == tp->rcv_nxt) {
		tp->rcv_nxt += ti->ti_len;
		flags = ti->ti_flags & TH_FIN;
		ti = (struct tcpiphdr *)ti->ti_next;
		if (ti == (struct tcpiphdr *)tp)
			break;
	}

present:
	/*
	 * Present data to user.
	 */
	if (tp->t_state < ESTAB)
		return (flags);
	ti = tp->seg_next;
	while (ti != (struct tcpiphdr *)tp && ti->ti_seq < tp->rcv_nxt) {
		remque(ti);
		sbappend(&so->so_rcv, dtom(ti));
		tp->seqcnt -= ti->ti_len;
		if (tp->seqcnt < 0)
			panic("tcp_reass");
		ti = (struct tcpiphdr *)ti->ti_next;
	}
	if (so->so_state & SS_CANTRCVMORE)
		sbflush(&so->so_rcv);
	else
		sorwakeup(so);
	return (flags);
drop:
	m_freem(dtom(ti));
	return (flags);
}
