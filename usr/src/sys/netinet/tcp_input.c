/* tcp_input.c 1.29 81/11/24 */

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

tcp_drain()
{
	register struct inpcb *ip;

COUNT(TCP_DRAIN);
	for (ip = tcb.inp_next; ip != &tcb; ip = ip->inp_next)
		tcp_drainunack(intotcpcb(ip));
}

tcp_drainunack(tp)
	register struct tcpcb *tp;
{
	register struct mbuf *m;

COUNT(TCP_DRAINUNACK);
	for (m = tp->seg_unack; m; m = m->m_act)
		m_freem(m);
	tp->seg_unack = 0;
}
	
tcp_ctlinput(m)
	struct mbuf *m;
{

COUNT(TCP_CTLINPUT);
	m_freem(m);
}

struct	sockaddr_in tcp_sockaddr = { AF_INET };

tcp_input(m0)
	struct mbuf *m0;
{
	register struct tcpiphdr *ti;
	struct inpcb *inp;
	register struct mbuf *m;
	int len, tlen, off;

	register struct tcpcb *tp;
	register int j;
	register int tiflags;
	int nstate;
	struct socket *so;
#ifdef TCPDEBUG
	struct tcp_debug tdb;
#endif

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
			printf("tcp cksum %x\ti", ti->ti_sum);
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

	/*
	 * Locate pcb for segment.
	 */
	inp = in_pcblookup(&tcb, ti->ti_src, ti->ti_sport, ti->ti_dst, ti->ti_dport);
	if (inp == 0)
		goto notwanted;
	tp = intotcpcb(inp);		/* ??? */
	if (tp == 0)			/* ??? */
		goto notwanted;		/* ??? */

	/*
	 * Convert tcp protocol specific fields to host format.
	 */
	ti->ti_seq = ntohl(ti->ti_seq);
	ti->ti_ackno = ntohl((n_long)ti->ti_ackno);
	ti->ti_win = ntohs(ti->ti_win);
	ti->ti_urp = ntohs(ti->ti_urp);

	/*
	 * Check segment seq # and do rst processing
	 */
	tiflags = ti->ti_flags;
	switch (tp->t_state) {

	case LISTEN:
		if ((tiflags&TH_ACK) || (tiflags&TH_SYN) == 0) {
			tcp_sndrst(tp, ti);
			goto bad;
		}
		if (tiflags&TH_RST)
			goto bad;
		goto good;

	case SYN_SENT:
		if (!ack_ok(tp, ti) || (tiflags&TH_SYN) == 0) {
			tcp_sndrst(tp, ti);			/* 71,72,75 */
			goto bad;
		}
		if (tiflags&TH_RST) {
			tcp_drop(tp, ENETRESET);
			goto bad;
		}
		goto good;

	default:
        	if ((tiflags&TH_RST) == 0)
			goto common;
		if (ti->ti_seq < tp->rcv_nxt)		/* bad rst */
			goto bad;				/* 69 */
		switch (tp->t_state) {

		case L_SYN_RCVD:
			if (ack_ok(tp, ti) == 0)
				goto bad;			/* 69 */
			tp->t_rexmt = 0;
			tp->t_rexmttl = 0;
			tp->t_persist = 0;
			inp->inp_faddr.s_addr = 0;
			tp->t_state = LISTEN;
			goto bad;

		default:
			tcp_drop(tp, ENETRESET);
			goto bad;
		}
		/*NOTREACHED*/

	case SYN_RCVD:
common:
		if (ack_ok(tp, ti) == 0) {
			tcp_sndrst(tp, ti);			/* 74 */
			goto bad;
		}
		if ((tiflags&TH_SYN) == 0 && ti->ti_seq != tp->irs) {
			tcp_sndnull(tp);			/* 74 */
			goto bad;
		}
		goto good;
	}
bad:
	m_freem(m);
	return;

good:
	/*
	 * Defer processing if no buffer space for this connection.
	 */
	so = inp->inp_socket;
	if (so->so_rcv.sb_cc >= so->so_rcv.sb_hiwat &&
	     ti->ti_len != 0 && mbstat.m_bufs < mbstat.m_lowat) {
/*
		m->m_act = (struct mbuf *)0;
		if ((m = tp->seg_unack) != NULL) {
			while (m->m_act != NULL)
				m = m->m_act;
			m->m_act = m0;
		} else
			tp->seg_unack = m0;
*/
		m_freem(m0);
		return;
	}

	/*
	 * Discard ip header, and do tcp input processing.
	 */
	off += sizeof (struct ip);
	m->m_off += off;
	m->m_len -= off;
	nstate = tp->t_state;
	tp->tc_flags &= ~TC_NET_KEEP;
#ifdef KPROF
	acounts[tp->t_state][INRECV]++;
#endif
#ifdef TCPDEBUG
	if ((tp->t_socket->so_options & SO_DEBUG) || tcpconsdebug) {
		tdb_setup(tp, ti, INRECV, &tdb);
	} else
		tdb.td_tod = 0;
#endif
	switch (tp->t_state) {

	case LISTEN:
		tcp_sockaddr.sin_addr = ti->ti_src;
		tcp_sockaddr.sin_port = ti->ti_sport;
		if ((tiflags&TH_SYN) == 0 || in_pcbsetpeer(inp, &tcp_sockaddr)) {
			nstate = EFAILEC;
			goto done;
		}
		tp->t_template = tcp_template(tp);
		tcp_ctldat(tp, ti, 1);
		if (tp->tc_flags&TC_FIN_RCVD) {
			tp->t_finack = T_2ML;			/* 3 */
			nstate = CLOSE_WAIT;
		} else {
			tp->t_init = T_INIT / 2;		/* 4 */
			nstate = L_SYN_RCVD;
		}
		goto done;

	case SYN_SENT:
		if (!syn_ok(tp, ti)) {
			nstate = EFAILEC;
			goto done;
		}
		tcp_ctldat(tp, ti, 1);
		if (tp->tc_flags&TC_FIN_RCVD) {
			if ((tiflags&TH_ACK) == 0)
				tp->t_finack = T_2ML;		/* 9 */
			nstate = CLOSE_WAIT;
			goto done;
		}
		nstate = (tiflags&TH_ACK) ? ESTAB : SYN_RCVD; /* 11:8 */
		goto done;

	case SYN_RCVD:
	case L_SYN_RCVD:
		if ((tiflags&TH_ACK) == 0 ||
		    (tiflags&TH_ACK) && ti->ti_ackno <= tp->iss) {
			nstate = EFAILEC;
			goto done;
		}
		goto input;

	case ESTAB:
	case FIN_W1:
	case FIN_W2:
	case TIME_WAIT:
input:
		tcp_ctldat(tp, ti, 1);				/* 39 */
		switch (tp->t_state) {

		case ESTAB:
			if (tp->tc_flags&TC_FIN_RCVD)
				nstate = CLOSE_WAIT;
			break;

		case SYN_RCVD:
		case L_SYN_RCVD:
			nstate = (tp->tc_flags&TC_FIN_RCVD) ?
			    CLOSE_WAIT : ESTAB;			 /* 33:5 */
			break;

		case FIN_W1:
			j = ack_fin(tp, ti);
			if ((tp->tc_flags & TC_FIN_RCVD) == 0) {
				if (j)
					nstate = FIN_W2;	/* 27 */
				break;
			}
			tp->t_finack = T_2ML;
			nstate = j ? TIME_WAIT : CLOSING;	/* 28:26 */
			break;

		case FIN_W2:
			if (tp->tc_flags&TC_FIN_RCVD) {
				tp->t_finack = T_2ML;		/* 29 */
				nstate = TIME_WAIT;
				break;
			}
			break;
		}
		goto done;

	case CLOSE_WAIT:
		if (tiflags&TH_FIN) {
			if ((tiflags&TH_ACK) &&
			    ti->ti_ackno <= tp->seq_fin) {
				tcp_ctldat(tp, ti, 0);		/* 30 */
				tp->t_finack = T_2ML;
			} else
				(void) tcp_sndctl(tp);		/* 31 */
			goto done;
		}
		goto input;

	case CLOSING:
		j = ack_fin(tp, ti);
		if (tiflags&TH_FIN) {
			tcp_ctldat(tp, ti, 0);
			tp->t_finack = T_2ML;
			if (j)
				nstate = TIME_WAIT;		/* 23 */
			goto done;
		}
		if (j) {
			if (tp->t_finack == 0)
				if (rcv_empty(tp)) {
					sorwakeup(inp->inp_socket);
					nstate = CLOSED;	/* 15 */
				} else
					nstate = RCV_WAIT;	/* 18 */
			else
				nstate = TIME_WAIT;
			goto done;
		}
		goto input;

	case LAST_ACK:
		if (ack_fin(tp, ti)) {
			if (rcv_empty(tp)) {		/* 16 */
				sorwakeup(inp->inp_socket);
				nstate = CLOSED;
			} else
				nstate = RCV_WAIT;		/* 19 */
			goto done;
		}
		if (tiflags&TH_FIN) {
			(void) tcp_sndctl(tp);			/* 31 */
			goto done;
		}
		goto input;

	case RCV_WAIT:
		if ((tiflags&TH_FIN) && (tiflags&TH_ACK) &&
		    ti->ti_ackno <= tp->seq_fin) {
			tcp_ctldat(tp, ti, 0);
			tp->t_finack = T_2ML;			/* 30 */
		}
		goto done;
	}
	panic("tcp_input");
done:

	/*
	 * Done with state*input specific processing.
	 * Form trace records, free input if not needed,
	 * and enter new state.
	 */
#ifdef TCPDEBUG
	if (tdb.td_tod)
		tdb_stuff(&tdb, nstate);
#endif
	switch (nstate) {

	case EFAILEC:
		m_freem(m);
		return;

	default:
		tp->t_state = nstate;
		/* fall into ... */

	case CLOSED:
		/* IF CLOSED CANT LOOK AT tc_flags */
		if ((tp->tc_flags&TC_NET_KEEP) == 0) {
			register struct mbuf *n;
			/* inline expansion of m_freem */
			while (m) {
				MFREE(m, n);
				m = n;
			}
		}
		return;
	}
	/* NOTREACHED */

	/*
	 * Unwanted packed; free everything
	 * but the header and return an rst.
	 */
notwanted:
	m_freem(m->m_next);
	m->m_next = NULL;
	m->m_len = sizeof(struct tcpiphdr);
#define xchg(a,b) j=a; a=b; b=j
	xchg(ti->ti_dst.s_addr, ti->ti_src.s_addr);
	xchg(ti->ti_dport, ti->ti_sport);
#undef xchg
	if (tiflags&TH_ACK)
		ti->ti_seq = ti->ti_ackno;
	else {
		ti->ti_ackno = htonl((unsigned)(ntohl(ti->ti_seq) + ti->ti_len));
		ti->ti_seq = 0;
	}
	ti->ti_flags = ((tiflags & TH_ACK) ? 0 : TH_ACK) | TH_RST;
	ti->ti_len = htons(TCPSIZE);
	ti->ti_off = 5;
	ti->ti_sum = inet_cksum(m, sizeof(struct tcpiphdr));
	((struct ip *)ti)->ip_len = sizeof(struct tcpiphdr);
	((struct ip *)ti)->ip_ttl = MAXTTL;
	ip_output(m);
	tcpstat.tcps_badsegs++;
}

tcp_ctldat(tp, n0, dataok)
	register struct tcpcb *tp;
	struct tcpiphdr *n0;
	int dataok;
{
	register struct tcpiphdr *ti = n0;
	register int tiflags = ti->ti_flags;
	struct socket *so = tp->t_inpcb->inp_socket;
	seq_t past = ti->ti_seq + ti->ti_len;
	seq_t urgent;
	int sent;
COUNT(TCP_CTLDAT);

	if (tiflags & TH_URG)
		urgent = ti->ti_seq + ti->ti_urp;
	tp->tc_flags &= ~(TC_ACK_DUE|TC_NEW_WINDOW);
/* syn */
	if ((tp->tc_flags&TC_SYN_RCVD) == 0 && (tiflags&TH_SYN)) {
		tp->irs = ti->ti_seq;
		tp->rcv_nxt = ti->ti_seq + 1;
		tp->snd_wl = tp->rcv_urp = tp->irs;
		tp->tc_flags |= (TC_SYN_RCVD|TC_ACK_DUE);
	}
/* ack */
	if ((tiflags&TH_ACK) && (tp->tc_flags&TC_SYN_RCVD) &&
	    ti->ti_ackno > tp->snd_una) {
		/*
		 * Reflect newly acknowledged data.
		 */
		tp->snd_una = ti->ti_ackno;
		if (tp->snd_una > tp->snd_nxt)
			tp->snd_nxt = tp->snd_una;

		/*
		 * If timed msg acked, update retransmit time value.
		 */
		if ((tp->tc_flags&TC_SYN_ACKED) &&
		    tp->snd_una > tp->t_xmt_val) {
			/* NEED SMOOTHING HERE */
			tp->t_xmtime = (tp->t_xmt != 0 ? tp->t_xmt : T_REXMT);
			if (tp->t_xmtime > T_REMAX)
				tp->t_xmtime = T_REMAX;
		}

		/*
		 * Remove acked data from send buf
		 */
		sbdrop(&so->so_snd, (int)(tp->snd_una - tp->snd_off));
		tp->snd_off = tp->snd_una;
		if ((tp->tc_flags&TC_SYN_ACKED) == 0 &&
		    (tp->snd_una > tp->iss)) {
			tp->tc_flags |= TC_SYN_ACKED;
			tp->t_init = 0;
		}
		if (tp->seq_fin != tp->iss && tp->snd_una > tp->seq_fin)
			tp->tc_flags &= ~TC_SND_FIN;
		tp->t_rexmt = 0;
		tp->t_rexmttl = 0;
		tp->tc_flags |= TC_CANCELLED;
		sowwakeup(tp->t_inpcb->inp_socket);
	}
/* win */
	if ((tp->tc_flags & TC_SYN_RCVD) && ti->ti_seq >= tp->snd_wl) {
		tp->snd_wl = ti->ti_seq;
		tp->snd_wnd = ti->ti_win;
		tp->tc_flags |= TC_NEW_WINDOW;
		tp->t_persist = 0;
	}
/* text */
	if (dataok && ti->ti_len) {
		register struct tcpiphdr *q;
		int overage;

/* eol */
		if ((tiflags&TH_EOL)) {
			register struct mbuf *m;
			for (m = dtom(ti); m->m_next; m = m->m_next)
				;
			m->m_act = (struct mbuf *)(mtod(m, caddr_t) - 1);
		}

/* text */
		/*
		 * Discard duplicate data already passed to user.
		 */
		if (SEQ_LT(ti->ti_seq, tp->rcv_nxt)) {
			register int i = tp->rcv_nxt - ti->ti_seq;
			if (i >= ti->ti_len)
				goto notext;
			ti->ti_seq += i;
			ti->ti_len -= i;
			m_adj(dtom(ti), i);
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
					goto notext;
						/* w/o setting TC_NET_KEEP */
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
		    (so->so_rcv.sb_cc /*XXX+tp->rcv_seqcnt*/) - so->so_rcv.sb_hiwat;
		if (overage > 0) {
			q = tp->seg_prev;
			for (;;) {
				register int i = MIN(q->ti_len, overage);
				overage -= i;
				q->ti_len -= i;
				m_adj(dtom(q), -i);
				if (q->ti_len)
					break;
				if (q == ti)
					panic("tcp_text dropall");
				q = (struct tcpiphdr *)q->ti_prev;
				remque(q->ti_next);
			}
		}

		/*
		 * Advance rcv_next through newly completed sequence space.
		 */
		while (ti->ti_seq == tp->rcv_nxt) {
			tp->rcv_nxt += ti->ti_len;
			ti = (struct tcpiphdr *)ti->ti_next;
			if (ti == (struct tcpiphdr *)tp)
				break;
		}
/* urg */
		if (tiflags&TH_URG) {
			/* ... */
			if (SEQ_GT(urgent, tp->rcv_urp))
				tp->rcv_urp = urgent;
		}
		tp->tc_flags |= (TC_ACK_DUE|TC_NET_KEEP);
	}
notext:
/* fin */
	if ((tiflags&TH_FIN) && past == tp->rcv_nxt) {
		if ((tp->tc_flags&TC_FIN_RCVD) == 0) {
			tp->tc_flags |= TC_FIN_RCVD;
			sorwakeup(so);
			tp->rcv_nxt++;
		}
		tp->tc_flags |= TC_ACK_DUE;
	}
/* respond */
	sent = 0;
	if (tp->tc_flags&TC_ACK_DUE)
		sent = tcp_sndctl(tp);
	else if ((tp->tc_flags&TC_NEW_WINDOW))
		if (tp->snd_nxt <= tp->snd_off + so->so_snd.sb_cc ||
		    (tp->tc_flags&TC_SND_FIN))
			sent = tcp_send(tp);

/* set for retrans */
	if (!sent && tp->snd_una < tp->snd_nxt &&
	    (tp->tc_flags&TC_CANCELLED)) {
		tp->t_rexmt = tp->t_xmtime;
		tp->t_rexmttl = T_REXMTTL;
		tp->t_rexmt_val = tp->t_rtl_val = tp->snd_lst;
		tp->tc_flags &= ~TC_CANCELLED;
	}
/* present data to user */
	if ((tp->tc_flags&TC_SYN_ACKED) == 0)
		return;
	ti = tp->seg_next;
	while (ti != (struct tcpiphdr *)tp && ti->ti_seq < tp->rcv_nxt) {
		remque(ti);
		sbappend(&so->so_rcv, dtom(ti));
		tp->seqcnt -= ti->ti_len;
		if (tp->seqcnt < 0)
			panic("tcp_input present");
		ti = (struct tcpiphdr *)ti->ti_next;
	}
	sorwakeup(so);
}
