/* tcp_input.c 1.23 81/11/15 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/inet_cksum.h"
#include "../net/inet.h"
#include "../net/inet_pcb.h"
#include "../net/inet_systm.h"
#include "../net/imp.h"
#include "../net/inet_host.h"
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

	for (ip = tcb.inp_next; ip != &tcb; ip = ip->inp_next)
		tcp_drainunack(intotcpcb(ip));
}

tcp_drainunack(tp)
	register struct tcpcb *tp;
{
	register struct mbuf *m;

	for (m = tp->seg_unack; m; m = m->m_act)
		m_freem(m);
	tp->seg_unack = 0;
}
	
tcp_ctlinput(m)
	struct mbuf *m;
{

	m_freem(m);
}

tcp_input(mp)
	register struct mbuf *mp;
{
	register struct tcpiphdr *n;		/* known to be r10 */
	register int j;
	register struct tcpcb *tp;
	struct inpcb *inp;
	register int thflags;
	int nstate;
	struct mbuf *m;
	struct socket *so;
	int hlen, tlen;
	u_short lport, fport;
#ifdef TCPDEBUG
	struct tcp_debug tdb;
#endif
COUNT(TCP_INPUT);

	/*
	 * Build extended tcp header
	 */
	n = mtod(mp, struct tcpiphdr *);
	thflags = n->ti_flags;
	tlen = ((struct ip *)n)->ip_len;
	n->ti_len = htons(tlen);
	n->ti_next = NULL;
	n->ti_prev = NULL;
	n->ti_x1 = 0;
	lport = ntohs(n->ti_dst);
	fport = ntohs(n->ti_src);

	/* WONT BE POSSIBLE WHEN MBUFS ARE 256 BYTES */
	if ((hlen = n->ti_off << 2) > mp->m_len)
		{ printf("tcp header overflow\n"); m_freem(mp); return; }

	if (tcpcksum) {
		/*
		 * Checksum extended header and data
		 */
		CKSUM_TCPCHK(mp, n, r10, sizeof (struct ip) + tlen);
		if (n->ti_sum != 0) {
			netstat.t_badsum++;
			m_freem(mp);
			return;
		}
	}

	/*
	 * Find tcb for message.
	 */
	inp = inpcb_lookup(&tcb, &n->ti_src, fport, &n_lhost, lport);
	if (inp == 0)
		goto notwanted;

	/*
	 * Byte swap header
	 */
	n->ti_len = tlen - hlen;
	n->ti_sport = fport;
	n->ti_dport = lport;
	n->ti_seq = ntohl(n->ti_seq);
	n->ti_ackno = ntohl(n->ti_ackno);
	n->ti_win = ntohs(n->ti_win);
	n->ti_urp = ntohs(n->ti_urp);

	/*
	 * Check segment seq # and do rst processing
	 */
	switch (tp->t_state) {

	case LISTEN:
		if ((thflags&TH_ACK) || !syn_ok(tp, n)) {
			tcp_sndrst(tp, n);
			goto badseg;
		}
		if (thflags&TH_RST)
			goto badseg;
		goto goodseg;

	case SYN_SENT:
		if (!ack_ok(tp, n) || !syn_ok(tp, n)) {
			tcp_sndrst(tp, n);			/* 71,72,75 */
			goto badseg;
		}
		if (thflags&TH_RST) {
			tcp_error(tp, ENETRESET);
			tcp_detach(tp);				/* 70 */
			tp->t_state = CLOSED;
			goto badseg;
		}
		goto goodseg;

	default:
        	if ((thflags&TH_RST) == 0)
			goto common;
		if (n->ti_seq < tp->rcv_nxt)		/* bad rst */
			goto badseg;				/* 69 */
		switch (tp->t_state) {

		case L_SYN_RCVD:
			if (ack_ok(tp, n) == 0)
				goto badseg;			/* 69 */
			tp->t_rexmt = 0;
			tp->t_rexmttl = 0;
			tp->t_persist = 0;
			h_free(inp->inp_fhost);
			inp->inp_fhost = 0;
			tp->t_state = LISTEN;
			goto badseg;

		default:
			tcp_error(tp, ENETRESET);
			tcp_detach(tp);				/* 66 */
			tp->t_state = CLOSED;
			goto badseg;
		}
		/*NOTREACHED*/

	case SYN_RCVD:
common:
		if (ack_ok(tp, n) == 0) {
			tcp_sndrst(tp, n);			/* 74 */
			goto badseg;
		}
		if (syn_ok(tp, n) && n->ti_seq != tp->irs) {
			tcp_sndnull(tp);			/* 74 */
			goto badseg;
		}
		goto goodseg;
	}
badseg:
	m_freem(mp);
	return;

goodseg:
	/*
	 * Defer processing if no buffer space for this connection.
	 */
	so = inp->inp_socket;
	if (so->so_rcv.sb_cc >= so->so_rcv.sb_hiwat &&
	     n->ti_len != 0 && mbstat.m_bufs < mbstat.m_lowat) {
/*
		mp->m_act = (struct mbuf *)0;
		if ((m = tp->seg_unack) != NULL) {
			while (m->m_act != NULL)
				m = m->m_act;
			m->m_act = mp;
		} else
			tp->seg_unack = mp;
*/
		m_freem(mp);
		return;
	}

	/*
	 * Discard ip header, and do tcp input processing.
	 */
	hlen += sizeof(struct ip);
	mp->m_off += hlen;
	mp->m_len -= hlen;
	nstate = tp->t_state;
	tp->tc_flags &= ~TC_NET_KEEP;
#ifdef KPROF
	acounts[tp->t_state][INRECV]++;
#endif
#ifdef TCPDEBUG
	if ((tp->t_socket->so_options & SO_DEBUG) || tcpconsdebug) {
		tdb_setup(tp, n, INRECV, &tdb);
	} else
		tdb.td_tod = 0;
#endif
	switch (tp->t_state) {

	case LISTEN:
		if (!syn_ok(tp, n) ||
		    ((inp->inp_lhost = in_hmake(&n->ti_src)) == 0)) {
			nstate = EFAILEC;
			goto done;
		}
		inp->inp_fport = n->ti_sport;
		tp->t_template = tcp_template(tp);
		tcp_ctldat(tp, n, 1);
		if (tp->tc_flags&TC_FIN_RCVD) {
			tp->t_finack = T_2ML;			/* 3 */
			tp->tc_flags &= ~TC_WAITED_2_ML;
			nstate = CLOSE_WAIT;
		} else {
			tp->t_init = T_INIT / 2;		/* 4 */
			nstate = L_SYN_RCVD;
		}
		goto done;

	case SYN_SENT:
		if (!syn_ok(tp, n)) {
			nstate = EFAILEC;
			goto done;
		}
		tcp_ctldat(tp, n, 1);
		if (tp->tc_flags&TC_FIN_RCVD) {
			if ((thflags&TH_ACK) == 0) {
				tp->t_finack = T_2ML;		/* 9 */
				tp->tc_flags &= ~TC_WAITED_2_ML;
			}
			nstate = CLOSE_WAIT;
			goto done;
		}
		nstate = (thflags&TH_ACK) ? ESTAB : SYN_RCVD; /* 11:8 */
		goto done;

	case SYN_RCVD:
	case L_SYN_RCVD:
		if ((thflags&TH_ACK) == 0 ||
		    (thflags&TH_ACK) && n->ti_ackno <= tp->iss) {
			nstate = EFAILEC;
			goto done;
		}
		goto input;

	case ESTAB:
	case FIN_W1:
	case FIN_W2:
	case TIME_WAIT:
input:
		tcp_ctldat(tp, n, 1);				/* 39 */
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
			j = ack_fin(tp, n);
			if ((tp->tc_flags & TC_FIN_RCVD) == 0) {
				if (j)
					nstate = FIN_W2;	/* 27 */
				break;
			}
			tp->t_finack = T_2ML;
			tp->tc_flags &= ~TC_WAITED_2_ML;
			nstate = j ? TIME_WAIT : CLOSING;	/* 28:26 */
			break;

		case FIN_W2:
			if (tp->tc_flags&TC_FIN_RCVD) {
				tp->t_finack = T_2ML;		/* 29 */
				tp->tc_flags &= ~TC_WAITED_2_ML;
				nstate = TIME_WAIT;
				break;
			}
			break;
		}
		goto done;

	case CLOSE_WAIT:
		if (thflags&TH_FIN) {
			if ((thflags&TH_ACK) &&
			    n->ti_ackno <= tp->seq_fin) {
				tcp_ctldat(tp, n, 0);		/* 30 */
				tp->t_finack = T_2ML;
				tp->tc_flags &= ~TC_WAITED_2_ML;
			} else
				tcp_sndctl(tp);			/* 31 */
			goto done;
		}
		goto input;

	case CLOSING:
		j = ack_fin(tp, n);
		if (thflags&TH_FIN) {
			tcp_ctldat(tp, n, 0);
			tp->t_finack = T_2ML;
			tp->tc_flags &= ~TC_WAITED_2_ML;
			if (j)
				nstate = TIME_WAIT;		/* 23 */
			goto done;
		}
		if (j) {
			if (tp->tc_flags&TC_WAITED_2_ML)
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
		if (ack_fin(tp, n)) {
			if (rcv_empty(tp)) {		/* 16 */
				sorwakeup(inp->inp_socket);
				nstate = CLOSED;
			} else
				nstate = RCV_WAIT;		/* 19 */
			goto done;
		}
		if (thflags&TH_FIN) {
			tcp_sndctl(tp);				/* 31 */
			goto done;
		}
		goto input;

	case RCV_WAIT:
		if ((thflags&TH_FIN) && (thflags&TH_ACK) &&
		    n->ti_ackno <= tp->seq_fin) {
			tcp_ctldat(tp, n, 0);
			tp->t_finack = T_2ML;
			tp->tc_flags &= ~TC_WAITED_2_ML;	/* 30 */
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
		m_freem(mp);
		return;

	default:
		tp->t_state = nstate;
		/* fall into ... */

	case CLOSED:
		/* IF CLOSED CANT LOOK AT tc_flags */
		if ((tp->tc_flags&TC_NET_KEEP) == 0)
			/* inline expansion of m_freem */
			while (mp) {
				MFREE(mp, m);
				mp = m;
			}
		return;
	}
	/* NOTREACHED */

	/*
	 * Unwanted packed; free everything
	 * but the header and return an rst.
	 */
notwanted:
	m_freem(mp->m_next);
	mp->m_next = NULL;
	mp->m_len = sizeof(struct tcpiphdr);
#define xchg(a,b) j=a; a=b; b=j
	xchg(n->ti_dst.s_addr, n->ti_src.s_addr);
	xchg(n->ti_dport, n->ti_sport);
#undef xchg
	if (thflags&TH_ACK)
		n->ti_seq = n->ti_ackno;
	else {
		n->ti_ackno = htonl(ntohl(n->ti_seq) + tlen - hlen);
		n->ti_seq = 0;
	}
	n->ti_flags = ((thflags & TH_ACK) ? 0 : TH_ACK) | TH_RST;
	n->ti_len = htons(TCPSIZE);
	n->ti_off = 5;
	n->ti_sum = inet_cksum(mp, sizeof(struct tcpiphdr));
	((struct ip *)n)->ip_len = sizeof(struct tcpiphdr);
	ip_output(mp);
	netstat.t_badsegs++;
}

tcp_ctldat(tp, n0, dataok)
	register struct tcpcb *tp;
	struct tcpiphdr *n0;
	int dataok;
{
	register struct mbuf *m;
	register struct tcpiphdr *n = n0;
	register int thflags = n->ti_flags;
	struct socket *so = tp->t_inpcb->inp_socket;
	seq_t past = n->ti_seq + n->ti_len;
	seq_t urgent;
	int sent;
COUNT(TCP_CTLDAT);

	if (thflags & TH_URG)
		urgent = n->ti_seq + n->ti_urp;
	tp->tc_flags &= ~(TC_ACK_DUE|TC_NEW_WINDOW);
/* syn */
	if ((tp->tc_flags&TC_SYN_RCVD) == 0 && (thflags&TH_SYN)) {
		tp->irs = n->ti_seq;
		tp->rcv_nxt = n->ti_seq + 1;
		tp->snd_wl = tp->rcv_urp = tp->irs;
		tp->tc_flags |= (TC_SYN_RCVD|TC_ACK_DUE);
	}
/* ack */
	if ((thflags&TH_ACK) && (tp->tc_flags&TC_SYN_RCVD) &&
	    n->ti_ackno > tp->snd_una) {
		register struct mbuf *mn;

		/*
		 * Reflect newly acknowledged data.
		 */
		tp->snd_una = n->ti_ackno;
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
		sbdrop(&so->so_snd, tp->snd_una - tp->snd_off);
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
	if ((tp->tc_flags & TC_SYN_RCVD) && n->ti_seq >= tp->snd_wl) {
		tp->snd_wl = n->ti_seq;
		tp->snd_wnd = n->ti_win;
		tp->tc_flags |= TC_NEW_WINDOW;
		tp->t_persist = 0;
	}
/* text */
	if (dataok && n->ti_len) {
		register struct tcpiphdr *p, *q;
		int overage;

/* eol */
		if ((thflags&TH_EOL)) {
			register struct mbuf *m;
			for (m = dtom(n); m->m_next; m = m->m_next)
				;
			m->m_act = (struct mbuf *)(mtod(m, caddr_t) - 1);
		}

/* text */
		/*
		 * Discard duplicate data already passed to user.
		 */
		if (SEQ_LT(n->ti_seq, tp->rcv_nxt)) {
			register int i = tp->rcv_nxt - n->ti_seq;
			if (i >= n->ti_len)
				goto notext;
			n->ti_seq += i;
			n->ti_len -= i;
			m_adj(dtom(n), i);
		}

		/*
		 * Find a segment which begins after this one does.
		 */
		for (q = tp->seg_next; q != (struct tcpiphdr *)tp;
		    q = (struct tcpiphdr *)q->ti_next)
			if (SEQ_GT(q->ti_seq, n->ti_seq))
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
			i = q->ti_seq + q->ti_len - n->th_seq;
			if (i > 0) {
				if (i >= n->ti_len)
					goto notext;
						/* w/o setting TC_NET_KEEP */
				m_adj(dtom(tp), i);
				n->ti_len -= i;
				n->ti_seq += i;
			}
			q = (struct tcpiphdr *)(q->ti_next);
		}

		/*
		 * While we overlap succeeding segments trim them or,
		 * if they are completely covered, dequeue them.
		 */
		while (q != (struct tcpiphdr *)tp &&
		    SEQ_GT(n->ti_seq + n->ti_len, q->ti_seq)) {
			register int i = (n->ti_seq + n->ti_len) - q->ti_seq;
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
		insque(n, q->ti_prev);
		tp->seqcnt += n->ti_len;

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
				m_adj(q, -i);
				if (q->ti_len)
					break;
				if (q == n)
					panic("tcp_text dropall");
				q = (struct tcpiphdr *)q->ti_prev;
				remque(q->ti_next);
			}
		}

		/*
		 * Advance rcv_next through newly completed sequence space.
		 */
		while (n->ti_seq == tp->rcv_nxt) {
			tp->rcv_nxt += n->ti_len;
			n = (struct tcpiphdr *)n->ti_next;
			if (n == (struct tcpiphdr *)tp)
				break;
		}
/* urg */
		if (thflags&TH_URG) {
			/* ... */
			if (SEQ_GT(urgent, tp->rcv_urp))
				tp->rcv_urp = urgent;
		}
		tp->tc_flags |= (TC_ACK_DUE|TC_NET_KEEP);
	}
notext:
/* fin */
	if ((thflags&TH_FIN) && past == tp->rcv_nxt) {
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
	n = tp->seg_next;
	while (n != (struct tcpiphdr *)tp && n->ti_seq < tp->rcv_nxt) {
		remque(n);
		sbappend(so->so_rcv, dtom(n));
		tp->seqcnt -= n->ti_len;
		if (tp->seqcnt < 0)
			panic("tcp_input present");
		n = (struct tcpiphdr *)n->ti_next;
	}
	sorwakeup(so);
}
