/* tcp_input.c 1.11 81/10/30 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../inet/inet.h"
#include "../inet/inet_systm.h"
#include "../inet/imp.h"
#include "../inet/inet_host.h"
#include "../inet/ip.h"
#include "../inet/tcp.h"
#include "../inet/tcp_fsm.h"

extern int nosum;

tcp_input(mp)
	register struct mbuf *mp;
{
	register struct th *n;		/* known to be r10 */
	register int j;			/* known to be r9 */
	register struct tcb *tp;
	int nstate;
	struct mbuf *m;
	struct ucb *up;
	int hlen, tlen;
	u_short lport, fport;
#ifdef TCPDEBUG
	struct tcp_debug tdb;
#endif
COUNT(TCP_INPUT);

	/*
	 * Build extended tcp header
	 */
	n = (struct th *)((int)mp + mp->m_off);
	tlen = ((struct ip *)n)->ip_len;
	n->t_len = htons(tlen);
	n->t_next = NULL;
	n->t_prev = NULL;
	n->t_x1 = 0;
	lport = ntohs(n->t_dst);
	fport = ntohs(n->t_src);

	/* WONT BE POSSIBLE WHEN MBUFS ARE 256 BYTES */
	if ((hlen = n->t_off << 2) > mp->m_len)
		{ printf("tcp header overflow\n"); m_freem(mp); return; }

	/*
	 * Checksum extended header and data
	 */
	j = n->t_sum; n->t_sum = 0;
#ifdef vax
	if (tlen == 20) {
		asm("addl3 $8,r10,r0; movl (r0)+,r1; addl2 (r0)+,r1");
		asm("adwc (r0)+,r1; adwc (r0)+,r1; adwc (r0)+,r1");
		asm("adwc (r0)+,r1; adwc (r0)+,r1; adwc (r0)+,r1");
		asm("adwc $0,r1; ashl $-16,r1,r0; addw2 r0,r1");
		asm("adwc $0,r1");		/* ### */
		asm("mcoml r1,r1; movzwl r1,r1; subl2 r1,r9");
	} else
#endif
		j -= cksum(mp, sizeof (struct ip) + tlen);
	if (j != 0) {
		netstat.t_badsum++;
		if (nosum == 0) {
			m_freem(mp);
			return;
		}
	}

	/*
	 * Find tcb for message (SHOULDN'T USE LINEAR SEARCH!)
	 */
	for (tp = tcb_head; tp != 0; tp = tp->t_tcb_next)
		if (tp->t_lport == lport && tp->t_fport == fport &&
		    tp->t_ucb->uc_host->h_addr.s_addr == n->t_s.s_addr)
			goto found;
	for (tp = tcb_head; tp != 0; tp = tp->t_tcb_next)
		if (tp->t_lport == lport &&
		    (tp->t_fport==fport || tp->t_fport==0) &&
		    (tp->t_ucb->uc_host->h_addr.s_addr == n->t_s.s_addr ||
		     tp->t_ucb->uc_host->h_addr.s_addr == 0))
			goto found;
	goto notwanted;
found:

	/*
	 * Byte swap header
	 */
	n->t_len = tlen - hlen;
	n->t_src = fport;
	n->t_dst = lport;
	n->t_seq = ntohl(n->t_seq);
	n->t_ackno = ntohl(n->t_ackno);
	n->t_win = ntohs(n->t_win);
	n->t_urp = ntohs(n->t_urp);

	/*
	 * Check segment seq # and do rst processing
	 */
	switch (tp->t_state) {

	case LISTEN:
		if ((n->th_flags&TH_ACK) || !syn_ok(tp, n)) {
			tcp_sndrst(tp, n);
			goto badseg;
		}
		if (n->th_flags&TH_RST)
			goto badseg;
		goto goodseg;

	case SYN_SENT:
		if (!ack_ok(tp, n) || !syn_ok(tp, n)) {
			tcp_sndrst(tp, n);			/* 71,72,75 */
			goto badseg;
		}
		if (n->th_flags&TH_RST) {
			tcp_close(tp, URESET);			/* 70 */
			tp->t_state = CLOSED;
			goto badseg;
		}
		goto goodseg;

	default:
        	if ((n->th_flags&TH_RST) == 0)
			goto common;
		if (n->t_seq < tp->rcv_nxt)		/* bad rst */
			goto badseg;				/* 69 */
		switch (tp->t_state) {

		case L_SYN_RCVD:
			if (ack_ok(tp, n) == 0)
				goto badseg;			/* 69 */
			tp->t_rexmt = 0;
			tp->t_rexmttl = 0;
			tp->t_persist = 0;
			h_free(tp->t_ucb->uc_host);
			tp->t_state = LISTEN;
			goto badseg;

		default:
			tcp_close(tp, URESET);			/* 66 */
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
		if (syn_ok(tp, n) && n->t_seq != tp->irs) {
			tcp_sndnull(tp);				/* 74 */
			goto badseg;
		}
		goto goodseg;
	}
badseg:
	m_freem(mp);
	return;

goodseg:
#ifdef notdef
	/*
	 * Defer processing if no buffer space for this connection.
	 */
	up = tp->t_ucb;
	if (up->uc_rcc > up->uc_rhiwat && 
	     && n->t_len != 0 && mbstat.m_bufs < mbstat.m_lowat) {
		mp->m_act = (struct mbuf *)0;
		if ((m = tp->t_rcv_unack) != NULL) {
			while (m->m_act != NULL)
				m = m->m_act;
			m->m_act = mp;
		} else
			tp->t_rcv_unack = mp;
		return;
	}
#endif

	/*
	 * Discard ip header, and do tcp input processing.
	 */
	hlen += sizeof(struct ip);
	mp->m_off += hlen;
	mp->m_len -= hlen;
	nstate = tp->t_state;
	tp->tc_flags &= ~TC_NET_KEEP;
	acounts[tp->t_state][INRECV]++;
#ifdef TCPDEBUG
	if ((tp->t_ucb->uc_flags & UDEBUG) || tcpconsdebug) {
		tdb_setup(tp, n, INRECV, &tdb);
	} else
		tdb.td_tod = 0;
#endif
	switch (tp->t_state) {

	case LISTEN:
		if (!syn_ok(tp, n) ||
		    ((tp->t_ucb->uc_host = h_make(&n->t_s)) == 0)) {
			nstate = EFAILEC;
			goto done;
		}
		tp->t_fport = n->t_src;
		tp->t_ucb->uc_template = tcp_template(tp);
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
			if (n->th_flags&TH_ACK) {
				if (n->t_ackno > tp->iss)
					present_data(tp);	/* 32 */
			} else {
				tp->t_finack = T_2ML;		/* 9 */
				tp->tc_flags &= ~TC_WAITED_2_ML;
			}
			nstate = CLOSE_WAIT;
			goto done;
		}
		if (n->th_flags&TH_ACK) {
			present_data(tp);			/* 11 */
			nstate = ESTAB;
		} else
			nstate = SYN_RCVD;			/* 8 */
		goto done;

	case SYN_RCVD:
	case L_SYN_RCVD:
		if ((n->th_flags&TH_ACK) == 0 ||
		    (n->th_flags&TH_ACK) && n->t_ackno <= tp->iss) {
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
		present_data(tp);
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
			nstate = j ? TIME_WAIT : CLOSING1;	/* 28:26 */
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
		if (n->th_flags&TH_FIN) {
			if ((n->th_flags&TH_ACK) &&
			    n->t_ackno <= tp->seq_fin) {
				tcp_ctldat(tp, n, 0);		/* 30 */
				tp->t_finack = T_2ML;
				tp->tc_flags &= ~TC_WAITED_2_ML;
			} else
				tcp_sndctl(tp);			/* 31 */
			goto done;
		}
		goto input;

	case CLOSING1:
		j = ack_fin(tp, n);
		if (n->th_flags&TH_FIN) {
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
					tcp_close(tp, UCLOSED);	/* 15 */
					nstate = CLOSED;
				} else
					nstate = RCV_WAIT;	/* 18 */
			else
				nstate = TIME_WAIT;
			goto done;
		}
		goto input;

	case CLOSING2:
		if (ack_fin(tp, n)) {
			if (rcv_empty(tp)) {			/* 16 */
				tcp_close(tp, UCLOSED);
				nstate = CLOSED;
			} else
				nstate = RCV_WAIT;		/* 19 */
			goto done;
		}
		if (n->th_flags&TH_FIN) {
			tcp_sndctl(tp);				/* 31 */
			goto done;
		}
		goto input;

	case RCV_WAIT:
		if ((n->th_flags&TH_FIN) && (n->th_flags&TH_ACK) &&
		    n->t_ackno <= tp->seq_fin) {
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
			m_freem(mp);
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
	mp->m_len = sizeof(struct th);
#define xchg(a,b) j=a; a=b; b=j
	xchg(n->t_d.s_addr, n->t_s.s_addr); xchg(n->t_dst, n->t_src);
#undef xchg
	if (n->th_flags&TH_ACK)
		n->t_seq = n->t_ackno;
	else {
		n->t_ackno = htonl(ntohl(n->t_seq) + tlen - hlen);
		n->t_seq = 0;
	}
	n->th_flags = TH_RST; /* not TH_FIN, TH_SYN */
	n->th_flags ^= TH_ACK;
	n->t_len = htons(TCPSIZE);
	n->t_off = 5;
	n->t_sum = cksum(mp, sizeof(struct th));
	((struct ip *)n)->ip_len = sizeof(struct th);
	ip_output(mp);
	netstat.t_badsegs++;
}

tcp_ctldat(tp, n, dataok)
	register struct tcb *tp;
	register struct th *n;
{
	register sent;
	register struct ucb *up;
	register struct mbuf *m, *mn;
	register len;
COUNT(RCV_CTLDAT);

	tp->tc_flags &= ~(TC_DROPPED_TXT|TC_ACK_DUE|TC_NEW_WINDOW);
/* syn */
	if ((tp->tc_flags&TC_SYN_RCVD) == 0 && (n->th_flags&TH_SYN)) {
		tp->irs = n->t_seq;
		tp->rcv_nxt = n->t_seq + 1;
		tp->snd_wl = tp->rcv_urp = tp->irs;
		tp->tc_flags |= (TC_SYN_RCVD|TC_ACK_DUE);
	}
/* ack */
	if ((n->th_flags&TH_ACK) && (tp->tc_flags&TC_SYN_RCVD) &&
	    n->t_ackno > tp->snd_una) {
		up = tp->t_ucb;

		/* update snd_una and snd_nxt */
		tp->snd_una = n->t_ackno;
		if (tp->snd_una > tp->snd_nxt)
			tp->snd_nxt = tp->snd_una;

		/* if timed msg acked, set retrans time value */
		if ((tp->tc_flags&TC_SYN_ACKED) &&
		    tp->snd_una > tp->t_xmt_val) {
			tp->t_xmtime = (tp->t_xmt != 0 ? tp->t_xmt : T_REXMT);
			if (tp->t_xmtime > T_REMAX)
				tp->t_xmtime = T_REMAX;
		}

		/* remove acked data from send buf */
		len = tp->snd_una - tp->snd_off;
		m = up->uc_sbuf;
		while (len > 0 && m != NULL)
			if (m->m_len <= len) {
				len -= m->m_len;
				if (m->m_off > MMAXOFF)
					up->uc_ssize -= NMBPG;
				MFREE(m, mn);
				m = mn;
				up->uc_ssize--;
			} else {
				m->m_len -= len;
				m->m_off += len;
				break;
			}
		up->uc_sbuf = m;
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
		netwakeup(tp->t_ucb);		/* wasteful */
	}
/* win */
	if ((tp->tc_flags & TC_SYN_RCVD) && n->t_seq >= tp->snd_wl) {
		tp->snd_wl = n->t_seq;
		tp->snd_wnd = n->t_win;
		tp->tc_flags |= TC_NEW_WINDOW;
		tp->t_persist = 0;
	}
	if (dataok) {
/* text */
		if (n->t_len != 0)
			tcp_text(tp, n);
/* urg */
		if (n->th_flags&TH_URG) {
			unsigned urgent;

			urgent = n->t_urp + n->t_seq;
			if (tp->rcv_nxt < urgent) {
				if (tp->rcv_urp <= tp->rcv_nxt)
					to_user(tp->t_ucb, UURGENT);
				tp->rcv_urp = urgent;
			}
		}
/* eol */
		if ((n->th_flags&TH_EOL) &&
		    (tp->tc_flags&TC_DROPPED_TXT) == 0 &&
		    tp->t_rcv_prev != (struct th *)tp) {
			/* mark last mbuf */
			m = dtom(tp->t_rcv_prev);
			if (m != NULL) {
				while (m->m_next != NULL)
					m = m->m_next;
				m->m_act =
				    (struct mbuf *)(m->m_off + m->m_len - 1);
			}
		}
	}
/* fin */
	if ((n->th_flags&TH_FIN) && (tp->tc_flags&TC_DROPPED_TXT) == 0) {
		int last;

		if ((tp->tc_flags&TC_FIN_RCVD) == 0) {
			/* do we really have fin ? */
			last = firstempty(tp);
			if (tp->t_rcv_prev == (struct th *)tp ||
			    last == t_end(tp->t_rcv_prev)) {
				tp->tc_flags |= TC_FIN_RCVD;
				netwakeup(tp->t_ucb);		/* poke */
			}
			if ((tp->tc_flags&TC_FIN_RCVD) &&
			    tp->rcv_nxt >= last) {
				tp->rcv_nxt = last + 1;		/* fin seq */
				tp->tc_flags |= TC_ACK_DUE;
			}
		} else
			tp->tc_flags |= TC_ACK_DUE;
	}

/* respond */
	sent = 0;
	if (tp->tc_flags&TC_ACK_DUE)
		sent = tcp_sndctl(tp);
	else if (tp->tc_flags&TC_NEW_WINDOW) {
		seq_t last = tp->snd_off;
		up = tp->t_ucb;
		for (m = up->uc_sbuf; m != NULL; m = m->m_next)
			last += m->m_len;
		if (tp->snd_nxt <= last || (tp->tc_flags&TC_SND_FIN))
			sent = tcp_send(tp);
	}

/* set for retrans */
	if (!sent && tp->snd_una < tp->snd_nxt &&
	    (tp->tc_flags&TC_CANCELLED)) {
		tp->t_rexmt = tp->t_xmtime;
		tp->t_rexmttl = T_REXMTTL;
		tp->t_rexmt_val = tp->t_rtl_val = tp->snd_lst;
		tp->tc_flags &= ~TC_CANCELLED;
	}
}

tcp_text(tp, n)
	register struct tcb *tp;
	register struct th *n;
{
	register int i;
	register struct th *p, *q;
	register struct mbuf *m;
	int overage;
COUNT(RCV_TEXT);

	/*
	 * Discard duplicate data already passed to user.
	 */
	if (SEQ_LT(n->t_seq, tp->rcv_nxt)) {
		i = tp->rcv_nxt - n->t_seq;
		if (i >= n->t_len)
			goto dropseg;
		n->t_seq += i;
		n->t_len -= i;
		m_adj(dtom(n), i);
	}

	/*
	 * Find a segment which begins after this one does.
	 */
	for (q = tp->t_rcv_next; q != (struct th *)tp; q = q->t_next)
		if (SEQ_GT(q->t_seq, n->t_seq))
			break;

	/*
	 * If there is a preceding segment, it may provide some of
	 * our data already.  If so, drop the data from the incoming
	 * segment.  If it provides all of our data, drop us.
	 */
	if (q->t_prev != (struct th *)tp) {
		/* conversion to int (in i) handles seq wraparound */
		i = q->t_prev->t_seq + q->t_prev->t_len - n->t_seq;
		if (i > 0) {
			if (i >= n->t_len)
				goto dropseg;
			m_adj(dtom(tp), i);
			n->t_len -= i;
			n->t_seq += i;
		}
	}

	/*
	 * While we overlap succeeding segments trim them or,
	 * if they are completely covered, dequeue them.
	 */
	while (q != (struct th *)tp && SEQ_GT(n->t_seq + n->t_len, q->t_seq)) {
		i = (n->t_seq + n->t_len) - q->t_seq;
		if (i < q->t_len) {
			q->t_len -= i;
			m_adj(dtom(q), i);
			break;
		}
		q = q->t_next;
		m_freem(dtom(q->t_prev));
		remque(q->t_prev);
	}

	/*
	 * Stick new segment in its place.
	 */
	insque(n, q->t_prev);
	tp->seqcnt += n->t_len;

#ifdef notdef
	/*
	 * Calculate available space and discard segments for
	 * which there is too much.
	 */
	q = tp->t_rcv_prev;
	overage = 
	    (tp->t_socket->uc_rcc + tp->rcv_seqcnt) - tp->t_socket->uc_rhiwat;
	if (overage > 0)
		for (;;) {
			i = MIN(q->t_len, overage);
			overage -= i;
			q->t_len -= i;
			m_adj(q, -i);
			if (q == n)
				tp->tc_flags |= TC_DROPPED_TXT;
			if (q->t_len)
				break;
			if (q == n)
				panic("tcp_text dropall");
			q = q->t_prev;
			remque(q->t_next);
		}
#endif

	/*
	 * Advance rcv_next through
	 * newly completed sequence space
	 * and return forcing an ack.
	 */
	while (n->t_seq == tp->rcv_nxt) {
		/* present data belongs here */
		tp->rcv_nxt += n->t_len;
		n = n->t_next;
		if (n == (struct th *)tp)
			break;
	}
	tp->tc_flags |= (TC_ACK_DUE|TC_NET_KEEP);
	return;

dropseg:
	/* don't set TC_NET_KEEP, so that mbuf's will get dropped */
	return;
}

#define	socket		ucb			/* ### */
#define	t_socket	t_ucb			/* ### */

present_data(tp)
	register struct tcb *tp;
{
	register struct th *t;
	register struct socket *up;
	register struct mbuf *m, **mp;
	seq_t ready;
COUNT(PRESENT_DATA);

	/* connection must be synced and data available for user */
	if ((tp->tc_flags&TC_SYN_ACKED) == 0)
		return;
	up = tp->t_socket;
	mp = &up->uc_rbuf;
	while (*mp)
		mp = &(*mp)->m_next;
	t = tp->t_rcv_next;
	/* SHOULD PACK DATA IN HERE */
	while (t != (struct th *)tp && t->t_seq < tp->rcv_nxt) {
		remque(t);
		m = dtom(t);
		up->uc_rcc += t->t_len;
		tp->seqcnt -= t->t_len;
		if (tp->seqcnt < 0) panic("present_data");
		t = t->t_next;
		while (m) {
			if (m->m_len == 0) {
				m = m_free(m);
				continue;
			}
			*mp = m;
			mp = &m->m_next;
			m = *mp;
		}
	}
	if (up->uc_rcc != 0)
		netwakeup(up);
	if ((tp->tc_flags&TC_FIN_RCVD) &&			/* ### */
	    (tp->tc_flags&TC_USR_CLOSED) == 0 &&		/* ### */
	    rcv_empty(tp))					/* ### */
		to_user(up, UCLOSED);				/* ### */
}
