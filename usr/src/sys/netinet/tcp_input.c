/* tcp_input.c 1.3 81/10/25 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../bbnnet/net.h"
#include "../bbnnet/mbuf.h"
#include "../bbnnet/host.h"
#include "../bbnnet/imp.h"
#include "../bbnnet/ucb.h"
#include "../bbnnet/tcp.h"
#include "../bbnnet/ip.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/inode.h"
#include "../bbnnet/fsm.h"

extern int nosum;

tcp_input(mp)
	register struct mbuf *mp;
{
	register struct tcb *tp;
	register struct th *n;
	int nstate;
	struct mbuf *m;
	struct ucb *up;
	int hlen, tlen, j;
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
	if (j != cksum(mp, sizeof (struct ip) + tlen)) {
		netstat.t_badsum++;
		if (nosum == 0) {
			m_freem(mp);
			return;
		}
	}

	/*
	 * Find tcb for message (SHOULDN'T USE LINEAR SEARCH!)
	 */
	for (tp = netcb.n_tcb_head; tp != 0; tp = tp->t_tcb_next)
		if (tp->t_lport == lport && tp->t_fport == fport &&
		    tp->t_ucb->uc_host->h_addr.s_addr == n->t_s.s_addr)
			goto found;
	for (tp = netcb.n_tcb_head; tp != 0; tp = tp->t_tcb_next)
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
			send_rst(tp, n);
			goto badseg;
		}
		if (n->th_flags&TH_RST)
			goto badseg;
		goto goodseg;

	case SYN_SENT:
		if (!ack_ok(tp, n) || !syn_ok(tp, n)) {
			send_rst(tp, n);			/* 71,72,75 */
			goto badseg;
		}
		if (n->th_flags&TH_RST) {
			t_close(tp, URESET);			/* 70 */
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
			t_close(tp, URESET);			/* 66 */
			tp->t_state = CLOSED;
			goto badseg;
		}
		/*NOTREACHED*/

	case SYN_RCVD:
common:
		if (ack_ok(tp, n) == 0) {
			send_rst(tp, n);			/* 74 */
			goto badseg;
		}
		if (syn_ok(tp, n) && n->t_seq != tp->irs) {
			send_null(tp);				/* 74 */
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
	if ((int)up->uc_rcv - (int)up->uc_rsize <= 0
	     && n->t_len != 0 && netcb.n_bufs < netcb.n_lowat) {
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
		rcv_ctldat(tp, n, 1);
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
		rcv_ctldat(tp, n, 1);
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
		rcv_ctldat(tp, n, 1);				/* 39 */
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
				rcv_ctldat(tp, n, 0);		/* 30 */
				tp->t_finack = T_2ML;
				tp->tc_flags &= ~TC_WAITED_2_ML;
			} else
				send_ctl(tp);			/* 31 */
			goto done;
		}
		goto input;

	case CLOSING1:
		j = ack_fin(tp, n);
		if (n->th_flags&TH_FIN) {
			rcv_ctldat(tp, n, 0);
			tp->t_finack = T_2ML;
			tp->tc_flags &= ~TC_WAITED_2_ML;
			if (j)
				nstate = TIME_WAIT;		/* 23 */
			goto done;
		}
		if (j) {
			if (tp->tc_flags&TC_WAITED_2_ML)
				if (rcv_empty(tp)) {
					t_close(tp, UCLOSED);	/* 15 */
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
				t_close(tp, UCLOSED);
				nstate = CLOSED;
			} else
				nstate = RCV_WAIT;		/* 19 */
			goto done;
		}
		if (n->th_flags&TH_FIN) {
			send_ctl(tp);				/* 31 */
			goto done;
		}
		goto input;

	case RCV_WAIT:
		if ((n->th_flags&TH_FIN) && (n->th_flags&TH_ACK) &&
		    n->t_ackno <= tp->seq_fin) {
			rcv_ctldat(tp, n, 0);
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

rcv_ctldat(tp, n, dataok)
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
			rcv_text(tp, n);
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
	if (tp->tc_flags&TC_ACK_DUE)
		sent = send_ctl(tp);
	else if (tp->tc_flags&TC_NEW_WINDOW)
		sent = send(tp);
	else
		sent = 0;

/* set for retrans */
	if (!sent && tp->snd_una < tp->snd_nxt &&
	    (tp->tc_flags&TC_CANCELLED)) {
		tp->t_rexmt = tp->t_xmtime;
		tp->t_rexmttl = T_REXMTTL;
		tp->t_rexmt_val = tp->t_rtl_val = tp->snd_lst;
		tp->tc_flags &= ~TC_CANCELLED;
	}
}

rcv_text(tp, t)
	register struct tcb *tp;
	register struct th *t;
{
	register i;
	register struct th *p, *q;
	register struct mbuf *m, *n;
	struct th *savq;
	int last, j, k;
COUNT(RCV_TEXT);

	/* throw away any data we have already received */
	if ((i = tp->rcv_nxt - t->t_seq) > 0)  {
		if (i >= t->t_len)
			return;
		t->t_seq += i;
		t->t_len -= i;
		m_adj(dtom(t), i);
	}

	last = t_end(t);                /* last seq # in incoming seg */
	i = rcv_resource(tp);           /* # buffers available to con */

	/* count buffers in segment */

	for (m = dtom(t), j = 0; m != NULL; m = m->m_next)
		if (m->m_len != 0) {
        		j++;
			if (m->m_off > MMAXOFF)
				j += NMBPG;
		}

	/* not enough resources to process segment */

	if (j > i && netcb.n_bufs < netcb.n_lowat) {

		/* if segment preceeds top of seqeuncing queue, try to take
		   buffers from bottom of queue */

                q = tp->t_rcv_next;
		if (q != (struct th *)tp && tp->rcv_nxt < q->t_seq &&
		    t->t_seq < q->t_seq)

			for (k=j-i, p = tp->t_rcv_prev; k > 0 &&
			     p != (struct th *)tp; k--) {
				savq = p->t_prev;
				tcp_deq(p);
				i += m_freem(dtom(p));
				p = savq;
			}

		/* if still not enough room, drop text from end of segment */

		if (j > i) {

			for (m = dtom(t); i > 0 && m != NULL; i--)
				m = m->m_next;

        		while (m != NULL) {
        			t->t_len -= m->m_len;
        			last -= m->m_len;
        			m->m_len = 0;
        			m = m->m_next;
        		}
        		tp->tc_flags |= TC_DROPPED_TXT;
        		if (last < t->t_seq)
        			return;
        	}
	}

	/* merge incoming data into the sequence queue */

        q = tp->t_rcv_next;             /* -> top of sequencing queue */

        /* skip frags which new doesn't overlap at end */

        while ((q != (struct th *)tp) && (t->t_seq > t_end(q)))
        	q = q->t_next;

        if (q == (struct th *)tp) {     /* frag at end of chain */

		if (last >= tp->rcv_nxt) {
		        tp->tc_flags |= TC_NET_KEEP;
        	        tcp_enq(t, tp->t_rcv_prev);
		}

        } else {

		/* frag doesn't overlap any on chain */

        	if (last < q->t_seq) {
			tp->tc_flags |= TC_NET_KEEP;
        		tcp_enq(t, q->t_prev);

        	/* new overlaps beginning of next frag only */

        	} else if (last < t_end(q)) {
        		if ((i = last - q->t_seq + 1) < t->t_len) {
                		t->t_len -= i;
        			m_adj(dtom(t), -i);
				tp->tc_flags |= TC_NET_KEEP;
        			tcp_enq(t, q->t_prev);
        		}

        	/* new overlaps end of previous frag */

        	} else {
        		savq = q;
        		if (t->t_seq <= q->t_seq) {     /* complete cover */
        			savq = q->t_prev;
        			tcp_deq(q);
        			m_freem(dtom(q));

        		} else {                        /* overlap */
        			if ((i = t_end(q) - t->t_seq + 1) < t->t_len) {
                			t->t_seq += i;
                			t->t_len -= i;
                			m_adj(dtom(t), i);
				} else
					t->t_len = 0;
        		}

        	/* new overlaps at beginning of successor frags */

        		q = savq->t_next;
        		while ((q != (struct th *)tp) && (t->t_len != 0) &&
        			(q->t_seq < last))

        			/* complete cover */

        			if (t_end(q) <= last) {
        				p = q->t_next;
        				tcp_deq(q);
        				m_freem(dtom(q));
        				q = p;

        			} else {        /* overlap */

        				if ((i = last - q->t_seq + 1) < t->t_len) {
                				t->t_len -= i;
                				m_adj(dtom(t), -i);
					} else
						t->t_len = 0;
        				break;
        			}

        	/* enqueue whatever is left of new before successors */

        		if (t->t_len != 0) {
				tp->tc_flags |= TC_NET_KEEP;
        			tcp_enq(t, savq);
			}
        	}
        }

	/* set to ack completed data (no gaps) */

	tp->rcv_nxt = firstempty(tp);
	tp->tc_flags |= TC_ACK_DUE;

#ifdef notdef
	/* THIS CODE CANT POSSIBLY WORK */
	/* if any room remaining in rcv buf, take any unprocessed
	   messages and schedule for later processing */

	i = rcv_resource(tp);

	while ((m = tp->t_rcv_unack) != NULL && i > 0) {

		/* schedule work request */

		t = (struct th *)((int)m + m->m_off);
		j = (t->t_off << 2) + sizeof(struct ip);
		m->m_off += j;
		m->m_len -= j;
		tp->t_rcv_unack = m->m_act;
		m->m_act = (struct mbuf *)0;
		netstat.t_unack++;
		tcp_work(INRECV, 0, tp, t);

		/* remaining buffer space */

		for (n = m; n != NULL; n = n->m_next)
			i--;
	}
#endif
}

present_data(tp)
	register struct tcb *tp;
{
	register struct th *t;
	register struct ucb *up;
	register struct mbuf *m, **mp;
	seq_t ready;
COUNT(PRESENT_DATA);

	/* connection must be synced and data available for user */
	if (((tp->tc_flags&TC_SYN_ACKED) == 0) ||
	    (t = tp->t_rcv_next) == (struct th *)tp)
		return;
	up = tp->t_ucb;
	ready = firstempty(tp);     /* seq # of last complete datum */
	mp = &up->uc_rbuf;
	while (*mp)
		mp = &(*mp)->m_next;
	while (up->uc_rsize < up->uc_rcv && t != (struct th *) tp &&
	    t_end(t) < ready) {
		tcp_deq(t);
		m = dtom(t);
		t = t->t_next;
		while (m) {
			if (m->m_len == 0) {
				m = m_free(m);
				continue;
			}
			up->uc_rsize++;
			if (m->m_off > MMAXOFF)
				up->uc_rsize += NMBPG;
			if (*mp == 0)
				*mp = m;
			mp = &m->m_next;
			m = *mp;
		}
	}
	if (up->uc_rsize != 0)
		netwakeup(up);
	/*
	 * Let user know about foreign tcp close if no more data.
	 */
	if ((tp->tc_flags&TC_FIN_RCVD) && (tp->tc_flags&TC_USR_CLOSED) == 0 &&
	    rcv_empty(tp))
		to_user(up, UCLOSED);
}

#ifdef TCPDEBUG
tdb_setup(tp, n, input, tdp)
	struct tcb *tp;
	register struct th *n;
	int input;
	register struct tcp_debug *tdp;
{

	tdp->td_tod = time;
	tdp->td_tcb = tp;
	tdp->td_old = tp->t_state;
	tdp->td_inp = input;
	tdp->td_tim = 0;
	tdp->td_new = -1;
	if (n) {
		tdp->td_sno = n->t_seq;
		tdp->td_ano = n->t_ackno;
		tdp->td_wno = n->t_win;
		tdp->td_lno = n->t_len;
		tdp->td_flg = n->th_flags;
	} else
		tdp->td_sno = tdp->td_ano = tdp->td_wno = tdp->td_lno =
		    tdp->td_flg = 0;
}

tdb_stuff(tdp, nstate)
	struct tcp_debug *tdp;
	int nstate;
{

	tdp->td_new = nstate;
	tcp_debug[tdbx++ % TDBSIZE] = *tdp;
	if (tcpconsdebug & 2)
		tcp_prt(tdp);
}
#endif
