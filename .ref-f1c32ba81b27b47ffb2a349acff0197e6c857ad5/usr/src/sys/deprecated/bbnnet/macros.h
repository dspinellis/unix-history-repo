
	/*** macros to simulate action() ***/

extern int tcprint;

#define TCP_DEBUG(soptr,tptr,wptr,act,newstate) \
	if (soptr){ \
		if ((soptr)->so_options & SO_DEBUG) { \
			if (tcprint) \
				printf("via 0x%x, ", fsactab[act]); \
			tcp_debug(tptr, wptr, newstate); \
		} \
	}


/*
 * Simulates calls to action.
 *	tp	valid tcpcb pointer ( == wp->w_tcb )
 *	so	valid socket pointer ( == tp->t_in_pcb->inp_socket )
 *	wp	valid work pointer
 *	wtype	== wp->w_type, used so compiler can remove constant conditionals
 *	wdat	== wp->w_dat
 *
 *	act, newstate	integers
 *
 * Remember that if the state transition results in CLOSED, then we have
 * lost the mbuf(s) containing the tcpcb...
 *
 * moved tcp->net_keep to tcp_net_keep to avoid race condition since don't
 * always have MBUF holding tcp after state transition function returns.
 */
extern int tcp_net_keep;

#define ACTION(tp, so, wp, wtype, wdat, act, newstate) \
{	act = fstab[(tp)->t_state][wtype]; \
	if (act == 0)  { \
		/* \
		 * invalid state transition, just print a message and ignore \
		 */ \
		printf("tcp bad state: tcb=%x state=%d input=%d\n", \
			tp, (tp)->t_state, wtype); \
		if (wdat != NULL && wtype == INRECV) \
			m_freem(dtom(wdat)); \
	} else { \
		tcp_net_keep = FALSE; \
		newstate = (*fsactab[act])(wp); \
		TCP_DEBUG (so, tp, wp, act, newstate); \
		if (wdat != NULL && !tcp_net_keep && wtype == INRECV) \
			m_freem(dtom(wdat)); \
		if ((newstate != SAME) && (newstate != CLOSED)) \
			(tp)->t_state = newstate; \
	} \
}

extern char	fstab[TCP_NSTATES][INOP];
extern int	(*fsactab[])();

/*
 * like w_alloc() macro, but suitable for above ACTION.
 */
#define W_ALLOC(type, stype, tp, m, so, act, newstate) \
{									\
	struct work w; \
	w.w_type = type; w.w_stype = stype; w.w_tcb = tp; w.w_dat = (char *)m; \
	ACTION(tp, so, &w, type, m, act, newstate); \
}


/*
 * Enqueue/dequeue segment on tcp sequencing queue
 */
#define TCP_ENQ(new, list, tp) \
{	(tp)->t_rcv_len += (new)->t_len; \
	insque(new, list); \
}

#define TCP_DEQ(old, tp) \
{	(tp)->t_rcv_len -= (old)->t_len; \
	remque(old); \
}

/*
 * Macro form of firstempty().  Find the first empty spot in rcv buffer.
 */
#define FIRSTEMPTY(tp, retval) \
{	register struct th *p; \
 \
	if ((p = (tp)->t_rcv_next) == (struct th *)(tp) || \
	    SEQ_LT((tp)->rcv_nxt, p->t_seq)) \
		retval = (tp)->rcv_nxt; \
	else { \
		register struct th *q; \
 \
		while ((q = p->t_next) != (struct th *)(tp) && \
		       SEQ_EQ(t_end(p)+1, q->t_seq)) \
			p = q; \
 \
		retval = t_end(p) + 1; \
	}}

/*
 * macro form of present_data().
 */
extern struct mbuf *extract_oob();

#define PRESENT_DATA(tp) \
{ \
	/* connection must be synced and data available for user */ \
 \
	if ((tp)->syn_acked){ \
		register struct th *t; \
			 struct socket *so; \
 \
		so = (tp)->t_in_pcb->inp_socket; \
		if ((t = (tp)->t_rcv_next) != (struct th *)(tp)) { \
		    /* \
		     * move as many mbufs as possible from tcb \
		     * to user queue.  Used to use firstempty(), \
		     * but that caused traversal of list twice. \
		     */ \
		    if (SEQ_LEQ(t->t_seq, (tp)->rcv_nxt)) { \
		        register struct sockbuf *sorcv; \
			register int	done; \
			register struct mbuf *m; \
			register struct th *next; \
 \
		        sorcv = &so->so_rcv; \
			done = FALSE; \
			while (sbspace(sorcv) > 0 && !done) { \
				/* \
				 * Note order of events: sbappend tries to \
				 * coalesce mbufs, so if get a packet in, it \
				 * may use the mbuf that sbappend may free. \
				 */ \
 \
				/* dequeue chunk from tcb */ \
 \
				next = t->t_next; \
				TCP_DEQ(t, tp); \
				m = dtom(t); \
 \
				/* \
				 * check for end of list and gaps. \
				 */ \
				if ((next == (struct th *)tp) || \
				    (t_end(t)+1 != next->t_seq)) \
					done = TRUE; \
 \
				/* SS_CANTRCVMORE == usr_abort */  \
				if (so->so_state & SS_CANTRCVMORE) \
					m_freem(m); \
				else { \
					/* \
					 * remove urgent data from input stream\
					 */ \
					if (SEQ_GEQ((tp)->rcv_urpend, (tp)->rcv_urp)) \
						m = extract_oob(tp, m, sorcv); \
 \
					if (m) \
						/* \
						 * chain new data to user \
						 * receive buf \
						 */ \
						sbappend(sorcv, m); \
				} \
 \
				t = next; \
			} \
 \
		        /* awaken reader only if any data on user rcv queue */ \
		        if (sorcv->sb_cc > 0) \
				sbwakeup(sorcv); \
		    } \
		} \
 \
		/* let user know about foreign tcp close if no more data \
		 * OR if no data ever transferred. \
		 */ \
 \
		if ((tp)->fin_rcvd && /* !tp->usr_closed && */ rcv_empty(tp)) \
			socantrcvmore(so); \
	}}
