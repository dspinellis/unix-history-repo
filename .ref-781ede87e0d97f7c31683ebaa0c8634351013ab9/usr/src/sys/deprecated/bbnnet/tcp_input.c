#ifdef	RCSIDENT
static char rcsident[] = "$Header: tcp_input.c,v 1.25 85/07/31 09:33:47 walsh Exp $";
#endif

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/kernel.h"
#include "../h/inode.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/syslog.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../bbnnet/in.h"
#include "../bbnnet/net.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/fsm.h"
#include "../bbnnet/tcp.h"
#include "../bbnnet/seq.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/fsmdef.h"
#include "../bbnnet/macros.h"
#include "../bbnnet/nopcb.h"
#ifdef	HMPTRAPS
#include "../bbnnet/hmp_traps.h"
#endif

#ifdef HMPTRAPS
#define HMP_TRAP(a,b,c)	hmp_trap(a,b,c)
#else
#define HMP_TRAP(a,b,c)
#endif

extern int nosum;
extern struct inpcb tcp;

/*
 * net preproc (66,67,68,69,70,71,72,73,74,75,76)
 *
 * macro form of former function netprepr()
 *
 * tp	valid tcpcb
 * n	valid th
 * inp	valid inpcb ( == tp->t_in_pcb )
 */
#define NETPREPR(tp, n, inp, retval) \
{ \
	retval = (-1);	/* assume bad */ \
			/* tell caller to eat segment (unacceptable) */ \
 \
	switch (tp->t_state) { \
	    case LISTEN: \
		/* Ignore resets, ACKs cause resets, must have SYN. */ \
		if (n->t_flags&T_RST) \
			break; \
		else if (n->t_flags&T_ACK) \
			send_rst(tp, n); \
		else if (n->t_flags&T_SYN) \
			retval = SAME; \
		break; \
\
	case SYN_SENT: \
		/* Bad ACKs cause resets, good resets close, must have SYN. */ \
		if (n->t_flags&T_ACK && (SEQ_GEQ(tp->iss, n->t_ackno) || \
					 SEQ_GT(n->t_ackno, tp->snd_hi))) \
			send_rst(tp, n); \
		else if (n->t_flags&T_RST) { \
			if (n->t_flags&T_ACK) { \
				t_close(tp, ECONNREFUSED); \
				retval = CLOSED; \
			} \
		} else if (n->t_flags&T_SYN) \
			retval = SAME; \
		break; \
 \
	case 0: \
		/* \
		 * after bind, but before we've had a chance to \
		 * listen or connect \
		 */ \
		break; \
 \
	default: \
		{ struct sockbuf *sorcv; sequence xend; \
		/* \
		 * Part of packet must fall in window. \
		 * This allows for segments that are partially retransmits \
		 * and partially new. \
		 * otherwise just ACK and drop. \
		 */ \
		sorcv = &inp->inp_socket->so_rcv; \
		xend = n->t_seq; \
		if (n->t_len) \
			/* remember, could be an ACK-only packet */ \
			xend += n->t_len -1; \
		if (n->t_flags & T_FIN) \
			xend ++; /* in case FIN + rxmitted data (TOPS-20) */ \
		if (SEQ_LT(xend, tp->rcv_nxt) || \
		    SEQ_GEQ(n->t_seq, tp->rcv_nxt + sbspace(sorcv))) { \
			tp->t_preproc++; \
			send_tcp(tp, TCP_CTL); \
			HMP_TRAP(T_TCP_WINDOW, (caddr_t)0,0); \
		/* \
		 * Due to 4.2BSD net architecture, don't need to send \
		 * L_SYN_RCVD socket back to LISTEN on reset since server \
		 * socket and communication paths are separate. \
		 */ \
		} else if (n->t_flags&T_RST) { \
			t_close(tp, ENETRESET); \
			retval = CLOSED; \
		/* No SYNs allowed unless *SYN_RCVD */ \
		} else if ((n->t_flags&T_SYN) && (tp->t_state >= ESTAB)) { \
			send_rst(tp, n); \
			t_close(tp, ENETRESET); \
			retval = CLOSED; \
		/* \
		 * Must have good ACK.  Bad ACKs cause resets only in \
		 * SYN_RCVD states.  In other states, this may be a slow pkt? \
		 */ \
		} else if (n->t_flags&T_ACK) \
			if (SEQ_GT(tp->snd_una, n->t_ackno) ||	\
			    SEQ_GT(n->t_ackno, tp->snd_hi)) {	\
				if (tp->t_state == SYN_RCVD ||	\
				    tp->t_state == L_SYN_RCVD)	\
					send_rst(tp, n); \
			} else { \
				/* \
				 * Acceptable segment: \
				 * Reset no activity timer on established and \
				 * closing connections. \
				 */ \
				 if (tp->t_state >= ESTAB) \
					tp->t_timers[TNOACT] = tp->t_noact; \
				retval = SAME; \
}	}	}	}


int	tcp_net_keep;

/*
 * This is the scheduler for the tcp machine.  It is called
 * from the lower network levels, either directly from the
 * internet level, in case of input from the network; or
 * indirectly from netmain, in case of user or timer events
 * which awaken the main loop.
 */
tcp_input(mp, fragsize)
register struct mbuf *mp;
int fragsize;
{
    register struct th *tp;
    register int hlen;
    register struct tcpcb *t;
    register struct inpcb *inp;
    struct mbuf *m;
    int i, tlen;
    struct work w;
    u_short cks;

    tcpstat.t_total ++;

    /*
     * see ip_input()
     */
    if ((mp->m_off > MMAXOFF) || (mp->m_len < sizeof(struct th)))
    {
	if ((mp = m_pullup(mp, sizeof(struct th))) == NULL)
	{
	    tcpstat.t_tooshort ++;
	    return;
	}
    }

    /* set up needed info from ip header, note that beginning
       of tcp header struct overlaps ip header.  ip options
       have been removed by ip level option processing */

    tp = mtod(mp, struct th *);

    /* make sure header does not overflow mbuf */

    hlen = tp->t_off << TCP_OFFSHIFT;
    if (hlen < TCPSIZE)
    {
	ip_log ((struct ip *) tp, "tcp t_off too small");
	netlog(mp);
	return;
    }
    if (hlen > mp->m_len) 
    {
	if ((mp = m_pullup(mp, hlen)) == NULL)
	{
	    ip_log((struct ip *) tp, "tcp header overflow");
#ifdef HMPTRAPS
	    /* hmp_trap(T_TCP_OVFLO, (caddr_t)0, 0); */
#else
	    /* netlog(mp); */
#endif
	    return;
	}
	tp = mtod(mp, struct th *);
    }

    tlen = ((struct ip *)tp)->ip_len;
    tp->t_len = htons((u_short)tlen);
    tp->t_next = NULL;
    tp->t_prev = NULL;
    tp->t_x1 = 0;

    /*
     * do checksum calculation, drop seg if bad
     */
    i = (u_short)tp->t_sum;
    tp->t_sum = 0;
    if (i != (cks = (u_short)in_cksum(mp, tlen + sizeof(struct ip)))) 
    {
	tcpstat.t_badsum++;
	if (! nosum)
	{
#ifdef HMPTRAPS
	    /* hmp_trap(T_TCP_CKSUM, (caddr_t)0,0); */
#endif
	    inet_cksum_err ("tcp", (struct ip *) tp, (u_long) i, (u_long) cks);
	    netlog(mp);
	    return;
	}
    }

    /* find a tcb for incoming message */
    inp = in_pcblookup(&tcp, tp->t_s.s_addr, tp->t_src,
			     tp->t_d.s_addr, tp->t_dst, TRUE);

    if ((inp != NULL) && ((t = (struct tcpcb *)inp->inp_ppcb) != NULL))
    {
	/* found a tcp for message */
	/* byte swap header */

	if ((int)(tp->t_len = tlen - hlen) < 0) 
	{
	    ip_log((struct ip *) tp, "tcp header length");
#ifdef HMPTRAPS
	    /* hmp_trap(T_TCP_HLEN, (caddr_t)0,0); */
#else
	    netlog(mp);
#endif
	    return;
	}
	tp->t_seq = ntohl(tp->t_seq);
	tp->t_ackno = ntohl(tp->t_ackno);
	tp->t_win = ntohs((u_short)tp->t_win);
	tp->t_urp = ntohs((u_short)tp->t_urp);

	/* record the max fragment size */

	t->t_maxfrag = MAX(t->t_maxfrag, fragsize);

	/* do TCP option processing */

	if (hlen > TCPSIZE)
	    tcp_opt(t, tp, hlen);

	/* check seg seq #, do RST processing */

	NETPREPR(t, tp, inp, i);
	if (i != SAME)
	{
	    /* segment failed preprocessing.  Drop it and
	     * possibly enter new state.  For now, always
	     * returns SAME/-1/CLOSED
	     */
	    m_freem(mp);
/*
	    if ((i != -1) && (i != CLOSED))
		t->t_state = i;
*/
	}
	else 
	{
	    if (sbspace(&inp->inp_socket->so_rcv) <= 0 &&
		tp->t_len != 0) 
	    {
		/*
		 * The user's receive q is full.  Either the
		 * remote TCP is not paying attention to the
		 * window, or this is a persistence packet.
		 *
		 * The first reason was once common with
		 * TOPS-20.  Let's conserve network resources
		 * by holding onto the packet in the unack q.
		 * Place it at the end of the list.
		 */
		mp->m_act = NULL;
		if ((m = t->t_rcv_unack) != NULL) 
		{
		    while (m->m_act != NULL)
			m = m->m_act;
		    m->m_act = mp;
		}
		else
		    t->t_rcv_unack = mp;

		/*
		 * ACK if it was a window probe, just in case
		 * they have a TNOACT timer running.
		 */
		send_tcp(t, TCP_CTL);
	    }
	    else 
	    {
		int	act, newstate;
		struct socket *so;

		/* set up work entry for seg, and call
		   the fsm to process it */

		hlen += sizeof(struct ip);
		mp->m_off += hlen;
		mp->m_len -= hlen;

		/** HAND CODED action() CALL **/

		w.w_type = INRECV;
		w.w_tcb = t;
		w.w_dat = (char *)tp;

		/* get index of action routine from
		 * transition table
		 */
		act = fstab[t->t_state][INRECV];

		/* invalid state transition, just
		 * print a message and ignore */

		if (act == 0)  
		{
		    log(LOG_INFO, "tcp bad state: tcb=%x state=%d INRECV\n", t, t->t_state);
		    m_freem(mp);
		    return;
		}

		so = t->t_in_pcb->inp_socket;
		tcp_net_keep = FALSE;
		newstate = (*fsactab[act])(&w);

		/* debugging info */
		TCP_DEBUG (so, t, &w, act, newstate);

		/* if CLOSED, lost tcpcb */
		if ((newstate != SAME) && (newstate != CLOSED))
		    t->t_state = newstate;
		if (! tcp_net_keep)
		    m_freem(mp);

		/** END action() **/
	    }
	}
    }
    else
	/* nobody wants it */
	send_uncon_rst (tp, mp, tlen, hlen);
}

send_uncon_rst (n, mp, tlen, hlen)
register struct th	*n;
register struct mbuf	*mp;
{
    struct in_addr tempinaddr;
    u_short tempport;
    int error;

    /* make sure we don't send a RST in response to an RST */

    if (n->t_flags & T_RST) 
    {
	m_freem(mp);
	return;
    }

    /* free everything but the header */

    m_freem(mp->m_next);
    mp->m_next = NULL;
    mp->m_len = sizeof(struct th);

    /* form a reset from the packet and send */

    tempinaddr = n->t_d;
    n->t_d = n->t_s;
    n->t_s = tempinaddr;

    tempport = n->t_src;
    n->t_src = n->t_dst;
    n->t_dst = tempport;

    if (n->t_flags&T_ACK)
	n->t_seq = n->t_ackno;
    else
    {
	n->t_ackno = htonl((u_long)
	    ntohl((u_long)n->t_seq)
	    + tlen - hlen
	    + (n->t_flags&T_SYN ? 1 : 0));
	n->t_seq = 0;
    }
    n->t_flags	= (n->t_flags&T_ACK) ? T_RST : T_RST+T_ACK;
    n->t_len	= htons((u_short)TCPSIZE);
    n->t_off	= TCPSIZE >> TCP_OFFSHIFT;
    n->t_sum	= in_cksum(mp, sizeof(struct th));

    NOPCB_IPSEND (mp, TCPSIZE, FALSE, error);
    tcpstat.t_badsegs++;

#ifdef lint
    error = error;
#endif
}

/*
 * Entry into TCP finite state machine
 */
action(wp)
register struct work *wp;
{
    register act, newstate;
    register struct tcpcb *tp;
    register struct socket *so;

    tp = wp->w_tcb;
    so = tp->t_in_pcb->inp_socket;

    ACTION (tp, so, wp, wp->w_type, wp->w_dat, act, newstate);
    return(newstate);
}


struct mbuf *tcpdebuf;
int tcprint;

/*
 * Write a record in the tcp debugging log
 */
tcp_debug(tp, wp, newstate)
register struct tcpcb *tp;
register struct work *wp;
register newstate;
{
    register struct t_debug *dp;
    register struct mbuf *m;

#ifdef TCPDEBUG
    if (tcprint)
    {
	/*
	 * Print debugging info directly on the console (use this for 
	 * intial testing only).
	 */
	printf("TCP(%x) %s X %s", tp, tcpstates[tp->t_state],
	    tcpinputs[wp->w_type]);

	if (wp->w_type == ISTIMER)
	    printf("(%s)", tcptimers[wp->w_stype]);

	printf(" --> %s",
	    tcpstates[ (newstate > 0) ? newstate : tp->t_state]);

	if (newstate < 0)
	    printf(" (FAILED)\n");
	else
	    putchar('\n', 0);
    }
#endif

    /*
     * Get an mbuf to write the debugging record into.  If we don't already
     * have one, allocate a new one.
     */
    if ((m = tcpdebuf) == NULL)
    {
	register struct mbuf *c;

	if ((tcpdebuf = m = m_get(M_DONTWAIT, MT_DATA)) == NULL)
	    return;
	/*
	 * If possible, use a cluster so that we need to wake up the
	 * raw listener less often and reduce likelihood he misses
	 * some information.
	 */
	MCLGET(c, 1);
	if (c) 
	{
	    m->m_off = ((int) c) - ((int) m);
	    m->m_act = (struct mbuf *) TCDBLEN;
	}
	else
	    m->m_act = (struct mbuf *) TDBLEN;
	m->m_len = 0;
    }

    dp = (struct t_debug *) (mtod(m, char *) + m->m_len);
    /*
     * Set up the debugging record.
     */
    dp->t_iptime	= iptime();
    dp->t_input	= wp->w_type;
    dp->t_timer	= wp->w_stype;
    dp->t_newstate	= newstate;
    if (tp != NULL)
    {
	dp->t_oldstate = tp->t_state;
	dp->t_tcb = (*tp);	/* structure copy */
    }
    else
	dp->t_oldstate = 0;

    if (wp->w_type == INRECV) 
    {
	register struct th *n;

	n = (struct th *)wp->w_dat;
	dp->t_hdr = (*n);	/* structure copy */
    }
    /*
     * If the mbuf is full, dispatch it to a raw listener.
     * Also flush if the connection we're debugging closes so that
     * packet-printer/systems analyst sees final transitions.
     */
    m->m_len += sizeof(struct t_debug);
    if ((m->m_len >= ((int) m->m_act)) || (newstate == CLOSED)) 
    {
	m->m_act = 0;
	tcpdebuglog(m);
	tcpdebuf = NULL;
    }
}
