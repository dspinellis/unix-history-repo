#ifdef	RCSIDENT
static char rcsident[] = "$Header: tcp_procs.c,v 1.32 85/07/31 09:34:27 walsh Exp $";
#endif

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/syslog.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../bbnnet/in.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/net.h"
#include "../bbnnet/fsm.h"
#include "../bbnnet/tcp.h"
#include "../bbnnet/seq.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/macros.h"
#ifdef HMPTRAPS
#include "../bbnnet/hmp_traps.h"
#endif

/*
 * TCP finite state machine procedures.
 *
 * Called from finite state machine action routines, these do most of the work
 * of the protocol.  They in turn call primitive routines (in tcp_prim) to
 * perform lower level functions.
 */


/*
 * This works cooperatively with t_close for freeing up data on receive/send
 * buffers.
 */
tcp_pcbdisconnect(inp)
struct inpcb *inp;
{
    register struct tcpcb	*tp;

    if (tp = (struct tcpcb *) inp->inp_ppcb)
    {
	inp->inp_ppcb = (caddr_t) NULL;

	/*
	 * free all data on receive queues
	 */
	{
	    register struct th	*t, *next;

	    t = tp->t_rcv_next;
	    while (t != (struct th *)tp)
	    {
		next = t->t_next;
		m_freem(dtom(t));
		t = next;
	    }
	}
	{
	    register struct mbuf	*m, *next;

	    m = tp->t_rcv_unack;
	    while (m != NULL) 
	    {
		next = m->m_act;
		m_freem(m);
		m = next;
	    }
	}

	if (tp->oob_data)
	    m_freem(tp->oob_data);

	if (tp->t_template)
	    m_free(dtom(tp->t_template));

	m_free(dtom(tp));
    }
}

/*
 * Delete TCB and free all resources used by the connection.  Called after
 * the close protocol is complete.
 */
t_close(tp, state)
register struct tcpcb *tp;
short state;
{
    register struct inpcb *inp;

    /*
     * in_pcbdetach() calls soisdisconnected(), which wakes up the
     * process if it's sleeping.  Need only pass user error code if
     * appropriate (like ENETRESET) and hope he'll close the file
     * descriptor.  Don't need to clear timers since they're in the
     * tcpcb to be deleted.
     */
    inp = tp->t_in_pcb;
    if (!tp->usr_abort)
	inp->inp_socket->so_error = state;
    in_pcbdetach(inp, tcp_pcbdisconnect);
}

short max_ack_skipped = 1;

/*
 * We are in a position where, perhaps, we should send a TCP segment (packet).
 * The important decisions are:
 *	1)  How big a segment should we send?  This is important since most
 *		overhead occurs at the packet level (interrupts, queueing,
 *		header field checks...) and not at the byte level.
 *	2)  Is it worth it to send this packet?  Are we sending enough data
 *		or would we be better off waiting for some more to queue up?
 *
 * The above requirements are the point of view when called in response to
 * a user's write request.  We are also called on packet arrival in order
 * to send an ack (with piggy-backed data), and to respond to window openings
 * by sending any pent up data.
 *
 * Send a TCP segment.  Send data from left window edge of send buffer up to
 * window size or end (whichever is less).  Set retransmission timers.
 *
 * The Ford/Nagle algorithms might be thought of (if outstanding data, only
 * send if packet would be large), but they are primarily for telnet and 
 * that doesn't go with ideas in comments down by push.  Has idea of tcp
 * changed since RFC?
 */
send_tcp(tp, ctl)                        
register struct tcpcb *tp;
int ctl;
{
    register sequence last, wind;
    register int snd_flags;
    register int len;
    struct sockbuf *sosnd;
    int forced, error;
    int sendalot;

    sosnd = &tp->t_in_pcb->inp_socket->so_snd;
    sendalot = FALSE;
    snd_flags = 0;
    tp->snd_lst = tp->snd_nxt;
    forced = FALSE;
    /* 
     * Send SYN if this is first data (ISS)
     */
    if (SEQ_EQ(tp->snd_nxt, tp->iss)) 
    {
	snd_flags |= T_SYN;
	tp->snd_lst++;
    }
    /*
     * Get seq # of last datum in send buffer 
     */
    last = tp->snd_una;
    if (!tp->syn_acked)
	last++;	/* don't forget SYN */
    last += sosnd->sb_cc;
    /*
     * If no data to send in buffer, just do FIN check, otherwise see
     * how much we should send in segment.
     */
    if (SEQ_GEQ(tp->snd_nxt, last)) 
    {
	/*
	 * should send FIN?  don't unless haven't already sent one 
	 */
	if (tp->snd_fin && 
	    (SEQ_EQ(tp->seq_fin, tp->iss) || 
	    SEQ_LEQ(tp->snd_nxt, tp->seq_fin))) 
	{
	    snd_flags |= T_FIN;
	    tp->seq_fin = tp->snd_lst++;
	}
    }
    else if (tp->syn_acked) 
    {
	/*
	 * We can't send more than we have (SYN + data represented
	 * by last).  Nor can we send more than the other end is
	 * prepared to receive (represented by the window in snd_wnd
	 * and wind).
	 *
	 * Only send a segment if there is something in the buffer,
	 * and a non-zero window has been received.
	 */
	wind = tp->snd_una + tp->snd_wnd;
	tp->snd_lst = SEQ_MIN(last, wind);

	/*
	 * Make sure the segment is not larger than the remote end
	 * can handle.  Though they may advertise a 4K window, perhaps
	 * they can only fill it 512 bytes at a time due to some
	 * buffering or device driver constraint.
	 *
	 * If we're both on the local net, the maxseg is probably the
	 * mtu of the local network, and this will avoid some IP
	 * fragmentation.
	 *
	 * ">=" so that set sendalot.
	 */
	if ((len = tp->snd_lst - tp->snd_nxt) >= tp->t_maxseg) 
	{
	    tp->snd_lst -= len - tp->t_maxseg;
	    sendalot = TRUE;
	}

	/*
	 * If we're not on the same net or on similar nets immediately
	 * connected by a gateway, the negotiated maxseg may cause
	 * fragmentation.  Fragmentation per se is not bad, but
	 * tinygrams can cause problems and are inefficient.  So,
	 * send something that if it fragments, will produce reasonably
	 * sized fragments.  Avoid excessive fragmentation to reduce
	 * probability datagram fails to reassemble.
	 */
	if (tp->t_maxfrag) 
	{
	    len = tp->t_maxfrag*3;
	    if ((tp->snd_lst - tp->snd_nxt) > len) 
	    {
		tp->snd_lst = tp->snd_nxt + len;
		sendalot = TRUE;
	    }
	}

	if (SEQ_GT(tp->snd_end, tp->snd_una) && 
	    SEQ_LEQ(tp->snd_end, tp->snd_lst)) 
	    /*
	     * There is data to send, and it should be PUSHed.
	     * PUSHed segments avoid the SWS algorithm since it
	     * might delay transmission.  PUSHed data MUST go
	     * out ASAP.  Note:  To avoid performance degradation,
	     * bulk data transfers should not have PUSH on.
	     */
	    snd_flags |= T_PUSH;
	else if (tp->snd_wnd > 0) 
	{
	    /*
	     * Avoid the silly window syndrome (sending small
	     * packets).  Making sure the usable window is at
	     * least some % of the offered window ensures we're
	     * sending a relatively (for this connection) good
	     * sized segment.
	     *
	     * If sbspace(sosnd) == 0, then the user
	     * is blocked for send resources, and we won't be
	     * able to send a larger packet later, so send it now.
	     * (Hmm, still true?  How about the wakeup after we
	     * trim the acked data?)
	     *
	     *      SWS and persistence interaction
	     * If there is outstanding data, snd_nxt - snd_una
	     * will be > 0, we'll have retransmit timers running
	     * forcing eventual window updates.  If there is
	     * no outstanding data, then we'll send some and
	     * start up the retransmit timers.  So, any time
	     * we run through this segment of code instead of
	     * the next one, we've got some good timers running.
	     */
	    if (!tp->rexmt && !tp->ack_due && !tp->snd_fin &&
		!sendalot &&
		sbspace(sosnd) > 0 &&
		((100*(tp->snd_nxt-tp->snd_una))/tp->snd_wnd)
		> tp->sws_qff)
		tp->snd_lst = tp->snd_nxt;
	}
	else 
	{
	    /*
	     * We have stuff to send, but can't since the other
	     * end can't handle it right now.  We start up the
	     * persistence timer in case their window opening
	     * ack is lost.  When the timer goes off, we send
	     * a byte to force a window update.  Wait for timer
	     * in order to give him a chance to deal with the
	     * remotely buffered data and send us an update.
	     * (We'll get here on acks that stop rxmit timers
	     * but that contain zero window since remote user
	     * has not picked up data yet.)
	     *
	     * If we're called due to a write() or packet arrival,
	     * this is how we enter the persistence state.  If
	     * we're called because the persist timer went off,
	     * the timer is restarted to keep persisting.
	     */
	    if (tp->t_timers[TPERSIST] == 0)
		tp->t_timers[TPERSIST] = MIN(TCP_tvMAXPERSIST,
					     MAX(TCP_tvMINPERSIST, tp->t_srtt*3));

	    if (tp->force_one) 
	    {
		/* persist timer went off */
		tp->snd_lst = tp->snd_nxt + 1;
		forced = TRUE;
	    }
	}

	/* must send FIN and no more data left to send after this */

	if (tp->snd_fin && !forced && SEQ_EQ(tp->snd_lst, last) &&
	    (SEQ_EQ(tp->seq_fin, tp->iss) || 
	    SEQ_LEQ(tp->snd_nxt, tp->seq_fin))) 
	{
	    snd_flags |= T_FIN;
	    tp->seq_fin = tp->snd_lst++;
	}
    }

    /*
     * Now, we have determined how large a segment to send if our only
     * purpose is to get data to the other side.  If there is something
     * to send, do it and update timers for rexmt.
     */
    len = tp->snd_lst - tp->snd_nxt;
    if (len > 0) 
    {	/* then SEQ_LT(tp->snd_nxt, tp->snd_lst) */

	error = send_pkt (tp, snd_flags, len);

	/*
	 * SEQ_LEQ(snd_nxt, t_xmt_val): if this is a retransmission
	 * of the round-trip-time measuring byte, then remeasure the
	 * round trip time.  (Keep rtt from drifting upward on lossy
	 * networks.)
	 *
	 * SEQ_GT(snd_una, t_xmt_val):  Measure the rtt if the last
	 * timed byte has been acked.
	 */
	if (tp->syn_acked && (SEQ_LEQ(tp->snd_nxt, tp->t_xmt_val) ||
	    SEQ_GT(tp->snd_una, tp->t_xmt_val))) 
	{
	    if (tp->t_srtt != 0)
		tp->t_timers[TXMT] = 0;
	    tp->t_xmt_val = tp->snd_nxt;
	}

	/*
	 * If the window was full, and we're just forcing a byte
	 * out to try to get a new window, then don't use
	 * retransmission timeouts.  The other side can take as
	 * long as it wants to process the data it's currently got.
	 */
	if (! forced)
	{
	    /*
	     * Set timers for retransmission.  If we already have
	     * some outstanding data, then don't reset timer.  Think
	     * of case where send one byte every 1/4 second and only
	     * first byte is lost.  Would need to wait until filled
	     * window before retransmission timer started to decrease
	     * and go off.
	     */
	    if (tp->t_timers[TREXMT] == 0)
		tp->t_timers[TREXMT] = tp->t_rxmitime;

	    if (tp->t_timers[TREXMTTL] == 0)
		tp->t_timers[TREXMTTL] = tp->t_rttltimeo;

	    /*
	     * and remember that next segment out begins
	     * further into the stream if this one got out.
	     */
	    if (! error)
		tp->snd_nxt = tp->snd_lst;
	}

#if T_DELACK > 0
	t_cancel(tp, TDELACK);
	tp->force_ack = FALSE;
	tp->ack_skipped = 0;
#endif
	tp->ack_due = FALSE;
	tp->snd_hi = SEQ_MAX(tp->snd_lst, tp->snd_hi);
	if (!error)
	    return(TRUE);
    }

    /*
     * If ctl, make sure to send something so ACK gets through.  Attempt
     * to reduce ACK traffic by delaying ACKs with no data slightly.
     * Naive ack traffic can account for about 10% of what the receiving
     * tcp is doing.
     *
     * Bidirectional connection (telnet) => ack piggy backs application's
     * response.
     *
     * Unidirectional connection (ftp) => advertise large enough window
     * so that either #skipped (tp->ack_skipped) or our estimate of what he
     * thinks window is cause ack.  The estimate assumes most packets get
     * through.  This also assumes that the sender buffers enough to take
     * advantage of the estimated usable window, so we'll assume a minimum
     * send buffer provided by his operating system.  (Remember, his OS has
     * to buffer it until we ack it.)
     *
     * So, test assumes his send buffer > MINTCPBUF bytes large
     * and his silly window algorithm cuts in at < 50% of window.
     *
     * Use of the fasttimeout facility is a possibility.
     */
    if (ctl == TCP_CTL) 
    {
#if T_DELACK > 0
	if (tp->force_ack ||
	    (tp->ack_skipped >= max_ack_skipped) ||
	    ((tp->rcv_nxt - tp->lastack) > MIN(MINTCPBUF, tp->rcv_wnd>>1)))
	{
	    (void) send_pkt(tp, 0, 0);
	    t_cancel(tp, TDELACK);
	    tp->force_ack = FALSE;
	    tp->ack_skipped = 0;
	    tp->ack_due = FALSE;
	    tcpstat.t_ackonly ++;
	}
	else 
	{
	    tp->ack_skipped ++;
	    if (tp->t_timers[TDELACK] == 0)
		tp->t_timers[TDELACK] = T_DELACK;
	}
#else
	(void) send_pkt(tp, 0, 0);
	tp->ack_due = FALSE;
	tcpstat.t_ackonly ++;
#endif
    }
    return(FALSE);
}

/*
 * Process incoming ACKs.  Remove data from send queue up to acknowledgement.
 * Also handles round-trip timer for retransmissions and acknowledgement of
 * SYN, and clears the urgent flag if required.
 */

#ifdef BBNPING
#define BBNPING_RESET(inp, len) \
	if (len > 0){ \
		/* \
		 * We've sent him NEW data, perhaps by a gateway, that he \
		 * has successfully received.  If that's the case, then \
		 * we know the route works and we don't have to ping that \
		 * gateway. \
		 * \
		 * see check_ping() \
		 */ \
		register struct rtentry *rt; \
 \
		if (rt = inp->inp_route.ro_rt) \
			if (rt->rt_flags & RTF_GATEWAY) \
				rt->irt_pings = (-1); \
	}
#else
#define BBNPING_RESET(x,y) /* */
#endif

#ifdef MBUF_DEBUG
#define LENCHECK \
	if ((len > sosnd->sb_cc) || (len < 0)){			\
		printf("len %d sb_cc %d flags 0x%x state %d\n",	\
		   len, sosnd->sb_cc, n->t_flags, tp->t_state);	\
		if (len < 0)					\
			len = 0;				\
		else						\
			len = sosnd->sb_cc;			\
	}
#else
#define LENCHECK /* */
#endif

#define smooth(tp) (((75*(tp)->t_timers[TXMT]) + (125*(tp)->t_srtt)) / 200)

#define RCV_ACK(tp, n) \
{ \
	register struct inpcb *inp; \
	register struct sockbuf *sosnd; \
	register len; \
 \
	inp	= tp->t_in_pcb; \
	sosnd	= &inp->inp_socket->so_snd; \
	len	= n->t_ackno - tp->snd_una;  \
 \
	tp->snd_una = n->t_ackno; \
	if (SEQ_GT(tp->snd_una, tp->snd_nxt))  \
		tp->snd_nxt = tp->snd_una; \
 \
	/* \
	 * if urgent data has been acked, reset urgent flag \
	 */ \
 \
	if (tp->snd_urg && SEQ_GEQ(tp->snd_una, tp->snd_urp)) \
		tp->snd_urg = FALSE; \
 \
	if (tp->syn_acked) { \
		/* if timed message has been acknowledged, use the time to set \
		   the retransmission time value, exponential decay, 60/40 \
		   weighted average */ \
 \
		if (SEQ_GEQ(tp->snd_una, tp->t_xmt_val)) {			\
			if (tp->t_srtt == 0)					\
				tp->t_srtt = tp->t_timers[TXMT];		\
			else							\
				tp->t_srtt = smooth(tp);			\
			tp->t_rxmitime = MIN(TCP_tvRXMAX,			\
					   MAX(TCP_tvRXMIN, (3*tp->t_srtt)/2)); \
		} \
	} else { \
		/* handle ack of opening syn (tell user) */ \
 \
		if (SEQ_GT(tp->snd_una, tp->iss)) { \
			tp->syn_acked = TRUE; \
			len--;			/* ignore SYN */ \
			t_cancel(tp, TINIT);	/* cancel init timer */ \
		} \
	} \
 \
	/* remove acknowledged data from send buff */ \
	if (ack_fin(tp, n)) \
		len --; \
	LENCHECK \
	sbdrop (sosnd, len); \
	BBNPING_RESET(inp, len) \
	sbwakeup (sosnd);	/* wakeup iff > x% of buffering avail? */ \
 \
	/* handle ack of closing fin */ \
 \
	if (SEQ_NEQ(tp->seq_fin, tp->iss) && SEQ_GT(tp->snd_una, tp->seq_fin)) \
		tp->snd_fin = FALSE; \
	t_cancel(tp, TREXMT);          /* cancel retransmit timer */ \
	t_cancel(tp, TREXMTTL);        /* cancel retransmit too long timer */ \
	tp->cancelled = TRUE; \
}


/*
 * Process incoming segments 
 */
rcv_tcp(tp, n, ctl)                 
register struct tcpcb *tp;
register struct th *n;
int ctl;
{
    int sentsomedata;

    tp->dropped_txt = FALSE;
    tp->ack_due = FALSE;
    tp->new_window = FALSE;
    /*
     * Process SYN
     */
    if (!tp->syn_rcvd && n->t_flags&T_SYN) 
    {
	tp->snd_wl = tp->rcv_urp = tp->irs = n->t_seq;
	tp->rcv_urpend	= tp->rcv_urp -1;
	tp->rcv_nxt	= n->t_seq + 1;
	tp->syn_rcvd	= TRUE;
	tp->ack_due	= TRUE;
    }

    if (tp->syn_rcvd)
    {
	/*
	 * Process ACK if data not already acked previously. (Take 
	 * ACKed data off send queue, and reset rexmt timers).
	 */
	if (n->t_flags&T_ACK && SEQ_GT(n->t_ackno, tp->snd_una))
	    RCV_ACK(tp, n)

	/*
	 * Check for new window.  rcv_ack did not change syn_rcvd.
	 */
	if (SEQ_GEQ(n->t_seq, tp->snd_wl)) 
	{
	    tp->snd_wl = n->t_seq;
	    tp->snd_wnd = n->t_win;
	    tp->new_window = TRUE;
	    t_cancel(tp, TPERSIST); /* cancel persist timer */
	}
    }

    /*
     * For data packets only (vs. ctl), process data and URG.
     */
    if (ctl == TCP_DATA) 
    {
	/*
	 * Remember how much urgent data for present_data
	 */
	if (n->t_flags & T_URG)
	{
	    /*
	     * if last <= urpend, then is a retransmission
	     * bytes [n->t_seq ... last] are urgent
	     */
	    register sequence last;

	    last = n->t_seq + n->t_urp;
	    if (SEQ_GT(last, tp->rcv_urpend))
	    {
		/*
		 * Can only remember one contiguous region.
		 */
		if (SEQ_GT(n->t_seq, tp->rcv_urpend+1))
		{
		    struct socket *so;

		    tp->rcv_urp = n->t_seq;
		    if (tp->oob_data)
		    {
			m_freem(tp->oob_data);
			tp->oob_data = NULL;
		    }

		    so = tp->t_in_pcb->inp_socket;
		    so->so_oobmark = so->so_rcv.sb_cc +
			(tp->rcv_urp-tp->rcv_nxt);
		    if (so->so_oobmark == 0)
			so->so_state |= SS_RCVATMARK;
		}
		tp->rcv_urpend = last;
	    }
	}

	if (n->t_len != 0)
	    rcv_text(tp, n);	/* accept and sequence data */

	/* 
	 * Delay extraction of out-of-band data until
	 * present_data() so don't have to worry about
	 * duplication...
	 */

#ifdef bsd41
	/*
	 * Process PUSH, mark end of data chain.
	 *
	 * Not done in 4.2.  TCP is a byte stream, without record
	 * boundries, so don't have to mark for sbappend(), which
	 * preserves marks, and soreceive(), which terminates reads
	 * at marks.  Data IS pushed nevertheless since soreceive
	 * gives the user all that is available and returns.
	 */
	if (n->t_flags&T_PUSH && !tp->dropped_txt &&
	    tp->t_rcv_prev != (struct th *)tp) 
	{

	    /* Find last mbuf on received data chain and mark */

	    m = dtom(tp->t_rcv_prev);
	    if (m != NULL) 
	    {
		while (m->m_next != NULL)
		    m = m->m_next;
		m->m_act = (struct mbuf *) 1;
	    }
	}
#endif
    }
    /*
     * Process FIN, check for duplicates and make sure all data is in.
     */
    if (n->t_flags&T_FIN && !tp->dropped_txt) 
    {
	if (tp->fin_rcvd) 
	    tp->ack_due = TRUE;
	else
	{
	    /*
	     * Check if we really have FIN 
	     * (rcv buf filled in, no drops) 
	     */
	    register sequence last;

	    last = firstempty(tp);
	    if ((tp->t_rcv_prev == (struct th *)tp &&
		SEQ_EQ(last, t_end(n)+1)) || 
		SEQ_EQ(last, t_end(tp->t_rcv_prev)+1)) 
	    {
		tp->fin_rcvd = TRUE;
		uwake(tp->t_in_pcb);
	    }
	    /* 
	     * If FIN, then set to ACK: incr rcv_nxt, since FIN 
	     * occupies sequence space 
	     */
	    if (tp->fin_rcvd && SEQ_GEQ(tp->rcv_nxt, last)) 
	    {
		tp->rcv_nxt = last + 1;
		tp->ack_due = TRUE;
	    }
	}
    }
    /*
     * If ACK required or rcv window has changed, try to send something.
     */
    sentsomedata = FALSE;
    if (tp->ack_due)                
	sentsomedata = send_tcp(tp, TCP_CTL);
    else if (tp->new_window)       
	sentsomedata = send_tcp(tp, TCP_DATA);
    /*      
     * tp->cancelled => retransmit, rttl timers are now zero
     *
     * If didn't send any data, might not have retransmit, rttl timers
     * running.  If we still have unACKed data and we turned off
     * the timers above, then ensure timers are running.
     */
    if (!sentsomedata && is_unacked(tp) && tp->cancelled) 
    {
	tp->t_timers[TREXMT] = tp->t_rxmitime;
	tp->t_timers[TREXMTTL] = tp->t_rttltimeo;
	tp->cancelled = FALSE;
    }
}

#undef BBNPING_RESET
#undef LENCHECK

/*
 * Process incoming data.  Put the segments on sequencing queue in order,
 * taking care of overlaps and duplicates.  Data is removed from sequence
 * queue by present_data when sequence is complete (no holes at top).
 * Drop data that falls outside buffer quota if tight for space.  Otherwise,
 * process and recycle data held in tcp_input.
 */
rcv_text(tp, t)                 
register struct tcpcb *tp;
register struct th *t;
{
    register i;
    register struct sockbuf *sorcv;
    register struct mbuf *m;
    register struct th *q;
    struct th *p;
    struct mbuf *n;
    struct th *savq;
    int j, oldkeep;
    sequence last;

    /* throw away any data we have already received */

    if ((i = tp->rcv_nxt - t->t_seq) > 0)  
    {
	if (i < t->t_len) 
	{
	    t->t_seq += i;
	    t->t_len -= i;
	    m_adj(dtom(t), i);
	}
	else 
	{
	    tp->t_olddata++;
	    tp->ack_due = TRUE;	/* send ack just in case */
#ifdef HMPTRAPS
	    /* hmp_trap(T_TCP_DUP, (caddr_t)0,0); */
#endif
	    return;
	}
    }

    last = t_end(t); /* last seq # in incoming seg */

    /* # buffers available to con */

    sorcv = &tp->t_in_pcb->inp_socket->so_rcv;
    i = sbspace(sorcv);
    if (i < 0)
	i = 0;

    /* enough resources to process segment? used to walk mbuf chain to
     * count up data bytes. let's be smart and use t_len */

    j = t->t_len;
    if (j > i) 
    {

	/* if segment preceeds top of sequencing queue, try to take
	   buffers from bottom of queue */

	q = tp->t_rcv_next;
	if (q != (struct th *)tp && SEQ_LT(tp->rcv_nxt, q->t_seq) &&
	    SEQ_LT(t->t_seq, q->t_seq)) 

	    for (p = tp->t_rcv_prev; i < j && p != (struct th *)tp;) 
	    {
		savq = p->t_prev;
		TCP_DEQ(p, tp);
#ifdef HMPTRAPS
		/* hmp_trap(T_TCP_UDROP, (caddr_t)0,0); */
#endif
		for (m = dtom(p); m != NULL; m = m_free(m))
		    i += m->m_len;
		p = savq;
	    }

	/* if still not enough room, drop text from end of new segment */

	if (j > i) 
	{

	    for (m = dtom(t); i > 0 && m != NULL; m = m->m_next)
		i -= m->m_len;

	    while (m != NULL) 
	    {
		t->t_len -= m->m_len;
		last -= m->m_len;
		m->m_len = 0;
		m = m->m_next;
	    }
	    tp->dropped_txt = TRUE;
#ifdef HMPTRAPS
	    /* hmp_trap(T_TCP_RDROP, (caddr_t)0,0); */
#endif
	    if (SEQ_LT(last, t->t_seq))
		return;
	}
    }

    /* merge incoming data into the sequence queue */

    q = tp->t_rcv_next;	/* -> top of sequencing queue */

    /* skip frags which new doesn't overlap at end */

    while ((q != (struct th *)tp) && SEQ_GT(t->t_seq, t_end(q)))
	q = q->t_next;

    if (q == (struct th *)tp) 
    {		/* frag at end of chain */

	if (SEQ_GEQ(last, tp->rcv_nxt)) 
	{
	    tcp_net_keep = TRUE;
	    TCP_ENQ(t, tp->t_rcv_prev, tp);
	}

    }
    else 
    {

#ifdef HMPTRAPS
	/* we've received an out-of-order packet: trap! */

	/* hmp_trap(T_TCP_ORDER, (caddr_t)0,0); */

#endif
	/* frag doesn't overlap any on chain */

	if (SEQ_LT(last, q->t_seq)) 
	{
	    tcp_net_keep = TRUE;
	    TCP_ENQ(t, q->t_prev, tp);

	    /* new overlaps beginning of next frag only */

	}
	else if (SEQ_LT(last, t_end(q))) 
	{
	    if ((i = last - q->t_seq + 1) < t->t_len) 
	    {
		t->t_len -= i;
		m_adj(dtom(t), -i);
		tcp_net_keep = TRUE;
		TCP_ENQ(t, q->t_prev, tp);
	    }

	    /* new overlaps end of previous frag */

	}
	else 
	{
	    savq = q;
	    if (SEQ_LEQ(t->t_seq, q->t_seq)) 
	    {  /* complete cover */
		savq = q->t_prev;
		TCP_DEQ(q, tp);
		m_freem(dtom(q));

	    }
	    else 
	    {						/* overlap */
		if ((i = t_end(q) - t->t_seq + 1) < t->t_len) 
		{
		    t->t_seq += i;
		    t->t_len -= i;
		    m_adj(dtom(t), i);
		}
		else
		    t->t_len = 0;
	    }

	    /* new overlaps at beginning of successor frags */

	    q = savq->t_next;
	    while ((q != (struct th *)tp) && (t->t_len != 0) && 
		SEQ_LEQ(q->t_seq, last))

		/* complete cover */

		if (SEQ_LEQ(t_end(q), last)) 
		{
		    p = q->t_next;
		    TCP_DEQ(q, tp);
		    m_freem(dtom(q));
		    q = p;
		}
		else 
		{		/* overlap */
		    if ((i = last-q->t_seq+1) < t->t_len) 
		    {
			t->t_len -= i;
			m_adj(dtom(t), -i);
		    }
		    else
			t->t_len = 0;
		    break;
		}

	    /* enqueue whatever is left of new before successors */

	    if (t->t_len != 0) 
	    {
		tcp_net_keep = TRUE;
		TCP_ENQ(t, savq, tp);
	    }
	}
    }

    /* set to ack completed data (no gaps) */

    FIRSTEMPTY(tp, tp->rcv_nxt);
    tp->ack_due = TRUE;

    /* if any room remaining in rcv buf, take any unprocessed
       messages and schedule for later processing */

    if ((m = tp->t_rcv_unack) != NULL && (i = sbspace(sorcv)) > 0)
    do
    {

	/* schedule work request */

	t = mtod(m, struct th *);
	j = (t->t_off << TCP_OFFSHIFT) + sizeof(struct ip);
	m->m_off += j;
	m->m_len -= j;
	tp->t_rcv_unack = m->m_act;
	m->m_act = (struct mbuf *)0;
	oldkeep = tcp_net_keep;
	tcpstat.t_unack++;
	w_alloc(INRECV, 0, tp, t);
	tcp_net_keep = oldkeep;

	/* remaining buffer space */

	for (n = m; n != NULL; n = n->m_next)
	    i -= n->m_len;
    }
    while ((m = tp->t_rcv_unack) != NULL && i > 0);
}

/*
 * Send a reset segment
 */
send_rst(tp, n)                         
register struct tcpcb *tp;
register struct th *n;
{
    register struct inpcb *inp;
    struct in_addr src, dst;
    u_short port;
    int temp_rst;

    /* don't send a reset in response to a reset */

    if (n->t_flags&T_RST || (inp = tp->t_in_pcb) == NULL)
	return;

    tp->snd_rst = TRUE;
    temp_rst = FALSE;
    if (n->t_flags&T_ACK)
	tp->snd_nxt = n->t_ackno;

    /* if reset required from "wildcard" listener, take addresses and
       port from incoming packet */

    if (inp->inp_laddr.s_addr == 0 || inp->inp_faddr.s_addr == 0 || 
	inp->inp_fport == 0) 
    {
	src = inp->inp_laddr;
	dst = inp->inp_faddr;
	port = inp->inp_fport;
	inp->inp_laddr = n->t_d;
	inp->inp_faddr = n->t_s;
	inp->inp_fport = n->t_src;
	tp->t_template = tcp_template(tp);
	temp_rst = TRUE;
    }
    tp->syn_rcvd = FALSE;
    if (tp->t_template)
	(void) send_pkt(tp, 0, 0);
    else
	printf("send_rst: no template\n");
    tp->ack_due = FALSE;
    tp->snd_rst = FALSE;
#if T_DELACK > 0
    tp->force_ack = FALSE;
    t_cancel(tp, TDELACK);
    tp->ack_skipped = 0;
#endif

    /* restore "wildcard" addresses */

    if (temp_rst) 
    {
	inp->inp_laddr = src;
	inp->inp_faddr = dst;
	inp->inp_fport = port;
	tp->snd_nxt = tp->iss;
	if (inp->inp_route.ro_rt != NULL) 
	{
	    rtfree(inp->inp_route.ro_rt);
	    inp->inp_route.ro_rt = NULL;
	}
	if (tp->t_template)
	{
	    m_free(dtom(tp->t_template));
	    tp->t_template = NULL;
	}
    }
}

struct mbuf *extract_oob(tp, mp, sorcv)
struct tcpcb	*tp;
struct mbuf	*mp;
struct sockbuf	*sorcv;
{
    struct socket	*so;
    struct mbuf	*top, *here, *m;
    int off, len, tmp;

    m = mp;
    so = tp->t_in_pcb->inp_socket;
    /*
     * skip over bytes that preceed out of band data.
     */
    if ((off = so->so_oobmark - sorcv->sb_cc) < 0)
    {
	log(LOG_INFO, "extract_oob:  neg off\n");
	tp->rcv_urpend = tp->rcv_urp = tp->irs;
	return (mp);
    }

    while (m && (off > 0))
    {
	if (m->m_len <= off)
	{
	    off -= m->m_len;
	    m = m->m_next;
	}
	else
	    break;
    }

    if (!m)
	return (mp);

    /*
     * copy out of band data.  removing it from input stream.
     */
    len = tp->rcv_urpend - tp->rcv_urp + 1; /* # urgent bytes */
    top = here = NULL;
    while (m && (len > 0))
    {
	char	*p;
	struct mbuf *newm;
	int dropped;

	tmp = MIN(m->m_len - off, len);
	/* tmp == # urgent bytes in this mbuf */
	len -= tmp;
	tp->rcv_urp += tmp;

	p = mtod(m, caddr_t) + off; /* points at first urgent byte */
	dropped = FALSE;

	while (tmp > 0)
	{
	    unsigned nbytes;

	    /* in case this mbuf uses pages */
	    nbytes = MIN(tmp, MLEN);

	    if (! dropped)
	    {
		if (newm = m_get(M_WAIT, MT_DATA))
		{
		    bcopy (p, mtod(newm, char *), nbytes);
		    newm->m_len = nbytes;

		    if (!top)
			top = here = newm;
		    else
		    {
			here->m_next = newm;
			here = here->m_next;
		    }
		}
		else
		    /* potential unreliability */
		    dropped = TRUE;
	    }

	    bcopy(p+nbytes, p,  (unsigned)(m->m_len -off -nbytes));
	    m->m_len -= nbytes;
	    tmp -= nbytes;
	}

	if (m->m_len <= 0)
	{
	    /*
	     * So soreceive never sees a zero length mbuf
	     * with m_act set.  (PUSHED URGENT data packet)
	     */
	    if (m == mp)
		mp = m = m_free(m);
	    else
		m = m_free(m);
	}
	else
	    m = m->m_next;

	off = 0;
    }

    if (top)
    {
	if (tp->oob_data)
	    m_cat (tp->oob_data, top);
	else
	    tp->oob_data = top;
	sohasoutofband(so);
    }

    return (mp);
}

/*
 * Accept data for the user to receive.  Moves data from sequenced tcp
 * segments from the sequencing queue to the user's receive queue (in the
 * ucb).  Observes locking on receive queue.
 */
present_data(tp)                
register struct tcpcb *tp;
{
    PRESENT_DATA(tp)
}
