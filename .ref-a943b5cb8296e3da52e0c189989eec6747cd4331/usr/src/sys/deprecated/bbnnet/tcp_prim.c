#ifdef	RCSIDENT
static char rcsident[] = "$Header: tcp_prim.c,v 1.23 85/07/31 09:34:04 walsh Exp $";
#endif

#include "../h/param.h"
#include "../h/errno.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/protosw.h"

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
#include "../bbnnet/sws.h"

extern struct inpcb tcp;

/*
 * TCP finite state machine primitives
 *
 * These routines are called from the procedures in tcp_procs.c to do low
 * level protocol functions.
 */

/*
 * The hope is that a bcopy will fill in most tcp/ip header fields quicker
 * than a step by step stuffing of each individually when we have to send
 * some info.
 */
struct th *tcp_template(tp)
struct tcpcb	*tp;
{
    register struct mbuf	*m;
    register struct th	*t;
    register struct inpcb	*inp;

    m = m_getclr(M_WAIT, MT_HEADER);
    if (m == NULL)
	return ((struct th *) NULL);
    m->m_len = sizeof (struct th);
    t = mtod(m, struct th *);
    inp = tp->t_in_pcb;

    t->t_pr = IPPROTO_TCP;
    t->t_s = inp->inp_laddr;
    t->t_d = inp->inp_faddr;
    t->t_src = inp->inp_lport;
    t->t_dst = inp->inp_fport;
    t->t_off = TCPSIZE >> TCP_OFFSHIFT;

    return (t);
}

#ifdef GPROF
long tcp_pkt_size[2*1024];
#endif

/*
 * Send a tcp segment
 */
send_pkt(tp, flags, len)
register struct tcpcb *tp;
register int flags;
register int len;	/* in sequence units: includes SYN, FIN */
{
    register struct th *t;
    register struct inpcb *inp;
    register struct mbuf *m;
    struct mbuf *dat;
    int i;
    struct sockbuf *sorcv;
    short *p;
    struct th *tmpt;

    inp = tp->t_in_pcb;

    /*
     * Adjust data length for SYN and FIN.
     */
    if (flags & T_FIN)
	len--;
    if (flags & T_SYN)
	len--;

    /*
     * and get a copy of the data for this transmission
     */
    dat = (struct mbuf *) NULL;
    if (len > 0) 
    {
	int	off;

	off = tp->snd_nxt - tp->snd_una;
	if (! tp->syn_acked)
	    if (! (flags & T_SYN))
		off --;
	if ((dat = m_copy (inp->inp_socket->so_snd.sb_mb, off, len)) == NULL)
	    return (ENOBUFS);
    }

#ifdef MBUF_DEBUG
    m = m_get(M_WAIT, MT_HEADER);
#else
    MGET(m, M_WAIT, MT_HEADER);
#endif
    if (m == NULL)
	return(ENOBUFS);
    /*
     * Build tcp leader at bottom of new buffer to leave room for lower
     * level leaders.  Leave an extra four bytes for TCP max segment size
     * option, which is sent in SYN packets.
     * Align header for memory access speed in checksumming...
     */
    m->m_off = (MMAXOFF - sizeof(struct th) - TCP_MAXSEG_OPTLEN) &
	~(sizeof(long) -1);
    m->m_len = sizeof(struct th);
    m->m_next = dat;
    t = mtod(m, struct th *);

    if (tp->t_template == NULL)
	panic("send_pkt");
    bcopy((caddr_t)tp->t_template, (caddr_t)t, sizeof (struct th));

#ifndef NOTCPOPTS
    /*
     * Insert maximum segment size option for SYN.
     */
    if (flags & T_SYN) 
    {
	/*
	 * may not have a route yet.
	 */
	if (inp->inp_route.ro_rt) 
	{
	    m->m_len += TCP_MAXSEG_OPTLEN;
	    /* SYN occupies seq space */
	    len += TCP_MAXSEG_OPTLEN;
	    t->t_off = (TCPSIZE + TCP_MAXSEG_OPTLEN) >> TCP_OFFSHIFT;
	    p = (short *)((int)t + sizeof(struct th));
	    *p++ = htons((u_short)TCP_MAXSEG_OPTHDR);
	    *p = htons((u_short)inp->inp_route.ro_rt->rt_ifp->if_mtu
		- TCPIPMAX);
	}
    }
#endif

    t->t_len   = htons((u_short)len + TCPSIZE);
    t->t_seq   = htonl(tp->snd_nxt);
    t->t_ackno = htonl(tp->rcv_nxt);

#ifdef GPROF
    if (len < (sizeof(tcp_pkt_size)/sizeof(tcp_pkt_size[0])) - TCPSIZE)
	tcp_pkt_size[len+TCPSIZE] ++;
    else
	tcp_pkt_size[0] ++;
#endif

    if (tp->snd_rst) 
    {
	flags |= T_RST;
	flags &= ~T_SYN;
    }

    if (tp->snd_urg)
    {
	if (SEQ_GT(tp->snd_urp, tp->snd_nxt)) 
	{
	    short	up;

	    /*
	     * SEQ_LEQ(tp->snd_urp, tp->snd_nxt+len)
	     * Strictly speaking, we should be able to have the
	     * urgent pointer point into the stream beyond the
	     * current end of packet, but this is just in case
	     * some implementation is not ready for that.
	     */
	    flags |= T_URG;
	    up = MIN(len -1, tp->snd_urp - tp->snd_nxt -1);
	    t->t_urp = htons((u_short)up);
	}
    }

    if (tp->syn_rcvd) 
    {
#if T_DELACK > 0
	tp->lastack = tp->rcv_nxt;
#endif
	flags |= T_ACK;
    }

    t->t_flags = flags;

    /* Calculate the window we should advertise */

    sorcv = &inp->inp_socket->so_rcv;
    /*
     * Count bytes left in user rcv queue, and reduce by sequencing queue
     * Counting the sequencing q contracts the window when packets are
     * lost (== when the network is over-loaded).
     */
    i = sbspace(sorcv) - tp->t_rcv_len;
    /* 
     * apply receiver's solution to SWS in case sender does not have such
     * an algorithm.  One 8th was determined by benchmarks writing 2k
     * buffers on an Ethernet connection.  It has a slightly negative
     * influence on IMP networks when writing 1k buffers.
     *
     * (sorcv->sb_hiwat >> 3) limited by 256 == 2k / 8, since application
     * may adjust its buffering to large amounts via ioctl call.  An eighth
     * of a large number may be a reasonable sized packet to send.
     *
     * Only apply this algorithm if are getting packets in order,
     * so that advertisement of 0 window does not prevent retransmission
     * of dropped packet.
     */
    tmpt = tp->t_rcv_next;
    if ((i < MIN(256, (sorcv->sb_hiwat >> 3))) &&
	((tmpt == (struct th *) tp) || SEQ_LEQ(tmpt->t_seq, tp->rcv_nxt)))
	i = 0;
    else
    {
	/*
	 * if this connection gets fragmented, constrain the windowsize
	 */
	if (tp->t_maxfrag)
	    i = MIN(i, tp->t_maxfrag*3);

	if (i < 0)
	    i = 0;
    }

#if T_DELACK > 0
    tp->rcv_wnd = i;
#endif
    t->t_win = htons((u_short)i);
    /*
     * If we sent a zero window, we should try to send a non-zero ACK ASAP.
     */
    if (i == 0)
	tp->sent_zero = TRUE;
    else
	tp->sent_zero = FALSE;

    t->t_sum = in_cksum(m, len + sizeof(struct th));

    if (inp->inp_socket->so_options & SO_DEBUG) 
    {
	struct work w;

	w.w_type = INRECV;	/* not really. use -1 newstate */
	w.w_tcb  = tp;
	w.w_dat  = (char *)t;
	tcp_debug(tp, &w, -1);
    }

    /*
     * and ship packet off via IP.  Remember that since this protocol
     * involves retransmissions, errors can occur asynchronous to a
     * (write) system call, and that therefore we can not send the
     * error all the way back up through subroutine return values.  We
     * must also post it back via advise_user() at some point, and this
     * looks like a good point to try it.
     */
    {
	int	error;

	error = ip_send(inp, m, len+TCPSIZE, FALSE);
	if (error)
	    /*
	     * Since we use retransmissions, don't need to tell user
	     * process about this.  (Can be as simple as interface
	     * or host structure queues are too long due to current
	     * heavy traffic.  Backing off will take care of that.)
	     */
	    if (error != ENOBUFS)
		advise_user(inp->inp_socket, error);
	return (error);
    }
}

/*
 * Find the first empty spot in rcv buffer
 */
sequence firstempty(tp)
register struct tcpcb *tp;
{
    sequence	retval;

    FIRSTEMPTY(tp, retval);
    return(retval);
}


/*
 * TCP timer update routine
 */
tcp_timeo()
{
    register struct inpcb *inp, *next;
    register struct tcpcb *tp;
    register i;
    register s;
    extern sequence tcp_iss;	/* tcp initial send seq # */
    static int tcpmins;	/* tcp minute timer */

    /* search through tcb and update active timers */
    s = splnet();
    inp = tcp.inp_next;
    while (inp != &tcp)
    {
	next = inp->inp_next;
	if (tp = inptotcpcb(inp))
	{
	    if (tp->sws_qff < SWS_QFF_DEF)
		tp->sws_qff ++;

	    for (i = TINIT; i <= TDELACK; i++)
		if (tp->t_timers[i] != 0 && --tp->t_timers[i] == 0)
		{
		    struct work w;

		    w.w_type = ISTIMER;
		    w.w_stype = i;
		    w.w_tcb = tp;
		    w.w_dat = (char *) NULL;
		    if (action(&w) == CLOSED)
			goto next_tcb;
		}

	    if (tp->t_timers[TXMT] < MAX_TCPTIMERVAL-1)
		tp->t_timers[TXMT]++;

	    if (tcpmins == 0)
	    {
		if (tp->t_timers[TNOACT] != 0 && --tp->t_timers[TNOACT] == 0)
		    w_alloc(ISTIMER, TNOACT, tp, 0);
	    }
	}
next_tcb:
	inp = next;
    }
    splx(s);

    if (--tcpmins < 0)
	tcpmins = 120-1;	/* zero-origin strikes again */
    tcp_iss += ISSINCR;	/* increment iss */
}


/*
 * Do TCP option processing
 */
tcp_opt(tp, t, hlen)
register struct tcpcb *tp;
register struct th *t;
int hlen;
{
    register char *p;
    register i, j, len;

    p = (char *)((int)t + sizeof(struct th));	/* -> at options */

    if ((i = hlen - TCPSIZE) > 0) 
    {			/* any options */

	while (i > 0)

	    switch (*p++) 
	{
	  case TCP_END_OPT:
	  default:	/* garbage: throw away rest */
	    return;

	  case TCP_NOP_OPT:
	    i--;
	    break;

	  case TCP_MAXSEG_OPT:	/* max segment size */
	    if (t->t_flags&T_SYN && !tp->syn_rcvd) 
	    {
		len = ntohs(*(u_short *)((int)p + 1));
		/*
		 * may not have a route yet
		 */
		if (!tp->t_in_pcb->inp_route.ro_rt)
		    /* in LISTEN state */
		    tp->t_maxseg = len;
		else
		    /* connecting to server */
		    tp->t_maxseg =
		    MIN(tp->t_in_pcb->inp_route.ro_rt->rt_ifp->if_mtu -
		    TCPIPMAX, len);
		tp->t_maxseg -= tp->t_in_pcb->inp_optlen;
	    }
	    if ((j = *p) == 0)
		break;
	    i -= j;
	    p += j - 1;
	}
    }
}

/*
 * Called at splimp from uipc_mbuf.c
 * Network needs some space freed!  Remove unprocessed packets.
 */
tcp_drain()
{
    register struct inpcb *inp;
    register struct tcpcb *tp;
    register struct mbuf *m;

    for (inp = tcp.inp_next; inp != &tcp; inp = inp->inp_next)
    {
	tp = (struct tcpcb *)inp->inp_ppcb;

	if (tp == NULL)
	    continue;

	while (m = tp->t_rcv_unack)
	{
	    tp->t_rcv_unack = m->m_act;
	    m->m_act = (struct mbuf *)NULL;
	    m_freem (m);
	}
    }
}
