#ifdef RCSIDENT
static char rcsident[] = "$Header: tcp_states.c,v 1.21 85/07/31 09:42:53 walsh Exp $";
#endif


#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/errno.h"
#include "../h/systm.h"

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
#ifdef HMPTRAPS
#include "../bbnnet/hmp_traps.h"
#endif

extern struct rtentry *ip_route();

/*
 * These are the action routines of the TCP finite state machine.  They are
 * called from the TCP fsm dispatcher action().  These routines call 
 * on the routines in tcp_procs.c to do the actual segment processing.
 */

/*
 * UNOPENED x IUOPENA == passive open == listen()
 */
lis_cls(wp)
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;

    /* listen() system call */

    /*
     * Don't know who we're talking to yet, so we don't have a route
     * or mtu yet.
     */
    tp->t_maxseg = TCPMAXSND;
    tp->t_timers[TINIT] = tp->t_itimeo;

    return(LISTEN);
}

/*
 * UNOPENED x IUOPENR == active open == connect()
 */
sys_cls(wp)
register struct work *wp;
{
    register struct tcpcb *tp;
    register struct inpcb *inp;

    /* connect() system call */

    tp = wp->w_tcb;
    inp = tp->t_in_pcb;
    /*
     * Know foreign host and have a route to there.
     */
#ifdef NOTCPOPTS
    tp->t_maxseg =  inp->inp_route.ro_rt->rt_ifp->if_mtu - TCPIPMAX;
#else
    /*
     * Best can do until other guy tells us otherwise.
     */
    tp->t_maxseg =
	MIN(inp->inp_route.ro_rt->rt_ifp->if_mtu - TCPIPMAX, TCPMAXSND);
#endif
    tp->t_maxseg -= inp->inp_optlen;

    tp->t_timers[TINIT] = (tp->t_itimeo ? tp->t_itimeo : TCP_tvINIT);

    send_tcp(tp, TCP_CTL);	/* send SYN */
    return(SYN_SENT);
}

/*
 * UNOPENED x IUCLOSE
 * LISTEN   x IUCLOSE
 * SYN_SENT x IUCLOSE
 *
 * User close request before receiving foreign SYN
 */
cls_opn(wp)
struct work *wp;
{
    t_close(wp->w_tcb, ECONNABORTED);
    return(CLOSED);
}

/*
 * SYN_RCVD   x IUCLOSE
 * L_SYN_RCVD x IUCLOSE
 * ESTAB      x IUCLOSE
 * 
 * close request on synched connection
 */
fw1_syr(wp)
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;

    tp->snd_fin = TRUE; /* send FIN */
    send_tcp(tp, TCP_CTL);
    tp->usr_closed = TRUE;
    tp->t_noact = TCP_tvNOACT;
    tp->t_timers[TNOACT] = TCP_tvNOACT;
    return(FIN_W1);
}

/*
 * CLOSE_WAIT x IUCLOSE
 *
 * close request after received foreign FIN
 */
cl2_clw(wp)
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;

    tp->snd_fin = TRUE; /* send our own FIN */
    send_tcp(tp, TCP_CTL);
    tp->usr_closed = TRUE;
    tp->t_noact = TCP_tvNOACT;
    tp->t_timers[TNOACT] = TCP_tvNOACT;
    return(CLOSING2);
}

/*
 * UNOPENED   x IUABORT
 * LISTEN     x IUABORT
 * SYN_SENT   x IUABORT
 * SYN_RCVD   x IUABORT
 * L_SYN_RCVD x IUABORT
 *
 * User abort request on unsynched connection
 */
cls_nsy(wp)
struct work *wp;
{
    t_close(wp->w_tcb, ECONNABORTED);
    return(CLOSED);
}

/*
 * ESTAB      x IUABORT
 * FIN_WAIT_1 x IUABORT
 * FIN_WAIT_2 x IUABORT
 * TIME_WAIT  x IUABORT
 * CLOSE_WAIT x IUABORT
 * CLOSING_1  x IUABORT
 * CLOSING_2  x IUABORT
 * RCV_WAIT   x IUABORT
 *
 * User abort request on synched connection
 */
cls_syn(wp)
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;

    tp->snd_rst = TRUE; /* send reset */
    (void) send_pkt(tp, 0, 0);
    /* tp->ack_due = FALSE; don't since about to throw tcpcb away */
    t_close(tp, ECONNABORTED);
    return(CLOSED);
}

/*
 * LISTEN x INRECV
 *
 * From tcp_input/netprepr, we know the packet is a well formed SYN
 */
lis_netr(wp)
struct work *wp;
{
    register struct tcpcb *tp;
    register struct th *n;
    register struct inpcb *inp;
    register struct socket *so, *newso;
    struct rtentry *rt;
    struct tcpcb *newtp;
    struct inpcb *newinp;

    struct in_addr firsthop;
    extern int	ip_nhops;
    extern struct in_addr ip_hops[];

    n = (struct th *)wp->w_dat;

    /*
     * Need to route on basis of IP destination -- see ip_send()
     * ### What if loose routing and 1st hop not on local net and reroute?
     */
    if (ip_nhops == 0)
	firsthop = n->t_s;
    else
	/* source routed SYN packet */
	firsthop = ip_hops[ip_nhops];

    /*
     * O.k., let's get a route back to him
     */
    if (!(rt = ip_route(&n->t_d, &firsthop))) 
    {
	/*
	 * Can't talk to him.  Leave socket in receive state
	 * so we can connect to someone else, since we haven't
	 * been committed to anything yet anyway.
	 * ### Drop his info on the floor.
	 * Let the other machine just figure out on it's own that
	 * it can't reach us that way.
	 */
	no_route ("tcp", n->t_d, firsthop);
	return(LISTEN);
    }

    tp = wp->w_tcb;
    inp = tp->t_in_pcb;
    so = inp->inp_socket;

    /*
     * This socket is in the listen state, so the socket should have
     * so_options & SO_ACCEPTCONN set (solisten()).
     *
     * The order of sonewconn() and soisconnected() is
     * important, in order for the process to be woken up
     * at a time when the sleep condition is fulfilled.
     * sonewconn() is done here on the original socket, and
     * soisconnected() is done later in syr_netr() on the new
     * socket.
     */
    if (newso = sonewconn(so))
    {
	newinp = (struct inpcb *) newso->so_pcb;
	newtp = (struct tcpcb *) newinp->inp_ppcb;
	/*
	 * Remember our peer for this connection.
	 */
	newinp->inp_faddr = n->t_s;
	newinp->inp_fport = n->t_src;
	newinp->inp_laddr = n->t_d;
	if (ip_nhops > 0)
	{
	    /*
	     * optlen includes the source route to be copied
	     * to the outgoing IP header, not the firsthop
	     * which replaces ip_dst.
	     */
	    bcopy((caddr_t)ip_hops,newinp->inp_options, (unsigned)(ip_nhops+1)*4);
	    newinp->inp_optlen = ip_nhops * 4;
	}
	/*
	 * and copy fields into the new inpcb
	 */
	newinp->inp_lport = inp->inp_lport;
	newinp->inp_route.ro_rt = rt;

	/*
	 * and copy fields to the new tcpcb
	 */
	newtp->t_maxfrag = tp->t_maxfrag;	/* set in tcp_input() */
	newtp->t_itimeo  = tp->t_itimeo;
	newtp->t_noact	 = tp->t_noact;
	newtp->t_push	 = tp->t_push;
	newtp->t_noactsig = tp->t_noactsig;
	newtp->t_noactprobe = tp->t_noactprobe;

	/*
	 * and initialize others with new info
	 * Upward negotiation of t_maxseg in tcp_opt() done
	 * on socket in LISTEN.
	 */
	newtp->t_maxseg = MIN(rt->rt_ifp->if_mtu - TCPIPMAX, tp->t_maxseg);
	newtp->t_maxseg -= newinp->inp_optlen;
	/*
	 * In case next client doesn't negotiate maxseg.
	 */
	tp->t_maxseg = TCPMAXSND;


	if (!(newtp->t_template = tcp_template(newtp)))
	{
	    soabort (newso);
	    return (LISTEN);
	}

	newtp->sws_qff = SWS_QFF_DEF;

	/*
	 * So can debug connection problems without having to change
	 * every program or apply debugging flag to each program every
	 * time run it.
	 */
	dowedebug(newinp, newso, &tcp_dfilter);

	/*
	 * rcv_tcp may set fin_rcvd.  If so, We went up and down or
	 * we got a garbage/misrouted packet.  If it's set, it's
	 * meant for some other socket or some other instantiation
	 * of it.  In any case, ignore it and listen for other
	 * talkers.
	 */
	rcv_tcp(newtp, n, TCP_DATA);

	if (newtp->fin_rcvd)
	    soabort (newso);
	else
	{
	    /*
	     * no FIN (4)
	     * start init timer now that we have foreign host.
	     * Parent socket might have init timer as zero to
	     * avoid getting ETIMEDOUT, but we do want this
	     * child socket to time out on synchronization
	     * just in case other host just went down.
	     */
	    newtp->t_timers[TINIT] = (newtp->t_itimeo != 0
		? newtp->t_itimeo
		: TCP_tvINIT/2);
	    newtp->t_state = L_SYN_RCVD;
	}
    }
    else
	rtfree(rt);

    return(LISTEN);	/* original file descriptor stays in LISTEN state */
}

/*
 * SYN_SENT x INRECV
 *
 * from tcp_input/netprepr, we know its a SYN, with perhaps a well formed ACK
 */
sys_netr(wp)
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;
    register struct th *n = (struct th *)wp->w_dat;

    rcv_tcp(tp, n, TCP_DATA);
    if (tp->fin_rcvd) 
    {             /* got a FIN */

	/* if good ACK, present any data */

	if (n->t_flags&T_ACK) 
	{
	    if (SEQ_GT(n->t_ackno, tp->iss))	/* 32 */
		present_data(tp);
	}
	else 
	{                                /* 9 */
	    tp->t_timers[TFINACK] = TCP_tv2ML;
	    tp->waited_2_ml = FALSE;
	}
	tp->t_timers[TNOACT] = tp->t_noact;
	return (CLOSE_WAIT);
    }
    else                   /* no FIN */
	/* if good ACK, open connection, otherwise wait for one */
	if (n->t_flags&T_ACK) 
	{			/* 11 */
	    present_data(tp);
	    tp->t_timers[TNOACT] = tp->t_noact;
	    soisconnected (tp->t_in_pcb->inp_socket);
	    return(ESTAB);
	}

    return(SYN_RCVD); /* 8 */
}

/*
 * SYN_RCVD   x INRECV
 * L_SYN_RCVD x INRECV
 *
 * from tcp_input/netprepr, we know its an ACK of our SYN
 */
syr_netr(wp)
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;
    register struct th *n = (struct th *)wp->w_dat;

    rcv_tcp(tp, n, TCP_DATA);
    present_data(tp);

    /* if no FIN, open connection, otherwise wait for user close */

    tp->t_timers[TNOACT] = tp->t_noact;
    if (tp->fin_rcvd)                               /* 33 */
	return(CLOSE_WAIT);
    else
    {
	/* 5 */
	soisconnected (tp->t_in_pcb->inp_socket);
	return(ESTAB);
    }
}

/*
 * ESTAB x INRECV
 */
est_netr(wp)
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;

    rcv_tcp(tp, (struct th *)wp->w_dat, TCP_DATA);
    PRESENT_DATA(tp);

    /* if no FIN, remain open, otherwise wait for user close */

    if (tp->fin_rcvd)                       /* 12 */
	return(CLOSE_WAIT);
    else /* 39 */
	return(SAME);
}

/*
 * FIN_WAIT_1 x INRECV
 *
 * incoming segment after user has closed
 */
fw1_netr(wp)
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;
    register struct th *n = (struct th *)wp->w_dat;

    /* process any incoming data, since we closed but they didn't */

    rcv_tcp(tp, n, TCP_DATA);
    present_data(tp);

    /* send any data remaining on send buffer */

    send_tcp(tp, TCP_DATA);
    if (ack_fin(tp, n)) 
    {			/* our FIN got ACKed */
	if (tp->fin_rcvd) 
	{                     /* got for FIN (28) */
	    tp->t_timers[TFINACK] = TCP_tv2ML;
	    tp->waited_2_ml = FALSE;
	    return(TIME_WAIT);
	}
	else                   /* no FIN, wait (27) */
	    return(FIN_W2);
    }
    else 
    {				/* no ACK of FIN */
	if (tp->fin_rcvd) 
	{                     /* got for FIN (26) */
	    tp->t_timers[TFINACK] = TCP_tv2ML;
	    tp->waited_2_ml = FALSE;
	    return(CLOSING1);
	}
    }
    return(SAME); /* 39 */
}

/*
 * FIN_WAIT_2 x INRECV
 *
 * incoming segment while waiting for foreign FIN
 */
fw2_netr(wp)
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;
    register struct th *n = (struct th *)wp->w_dat;

    /* process data since we closed, but they may not have */

    rcv_tcp(tp, n, TCP_DATA);
    present_data(tp);

    /* if we get the FIN, start the finack timer, else keep waiting */

    if (tp->fin_rcvd) 
    {                     /* got for FIN (29) */
	tp->t_timers[TFINACK] = TCP_tv2ML;
	tp->waited_2_ml = FALSE;
	return(TIME_WAIT);
    }
    else                   /* 39 */
	return(SAME);
}

/*
 * TIME_WAIT x INRECV
 *
 * The close protocol (exchange of FINs) has progressed as far as it can.
 * We do not enter CLOSED immediately, but use TIME_WAIT so that if our ack
 * of other guys FIN didn't reach him, he can retransmit and we'll ack his
 * fin rather than respond with an rst.
 *
 * Since we received a packet, apparently our ack of his fin didn't get
 * there and we'll have to try again.  Restart finack timer in case this
 * one fails too.
 */
sss_syn(wp)
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;

    rcv_tcp(tp, (struct th *) wp->w_dat, TCP_DATA);
    present_data(tp);
    tp->t_timers[TFINACK] = TCP_tv2ML;
    return(SAME);
}

/*
 * CLOSE_WAIT x INRECV
 *
 * incoming segment after receipt of foreign FIN (local end still open)
 */
cwt_netr(wp)
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;
    register struct th *n = (struct th *)wp->w_dat;

    /* either duplicate FIN or data */

    if (n->t_flags&T_FIN) 
    {
	if (n->t_flags&T_ACK && SEQ_LEQ(n->t_ackno, tp->seq_fin)) 
	{
	    rcv_tcp(tp, n, TCP_CTL);
	    tp->t_timers[TFINACK] = TCP_tv2ML;
	    tp->waited_2_ml = FALSE;
	}
	else                   /* 31 */
	    send_tcp(tp, TCP_CTL);
    }
    else 
    {				/* duplicate data (39) */
	rcv_tcp(tp, n, TCP_DATA);
	present_data(tp);
    }
    return(SAME);
}

/*
 * CLOSING_1 x INRECV
 *
 * incoming segment after we closed
 */
cl1_netr(wp)
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;
    register struct th *n = (struct th *)wp->w_dat;

    if (ack_fin(tp, n)) 
    {			/* got ACK of our FIN */
	if (n->t_flags&T_FIN) 
	{		/* got for FIN (23) */
	    rcv_tcp(tp, n, TCP_CTL);
	    tp->t_timers[TFINACK] = TCP_tv2ML;
	    tp->waited_2_ml = FALSE;
	    return(TIME_WAIT);
	}
	else 
	{

	    /* if wait done, see if any data left for user */

	    if (tp->waited_2_ml)
		if (rcv_empty(tp)) 
		{    /* 15 */
		    t_close(tp, ECONNABORTED);
		    return(CLOSED);
		}
		else
		    return(RCV_WAIT); /* 18 */
	    else
		return(TIME_WAIT); /* 22 */
	}
    }
    else 
    {				/* our FIN not ACKed yet */
	if (n->t_flags&T_FIN) 
	{		/* rcvd for FIN (30) */
	    rcv_tcp(tp, n, TCP_CTL);
	    tp->t_timers[TFINACK] = TCP_tv2ML;
	    tp->waited_2_ml = FALSE;
	}
	else 
	{		/* no FIN, just proc new data (39) */
	    rcv_tcp(tp, n, TCP_DATA);
	    present_data(tp);
	}
    }
    return(SAME);
}

/*
 * CLOSING_2 x INRECV
 *
 * incoming segment after both of us have started closing
 */
cl2_netr(wp)
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;
    register struct th *n = (struct th *)wp->w_dat;

    if (ack_fin(tp, n)) 
    {                   /* this is ACK of our fin */

	/* if no data left for user, close; otherwise wait */

	if (rcv_empty(tp)) 
	{                            /* 16 */
	    t_close(tp, ECONNABORTED);
	    return(CLOSED);
	}
	else                   /* 19 */
	    return(RCV_WAIT);
    }
    else 
    {				/* no ACK of our FIN */
	/* duplicate FIN or data */

	if (n->t_flags&T_FIN)				/* 31 */
	    send_tcp(tp, TCP_CTL);	/* ACK duplicate FIN */
	else
	{
	    /* 39 */
	    rcv_tcp(tp, n, TCP_DATA);
	    present_data(tp);
	}
    }
    return(SAME);
}

/*
 * RCV_WAIT x INRECV
 */
rwt_netr(wp)            /* incoming seg while waiting for user rcv (30,21) */
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;
    register struct th *n = (struct th *)wp->w_dat;

    /* handle duplicate ACK of our FIN */

    if (n->t_flags&T_FIN && n->t_flags&T_ACK && SEQ_LEQ(n->t_ackno, tp->seq_fin)) 
    { 		/* 30 */
	rcv_tcp(tp, n, TCP_CTL);
	tp->t_timers[TFINACK] = TCP_tv2ML;
	tp->waited_2_ml = FALSE;
    }
    return(SAME);
}

/*
 *	ESTAB      x IURECV
 *	CLOSE_WAIT x IURECV
 *
 * and allowing for shutdown()
 *
 *	FIN_WAIT_1 x IURECV
 *	FIN_WAIT_2 x IURECV
 *	TIME_WAIT  x IURECV
 *	CLOSING_1  x IURECV
 *	CLOSING_2  x IURECV
 */
sss_rcv(wp)             /* rcv request on open connection (42) */
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;

    PRESENT_DATA(tp);

    /* if last window sent was zero, send an ACK to update window */

    if (tp->sent_zero) 
    {
	tp->force_ack = TRUE;	/* don't delay ACK here */
	send_tcp(tp, TCP_CTL);
    }
    return(SAME);
}

/*
 * RCV_WAIT x IURECV
 */
cls_rwt(wp)             /* rcv request after foreign close (20) */
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;

    present_data(tp); /* present any remaining data */
    if (rcv_empty(tp)) 
    {
	t_close(tp, ECONNABORTED);
	return(CLOSED);
    }
    else
	return(RCV_WAIT);
}

/*
 * SYN_SENT   x IUSEND
 * SYN_RCVD   x IUSEND
 * ESTAB      x IUSEND
 * CLOSE_WAIT x IUSEND
 *
 * For SYN_SENT and SYN_RCVD, just want to buffer data until connected.
 */
sss_snd(wp)             /* send request on open connection (40,41) */
struct work *wp;
{
    register struct tcpcb *tcp = wp->w_tcb;
    register struct inpcb *inp = tcp->t_in_pcb;
    sequence last;

    sbappend(&inp->inp_socket->so_snd, (struct mbuf *) wp->w_dat);
    last = tcp->snd_una + inp->inp_socket->so_snd.sb_cc;

    if (tcp->t_push)
	tcp->snd_end = last;
    if (tcp->t_urg) 
    {
	tcp->snd_urp = last;	/* this byte is not urgent */
	tcp->snd_urg = TRUE;
    }
    send_tcp(tcp, TCP_DATA);
    return(SAME);
}

cls_act(wp)             /* net closing open connection (47) */
struct work *wp;
{
    t_close(wp->w_tcb, ECONNABORTED);
    return(CLOSED);
}

cls_err(wp)             /* invalid user request in closing states */
struct work *wp;
{
    advise_user(tcpcbtoso(wp->w_tcb), ECONNABORTED);
    return(SAME);
}



timers(wp)              /* timer processor (14,17,34,35,36,37,38) */
struct work *wp;
{
    register struct tcpcb *tp = wp->w_tcb;
    register type = wp->w_stype;

    switch (type) 
    {

      case TINIT: /* initialization timer */
	/*
	 * Haven't got an ACK of our SYN yet
	 */
	if (tp->t_in_pcb->inp_socket->so_state & SS_NOFDREF)
	{
	    /*
	     * was a child socket of a listen(2)er trying to
	     * establish connection with other end.
	     * (state L_SYN_RCVD)
	     */
	    t_close(tp, ETIMEDOUT);
	    return(CLOSED);
	}
	/* socket in connect(2) */
	advise_user(tcpcbtoso(tp), ETIMEDOUT);
	tp->t_timers[TINIT] = tp->t_itimeo;
	break;

      case TFINACK: /* fin-ack timer */   

	if (tp->t_state == TIME_WAIT) 
	{

	    /* can be sure our ACK of for FIN was rcvd,
	       can close if no data left for user */

	    if (rcv_empty(tp)) 
	    {            /* 14 */
		t_close(tp, ECONNABORTED);
		return(CLOSED);
	    }
	    else                   /* 17 */
		return(RCV_WAIT);

	}
	else if (tp->t_state == CLOSING1)     /* 37 */

	    /* safe to close */

	    tp->waited_2_ml = TRUE;

	break;

      case TREXMT: /* retransmission timer */

	if (is_unacked(tp)) 
	{
	    /* statistics */
	    tp->t_rxtct++;
	    tcpstat.t_retransmit ++;

	    /*
	     * If we're retransmitting, then the network
	     * may be dropping packets because it is overloaded.
	     * Therefore, increase the retransmission time for
	     * successive retransmissions.  When we get an ACK,
	     * the srtt and rxmitime will be recalculated.
	     */
	    tp->t_rxmitime = tp->t_rxmitime << 1;
	    if (tp->t_rxmitime > TCP_tvRXMAX)
		tp->t_rxmitime = TCP_tvRXMAX;

	    tp->snd_nxt = tp->snd_una;
	    tp->rexmt = TRUE;
	    send_tcp(tp, TCP_DATA);
	    tp->rexmt = FALSE;
	}
	break;

      case TREXMTTL: /* retransmit too long */

#ifdef HMPTRAPS
	/* hmp_trap(T_TCP_REXMTTL, (caddr_t)0, 0); */
#endif
	if (tp->usr_abort) 
	{
	    /* user has already closed for r/w so abort connection
	     * usr_closed == closed for w (close or shutdown).
	     */
	    t_close(tp, ETIMEDOUT);
	    return(CLOSED);
	}
	advise_user(tcpcbtoso(tp), ETIMEDOUT);
	tp->t_timers[TREXMTTL] = tp->t_rttltimeo;
	break;

      case TPERSIST: /* persist timer */

	/* force a byte send through closed window */

	tp->force_one = TRUE; /* 38 */
	send_tcp(tp, TCP_DATA);	/* restarts timer */
	tp->force_one = FALSE;
	break;

      case TDELACK:	/* ack-delay timer */

	/* make sure an ack gets sent now */

	tp->force_ack = TRUE;
	send_tcp(tp, TCP_CTL);
	break;

      case TNOACT:	/* no activity timer */
	/*
	 * This timer is used for 2 reasons:
	 * 1) by the user to determine if the connection is idle or if the
	 *    other side has aborted/rebooted...  This is open states entry.
	 *    See tcp_newtcpcb()
	 * 2) by the system to timeout on receipt of ACK of our FIN.
	 *    This is separate from use of FINACK timer for other guy
	 *    to get our ACK of his FIN.  If closing has started, finish it.
	 */

	/*
	 * if its a shutdown(),
	 *	usr_closed == TRUE, usr_abort == FALSE
	 *	the user will find out about any problems getting an ACK of our
	 *	    FIN through the retransmit took too long timer
	 *	the connection could be idle because it takes the remote end a
	 *	    while to compute and produce a reply
	 *	user only gets to crank up protocol close once, but he can
	 *	    shutdown and then close, thereby adjusting usr_abort so
	 *	    that things get cleaned up if the remote host died.
	 *
	 * if its a close(),
	 *	usr_closed == TRUE, usr_abort == TRUE
	 *	user could be lingering (and SS_NOFDREF will still be false)
	 *	connection could be idle because the other host failed, and it
	 *	    could be down for days.  We don't want to wait for it to
	 *	    come back up and give us a reset.  Release resources now.
	 */
	if (tp->usr_abort) 
	{
	    t_close(tp, ETIMEDOUT);
	    return(CLOSED);
	}

	if (tp->t_noactprobe)
	    send_tcp(tp, TCP_CTL);

	if (tp->t_noactsig)
	    advise_user(tcpcbtoso(tp), ETIMEDOUT);

	tp->t_timers[TNOACT] = tp->t_noact;
	break;

    }
    return(SAME);
}
