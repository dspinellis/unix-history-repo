/*
 $Log:	rdp_states.c,v $
 * Revision 2.14  85/06/18  14:38:50  walsh
 * eliminated inpcb flags.
 * 
 * Revision 2.13  85/05/31  14:39:06  walsh
 * copy sequential delivery desires when fork off a new socket.
 * 
 * Revision 2.12  84/12/03  09:42:20  walsh
 * Keep no route messages from flooding console.
 * 
 * Revision 2.11  84/11/29  13:06:17  walsh
 * Have the NULL message retransmission back off so that don't load
 * a jammed network and so that don't wake user up so often when
 * some intermediary hop has gone down on a (normally) fast connection.
 * 
 * Revision 2.10  84/11/15  09:56:14  walsh
 * redid how we deal with compiler padding in the RDP header structure.
 * 
 * Revision 2.9  84/11/08  16:12:06  walsh
 * Added code to gather statistics on RDP traffic.  This makes the RDPCB
 * too big unles you make mbufs 512 bytes large.  RDP_CS should be turned off
 * unless you do.
 * 
 * Revision 2.8  84/11/06  15:24:54  walsh
 * *** empty log message ***
 * 
 * Revision 2.7  84/11/05  15:54:34  walsh
 * update_nulltimer() macro began to look inappropriate with recent
 * changes, so its been stripped out and put in-line.
 * 
 * Revision 2.6  84/11/05  12:42:34  walsh
 * Set things up so can debug RDP connections just like can debug TCP
 * connections.
 * 
 * Revision 2.5  84/11/05  11:34:36  walsh
 * Don't let round trip time estimate drift upward on lossy networks.
 * Check for retransmissions of packets used to measure round trip time.
 * 
 * Revision 2.4  84/11/05  10:47:38  walsh
 * More changes to go with NULL messages getting their own sequence
 * number.
 * 
 * Revision 2.3  84/11/02  18:24:20  walsh
 * Protocol specifiers want NULL message to have own sequence number in
 * case of slow (t>NULL msg timeout) packets.  I don't see this as a problem,
 * and even if happened (dubious) would only delay discovery, but I
 * didn't win this one.  Initially not designed for this, but fixes are
 * in almost neatly.
 * 
 * Revision 2.2  84/11/02  15:29:32  walsh
 * Allow for RDP header fields not on natural boundries.  (Protocol
 * specifiers say will be part of next version in 6-12 months).
 * Until then, there goes the speed...  Yucho modifications.
 * 
 * Revision 2.1  84/11/02  10:14:11  walsh
 * Fixed to include RCS comments in checked out source.
 * 
 *
 * description:
 * The state transition functions for the Reliable Datagram Protocol.
 * 
 * revision 1.17        
 * date: 84/07/24 16:58:17;  author: walsh;  state: Exp;  lines added/del: 2/2
 * When had gone to making retransmit took too long advisory,
 * had forgotten to change RDP_sCLOSED to RDP_sSAME.
 * 
 * revision 1.16        
 * date: 84/07/23 12:58:31;  author: walsh;  state: Exp;  lines added/del: 27/6
 * Clear all timers when enter close state.  Updates to protocol had not
 * been complete in this respect.
 * 
 * Retransmission and acceptance in CLOSEWAIT do not seem to be in the cards
 * in dealing with protocol specifiers, so removed ### markers and commented.
 * 
 * revision 1.15        
 * date: 84/07/22 19:45:31;  author: walsh;  state: Exp;  lines added/del: 19/0
 * Added a state transition function rdp_closew_rcv() to compensate for
 * socket code's dropping of system priority level for a brief period of time.
 * 
 * revision 1.14        
 * date: 84/07/19 10:21:42;  author: walsh;  state: Exp;  lines added/del: 14/85
 * Organized macros and classified their definitions in rdp_macros.h.
 * 
 * revision 1.13        
 * date: 84/07/19 08:54:01;  author: walsh;  state: Exp;  lines added/del: 4/0
 * NULL message processing should start before receive a packet in ESTAB,
 * so start up NULL timer when enter ESTAB.
 * 
 * revision 1.12        
 * date: 84/07/18 18:50:55;  author: walsh;  state: Exp;  lines added/del: 36/5
 * Added provision for sending of NULL messages.  These are sent on an idle
 * connection to determine that the other side still exists.
 * 
 * revision 1.11        
 * date: 84/07/18 13:35:36;  author: walsh;  state: Exp;  lines added/del: 6/6
 * made provisions for user-adjustable RTTL time period.
 * 
 * revision 1.10        
 * date: 84/07/13 09:50:33;  author: walsh;  state: Exp;  lines added/del: 22/19
 * When first send datagram, we determine its length.
 * Might as wellsave that length in m_act for retransmission.
 * 
 * revision 1.9        
 * date: 84/07/12 13:48:22;  author: walsh;  state: Exp;  lines added/del: 1/0
 * Rather than in-line stuffing of IP/RDP headers, at least half of which are
 * constant, copy headers in from a template of what the headers are like.  The
 * bcopy() call is turned into a movc3 instruction on the VAX by a sed script
 * run over the assembler output of the C compiler.  Marginal speed-up.
 * 
 * revision 1.8        
 * date: 84/07/12 09:55:02;  author: walsh;  state: Exp;  lines added/del: 5/13
 * some small optimizations.
 * 
 * revision 1.7        
 * date: 84/07/10 14:48:13;  author: walsh;  state: Exp;  lines added/del: 1/1
 * Reduced amount of unnecessary wakeup action.
 * 
 * revision 1.6        
 * date: 84/07/10 10:28:33;  author: walsh;  state: Exp;  lines added/del: 35/35
 * Added register declarations.
 * 
 * revision 1.5        
 * date: 84/07/09 14:31:33;  author: walsh;  state: Exp;  lines added/del: 11/2
 * Added an ACK-delay algorithm to reduce cpu and network loading.
 * 
 * revision 1.4        
 * date: 84/07/08 21:36:47;  author: walsh;  state: Exp;  lines added/del: 3/3
 * changed some references to r_sendq.rq_baseseq to r_snduna for clarity.
 * 
 * revision 1.3        
 * date: 84/07/06 15:13:50;  author: wjacobso;  state: Exp;  lines added/del: 17/17
 * add register var definitions; use sndnxt-baseseq instead of maxqlen
 * to determine number of passes
 * 
 * revision 1.2        
 * date: 84/07/06 09:49:52;  author: root;  state: Exp;  lines added/del: 93/35
 * This version seems to run bug-free.
 * 
 * revision 1.1        
 * date: 84/06/26 14:18:09;  author: walsh;  state: Exp;  
 * Initial revision
 */


#ifdef RDP
#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/kernel.h"
#include "../h/inode.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/errno.h"
#include "../h/syslog.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../bbnnet/in.h"
#include "../bbnnet/net.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/rdp.h"
#include "../bbnnet/seq.h"
#include "../bbnnet/rdp_macros.h"

extern struct rtentry *ip_route();

/*
 * Since a message just got through, re-associating rttl (retransmit
 * took too long) with some other current outstanding datagram (cf. wait
 * until some new dgram) is a little paranoid, but let's be careful
 * in case that new dgram doesn't come along for a while.  This also
 * allows us to decide that the check-for-retransmit and
 * retransmit-took-too-long timers can be cancelled.
 */
clear_rxtimer(rdpcb, N)
register RDPCB	*rdpcb;
{
    int Xi;
    int pass;

    rdpcb->r_rxtimers[N] = 0;
    if (rdpcb->r_rttlindex == N) 
    {
	/*
	 * look for new dgram of which to check rttl
	 */
	Xi = rdpcb->r_sendq.rq_front;
	pass = rdpcb->r_sndnxt - rdpcb->r_snduna;
	while (--pass >= 0)
	{
	    if (rdpcb->r_rxtimers[Xi]) 
	    {
		rdpcb->r_rttlindex = Xi;
		rdpcb->r_timers[RDP_tRTTL] = rdpcb->r_rttl;
		return;
	    }
	    Xi = (Xi + 1) % rdpcb->r_sendq.rq_maxqlen;
	}
	/*
	 * No outstanding dgrams left.
	 */
	rdpcb->r_rttlindex = (-1);
	rdpcb->r_timers[RDP_tRTTL] = 0;
	rdpcb->r_timers[RDP_tRXMIT] = 0;
    }
}

/*
 * set up things to discover the rtt (round trip time) for this
 * DATA-containing packet.
 */
#define time_rtt(rdpcb, seqnum) \
	if (! (rdpcb)->r_rttiming){            \
		(rdpcb)->r_rttiming = TRUE;    \
		(rdpcb)->r_rtt = 0;            \
		(rdpcb)->r_rttimed = (seqnum); \
	}


/*
 * Since we play with sb_cc for the socket send buffer to prevent the
 * user process from sending packets we can't buffer, must ensure it
 * is restored to a reasonable value before call upon socket code to clean
 * up or we'll get a "panic: sbdrop".  Socket code is called by
 * in_pcbdetach().
 */
trash_pcbs(rdpcb)
RDPCB *rdpcb;
{
    register struct sockbuf *sosnd;

    sosnd = &rdpcb->r_inpcb->inp_socket->so_snd;
    if ((sosnd->sb_cc == sosnd->sb_hiwat) && (sosnd->sb_mb == NULL))
	sosnd->sb_cc = 0;
    in_pcbdetach (rdpcb->r_inpcb, rdp_pcbdisconnect);
}

cancel_timers(rdpcb)
register RDPCB *rdpcb;
{
    register int i;

    for (i=0; i<RDP_NTIMERS; i++)
	rdpcb->r_timers[i] = 0;
}

/************************************************************************/

/*
 *	state: RDP_sUNOPENED
 *	input: RDP_iCONNECT
 */
/*ARGSUSED*/
rdp_unop_connect (rdpcb, nil)
register RDPCB	*rdpcb;
{
    /*
     * Send a SYN
     * and set re-transmission timer to ensure SYN eventually gets there
     */
    (void) rdp_sendpkt (rdpcb, (MBUF *) NULL, 0, rdpcb->r_iss);
    set_rxtimer (rdpcb, 0);
    return (RDP_sSYNSENT);
}

/*
 *	state: RDP_sUNOPENED
 *	input: RDP_iLISTEN
 */
/*ARGSUSED*/
rdp_unop_listen (rdpcb, nil)
RDPCB	*rdpcb;
{
    return (RDP_sLISTEN);
}

/*
 *	state: RDP_sUNOPENED
 *	input: RDP_iNETR
 */
/*ARGSUSED*/
rdp_unop_netr (rdpcb, pkt)
RDPCB	*rdpcb;
register RDPHDR	*pkt;
{
    if (pkt->rh_flags & (RDP_fACK|RDP_fEACK|RDP_fNULL))
	/*
	 * We haven't sent anything to (e)ack.  Nor have we
	 * established a connection and received something
	 * that we should ack (null).  The sender is very mixed
	 * up, so we'll send him a reset.
	 */
	rdp_uncon_rst (pkt);
    else
	/*
	 * ignore packet in hope user connect(2)s or listen(2)s before
	 * it's re-transmission comes in.
	 */
	m_freem(dtom(pkt));

    return (RDP_sSAME);
}

/*
 *	state: RDP_sUNOPENED
 *	input: RDP_iUCLOSE
 */
/*ARGSUSED*/
rdp_unop_close (rdpcb, nil)
RDPCB	*rdpcb;
{
    trash_pcbs(rdpcb);
    return (RDP_sCLOSED);
}

/************************************************************************/

/*
 *	state: RDP_sLISTEN
 *	input: RDP_iLISTEN
 */
/*ARGSUSED*/
rdp_lis_listen (rdpcb, nil)
RDPCB	*rdpcb;
{
    return (RDP_sSAME);
}

/*
 *	state: RDP_sLISTEN
 *	input: RDP_iNETR
 */
rdp_lis_netr (rdpcb, pkt)
RDPCB	*rdpcb;
register RDPHDR		*pkt;
{
    INPCB	*inp;
    struct socket	*so;
    struct rtentry	*rt;
    register RDPCB		*newrdpcb;
    register INPCB		*newinp;
    struct socket	*newso;
    register struct ip	*ip;
    register SYNOPTIONS	*synopt;

    if (pkt->rh_flags & (RDP_fRST|RDP_fACK|RDP_fEACK|RDP_fNULL)) 
    {
	if (pkt->rh_flags & RDP_fRST)
	    /*
	     * Ignore resets since we haven't sent anything to
	     * reset.  The packet may be a slow arrival meant to
	     * close a child socket of ours that has already
	     * finished close protocol with this sender.  We
	     * ignore it and the other end closes/closed on its own.
	     */
	    m_freem(dtom(pkt));
	else
	    /*
	     * We haven't sent anything to (e)ack.  Nor have we
	     * established a connection and received something
	     * that we should ack (null).  The sender is very mixed
	     * up, so we'll send him a reset.
	     */
	    rdp_uncon_rst (pkt);

	return (RDP_sSAME);
    }

    if (pkt->rh_flags & RDP_fSYN)
    {
	/* normal case, someone is trying to connect to us. */

	ip = (struct ip *) (((char *) pkt) - sizeof(struct ip));

	/*
	 * O.k., let's get a route back to him
	 */
	if (!(rt = ip_route(&ip->ip_dst, &ip->ip_src))) 
	{
	    /*
	     * Can't talk to him.  Leave socket in receive state
	     * so we can connect to someone else, since we haven't
	     * been committed to anything yet anyway.
	     * Drop his info on the floor.
	     * Let the other machine figure out on it's own
	     * that it can't reach us that way.
	     */
	    no_route ("rdp", ip->ip_dst, ip->ip_src);
	    m_freem(dtom(pkt));
	    return(RDP_sSAME);
	}

	inp = rdpcb->r_inpcb;
	so = inp->inp_socket;

	/*
	 * This socket is in the listen state, so the socket should have
	 * so_options & SO_ACCEPTCONN set (solisten()).
	 *
	 * The order of sonewconn() and soisconnected() is
	 * important, in order for the process to be woken up
	 * at a time when the sleep condition is fulfilled.
	 * sonewconn() is done here on the original socket, and
	 * soisconnected() is done later in rdp_lsynrcvd_netr() on
	 * the new socket.
	 */
	if (newso = sonewconn(so))
	{
	    newinp = (INPCB *) newso->so_pcb;
	    newrdpcb = (RDPCB *) newinp->inp_ppcb;
	    /*
	     * Remember our peer for this connection.
	     */
	    newinp->inp_faddr = ip->ip_src;
	    newinp->inp_fport = pkt->rh_sport;
	    newinp->inp_laddr = ip->ip_dst;
	    /*
	     * and copy fields into the new inpcb
	     */
	    newinp->inp_lport = inp->inp_lport;
	    newinp->inp_route.ro_rt = rt;
	    /*
	     * and copy fields into the new rdpcb.  In particular,
	     * the user's desired buffering allocations should be
	     * propogated.
	     */
	    newrdpcb->r_ournbuf = rdpcb->r_ournbuf;
	    sbreserve (&newrdpcb->r_inpcb->inp_socket->so_rcv,
		rdpcb->r_inpcb->inp_socket->so_rcv.sb_hiwat);
	    pick_ourmaxlen(newrdpcb);
	    /*
	     * Sequential delivery is a combination of both side's
	     * desires, and must be copied from server socket since
	     * user does not have a handle on the child socket in
	     * it's early states.
	     */
	    newrdpcb->r_sequential = rdpcb->r_sequential;

	    /*
	     * and stuff new information
	     */
	    got_syn(newrdpcb, RDP_SEQNO(pkt));
	    synopt = RDP_OPT(pkt, SYNOPTIONS *);
	    process_synopt(newrdpcb, synopt);

	    /*
	     * So can debug connection problems without having to
	     * change every program or apply debugging flag to each
	     * program every time run it.
	     */
	    dowedebug(newinp, newso, &rdp_dfilter);

	    /*
	     * send other guy our SYN and ACK his syn.
	     * set re-transmission timer to ensure eventually gets
	     * to him.
	     */
	    rdp_template(newrdpcb);
	    (void) rdp_sendpkt (newrdpcb, (MBUF *) NULL, 0,
		newrdpcb->r_iss);
	    set_rxtimer (newrdpcb, 0);

	    newrdpcb->r_state = RDP_sLSYNRCVD;
	}
	else
	rtfree(rt);
    }
    m_freem(dtom(pkt));
    return (RDP_sSAME);
}

/*
 *	state: RDP_sLISTEN
 *	input: RDP_iUCLOSE
 */
/*ARGSUSED*/
rdp_lis_close (rdpcb, nil)
RDPCB	*rdpcb;
{
    trash_pcbs(rdpcb);
    return (RDP_sCLOSED);
}

/************************************************************************/

/*
 *	state: RDP_sSYNSENT
 *	input: RDP_iNETR
 */
rdp_synsent_netr (rdpcb, pkt)
register RDPCB	*rdpcb;
register RDPHDR	*pkt;
{
    register rdpstate newstate;

    if (pkt->rh_flags & RDP_fACK)
    {
	if (RDP_ACKNO(pkt) != rdpcb->r_iss)
	{
	    /*
	     * We haven't sent any data yet, only SYNs.
	     * He's confused.
	     */
	    rdp_uncon_rst (pkt);
	    return (RDP_sSAME);
	}
    }

    if (pkt->rh_flags & RDP_fRST)
    {
	/*
	 * Require (rst, ack, ackno) to know rst meant for this, not
	 * a previous, incarnation of the socket.  Is an "in window"
	 * check.  Avoids problems with "slow" packets.
	 */
	if (pkt->rh_flags & RDP_fACK)
	{
	    set_error (rdpcb, ECONNREFUSED);
	    trash_pcbs(rdpcb);
	    newstate = RDP_sCLOSED;
	}
	else
	    newstate = RDP_sSAME;
	m_freem(dtom(pkt));
	return (newstate);
    }

    newstate = RDP_sSAME;
    if (pkt->rh_flags & RDP_fSYN)
    {
	register SYNOPTIONS	*synopt;
	rdpsequence seqnum;

	got_syn(rdpcb, RDP_SEQNO(pkt));
	synopt = RDP_OPT(pkt, SYNOPTIONS *);
	process_synopt(rdpcb, synopt);

	if (pkt->rh_flags & RDP_fACK)
	{
	    rdpcb->r_synacked = TRUE;
	    rdpisconnected(rdpcb);
	    newstate = RDP_sESTAB;
	    seqnum = rdpcb->r_iss +1;
	    /* clear re-xmit syn timer set in rdp_unop_connect() */
	    clear_rxtimer (rdpcb, 0);
	    /* start up connection loss detection */
	    rdpcb->r_timers[RDP_tNULL] = rdpcb->r_tvnull;
	}
	else 
	{
	    newstate = RDP_sSYNRCVD;
	    seqnum = rdpcb->r_iss;
	    /* keep sending syn until he acks it */
	    set_rxtimer (rdpcb, 0);
	}
	/* and ack his syn, retransmit ours if necessary */
	(void) rdp_sendpkt (rdpcb, (MBUF *) NULL, 0, seqnum);
    }

    m_freem(dtom(pkt));
    return (newstate);
}

/*
 *	state: RDP_sSYNSENT
 *	input: RDP_iUCLOSE
 */
/*ARGSUSED*/
rdp_synsent_close (rdpcb, nil)
register RDPCB *rdpcb;
{
    /* send RST */
    rdpcb->r_sendrst = TRUE;
    (void) rdp_sendpkt (rdpcb, (MBUF *) NULL, 0, rdpcb->r_sndnxt);
    trash_pcbs(rdpcb);
    return (RDP_sCLOSED);
}


/*
 *	state: RDP_sSYNSENT
 *	input: RDP_iTIMER
 */
rdp_synsent_timer (rdpcb, timer)
register RDPCB *rdpcb;
{
    switch (timer)
    {
      case RDP_tRTTL:
	/* retransmission took too long */
	rttl(rdpcb);
	return (RDP_sSAME);

      case RDP_tRXMIT:
	/*
	 * re-transmit our SYN.  Not every 0.5 second, though,
	 * but every RDP_tvRXMIN units.
	 */
	rdpcb->r_rxtimers[0] --;
	if (rdpcb->r_rxtimers[0] == 0)
	{
	    (void) rdp_sendpkt (rdpcb, (MBUF *) NULL, 0,
		rdpcb->r_iss);
	    set_rxtimer (rdpcb, 0);
#ifdef RDP_CS
	    rdpcb->r_sent.r_retrans ++;
#endif
	}
	else
	    /*
	     * ensure keep checking even if no packet goes
	     * out this time.  ACK will stop this.
	     */
	    rdpcb->r_timers[RDP_tRXMIT] = RDP_tvRXCHECK;
	break;

      default:
	log(LOG_INFO, "rdp_synsent_timer:  timer %d\n", timer);
    }

    return(RDP_sSAME);
}

/************************************************************************/

/*
 *	state: RDP_sLSYNRCVD
 *	input: RDP_iNETR
 */
rdp_lsynrcvd_netr (rdpcb, pkt)
register RDPCB		*rdpcb;
register RDPHDR		*pkt;
{
    /*
     * If it's a duplicate syn (seqno == irs), re-send ack since he must
     * have missed our ack.  If it's out of the window, well, let's give
     * him the benefit of the doubt and assume it's junk from an old
     * connection/window that took a while to get to us.
     */
    if (SEQ_LEQ(RDP_SEQNO(pkt), rdpcb->r_irs) ||
	SEQ_GEQ(RDP_SEQNO(pkt), rdpcb->r_rcvq.rq_baseseq + rdpcb->r_rcvq.rq_maxqlen))
    {

#ifdef RDP_CS
	rdpcb->r_rcvd.r_retrans ++;
#endif
	/* try to synchronize again */
	(void) rdp_sendpkt (rdpcb, (MBUF *) NULL, 0, rdpcb->r_iss);
	m_freem(dtom(pkt));
	return(RDP_sSAME);
    }

    if (pkt->rh_flags & (RDP_fRST|RDP_fEACK|RDP_fSYN)) 
    {
	if (pkt->rh_flags & RDP_fRST)
	{
	    /*
	     * User closed while his socket was in synsent state.
	     */
	    set_error (rdpcb, ECONNREFUSED);
	    trash_pcbs(rdpcb);
	    m_freem(dtom(pkt));
	    return (RDP_sCLOSED);
	}
	if (pkt->rh_flags & RDP_fEACK)
	{
	    /*
	     * shouldn't be EACK, since we haven't sent anything yet
	     */
	    rdp_uncon_rst (pkt);	/* frees mbufs for pkt */
	    return(RDP_sSAME);
	}
	if (pkt->rh_flags & RDP_fSYN)
	{
	    /*
	     * Boy, is the other end confused!  His syn has changed
	     * sequence numbers.
	     */
	    rdp_uncon_rst (pkt);
	    set_error (rdpcb, ECONNRESET);
	    trash_pcbs(rdpcb);
	    return (RDP_sCLOSED);
	}
    }

    if (pkt->rh_flags & RDP_fACK)
    {
	if (RDP_ACKNO(pkt) != rdpcb->r_iss)
	{
	    rdp_uncon_rst (pkt);	/* frees mbufs for pkt */
	    return(RDP_sSAME);
	}
    }
    else 
    {
	m_freem(dtom(pkt));
	return(RDP_sSAME);
    }

    /*
     * clear timer for re-transmission of syn that we set in
     * rdp_lis_netr().
     */
    clear_rxtimer (rdpcb, 0);
    rdpcb->r_synacked = TRUE;


    if (pkt->rh_dlen > rdpcb->r_ourmaxlen)
    {
	log(LOG_INFO, "RDP too large packet %d > %d\n",
	    pkt->rh_dlen, rdpcb->r_ourmaxlen);
theygoofed :
	rdp_uncon_rst(pkt);
	rdpcb->r_timers[RDP_tCLOSEWAIT] = rdpcb->r_closewait;
	set_error(rdpcb, ECONNRESET);
	return (RDP_sCLOSEWAIT);
    }
    /*
     * zero length packets can be NULL messages or (E)ACKs,
     * but all NULL messages must be zero length
     */
    if (pkt->rh_flags & RDP_fNULL)
    {
	if (pkt->rh_dlen != 0)
	{
	    log(LOG_INFO, "RDP %d length NULL packet\n", pkt->rh_dlen);
	    goto theygoofed;
	}
	if (RDP_SEQNO(pkt) != rdpcb->r_rcvq.rq_baseseq)
	{
	    log(LOG_INFO, "RDP NULL 0x%x rcvq baseseq 0x%x\n",
		RDP_SEQNO(pkt), rdpcb->r_rcvq.rq_baseseq);
	    goto theygoofed;
	}
	rdpcb->r_rcvq.rq_msgs[rdpcb->r_rcvq.rq_front] = NULL;
	rdpcb->r_rcvq.rq_front =
	    (rdpcb->r_rcvq.rq_front +1) % rdpcb->r_rcvq.rq_maxqlen;
	rdpcb->r_rcvq.rq_baseseq ++;

	(void) rdp_sendpkt (rdpcb, (MBUF *) NULL, 0, rdpcb->r_sndnxt);
	m_freem(dtom(pkt));
    }
    else if (pkt->rh_dlen) 
    {
#ifdef RDP_CS
	if (rdp_qinsert (&rdpcb->r_rcvq, dtom(pkt), RDP_SEQNO(pkt)) == -1)
	    rdpcb->r_rcvd.r_retrans ++;
#else
	(void) rdp_qinsert (&rdpcb->r_rcvq, dtom(pkt), RDP_SEQNO(pkt));
#endif
	/* No (e)ack now.  Wait til gets to user */
    }
    else
	/* Was an ACK-only packet */
	m_freem(dtom(pkt));


    rdpisconnected(rdpcb);
    /* start up connection loss detection */
    rdpcb->r_timers[RDP_tNULL] = rdpcb->r_tvnull;
    return (RDP_sESTAB);
}

/*
 *	state: RDP_sLSYNRCVD
 *	input: RDP_iUCLOSE
 */
/*ARGSUSED*/
rdp_lsynrcvd_close (rdpcb, nil)
register RDPCB *rdpcb;
{
    /* send RST */
    rdpcb->r_sendrst = TRUE;
    (void) rdp_sendpkt (rdpcb, (MBUF *) NULL, 0, rdpcb->r_sndnxt);
    trash_pcbs(rdpcb);
    return (RDP_sCLOSED);
}

/*
 *	state: RDP_sLSYNRCVD
 *	input: RDP_iTIMER
 */
rdp_lsynrcvd_timer (rdpcb, timer)
RDPCB	*rdpcb;
{
    /* whether connecting via connect(2) (SYNSENT) or child of
     * or via child of a listen(2)ing socket (LSYNRCVD), need to
     * retransmit out syn until it gets acked.
     */


    return (rdp_synsent_timer (rdpcb, timer));
}

/************************************************************************/

/*
 *	state: RDP_sSYNRCVD
 *	input: RDP_iNETR
 */
rdp_synrcvd_netr (rdpcb, pkt)
RDPCB	*rdpcb;
RDPHDR	*pkt;
{
    return (rdp_lsynrcvd_netr(rdpcb, pkt));
}

/*
 *	state: RDP_sSYNRCVD
 *	input: RDP_iUCLOSE
 */
rdp_synrcvd_close (rdpcb, nil)
RDPCB	*rdpcb;
{
    return (rdp_lsynrcvd_close(rdpcb, nil));
}

/*
 *	state: RDP_sSYNRCVD
 *	input: RDP_iTIMER
 */
rdp_synrcvd_timer (rdpcb, timer)
RDPCB	*rdpcb;
{
    return (rdp_lsynrcvd_timer (rdpcb, timer));
}

/************************************************************************/

/*
 *	state: RDP_sESTAB
 *	input: RDP_iNETR
 */
rdp_estab_netr (rdpcb, pkt)
register RDPCB		*rdpcb;
register RDPHDR		*pkt;
{
    /*
     * ensure packet is in window.  If not, ack him to straighten things
     * out.
     */
    if (SEQ_LT(RDP_SEQNO(pkt), rdpcb->r_rcvq.rq_baseseq) ||
	SEQ_GEQ(RDP_SEQNO(pkt), rdpcb->r_rcvq.rq_baseseq + rdpcb->r_rcvq.rq_maxqlen)) 
    {
#ifdef RDP_CS
	rdpcb->r_rcvd.r_retrans ++;
#endif
	(void) rdp_sendpkt (rdpcb, (MBUF *) NULL, 0, rdpcb->r_sndnxt);
	m_freem(dtom(pkt));
	return (RDP_sSAME);
    }

    /*
     * Whenever we receive a packet and we're not already waiting for
     * an ack of a NULL we sent, reset NULL timer.  Connection is alive.
     *
     * Don't reset for any packet if have an outstanding NULL since want
     * to keep timer at zero and not generate a new NULL segment until
     * current one is acknowledged.  (This might be a new message, not
     * the NULL's ack.  Send and receive paths may differ?)
     *
     * Don't reset NULL timer on datagram transmissions since those imply
     * receiving ACKs.  Besides, we want to know if he is up, not if we're
     * up.
     */
    if (rdpcb->r_nullsent == 0)
	rdpcb->r_timers[RDP_tNULL] = rdpcb->r_tvnull;

    if (pkt->rh_flags &  (RDP_fSYN|RDP_fRST))
    {
	m_freem(dtom(pkt));
	set_error(rdpcb, ECONNRESET);

	if (pkt->rh_flags & RDP_fSYN)
	{
	    /*
	     * We've gotten past the syn stage.  He's confused.
	     * His syn has also changed sequence numbers.
	     */
	    rdpcb->r_sendrst = TRUE;
	    (void) rdp_sendpkt (rdpcb, (MBUF *) NULL, 0, rdpcb->r_sndnxt);
	    trash_pcbs(rdpcb);
	    return (RDP_sCLOSED);
	}

	/*
	 * Since we've been reset, the user cannot send anymore
	 * datagrams.  user_cantsendmore() also wakes writers up
	 * in case he is doing synchronous i/o and is waiting for
	 * buffering space at the (socket) level.
	 */
	user_cantsendmore(rdpcb);
	/*
	 * User can't read anymore either, per specification.
	 * Reliable delivery and acceptance must be determined
	 * by the application before closing.
	 */
	user_cantreadmore(rdpcb);
	cancel_timers(rdpcb);
	rdpcb->r_timers[RDP_tCLOSEWAIT] = rdpcb->r_closewait;
	return (RDP_sCLOSEWAIT);
    }

    if (pkt->rh_flags & RDP_fACK)
	he_acked (rdpcb, RDP_ACKNO(pkt));

    if (pkt->rh_flags & RDP_fEACK)
    {
	register int		 neacks;
	register EACKOPTIONS	*eackopt;

	neacks = (hdrlen(pkt) - RDPHDRSZ) / sizeof(EACKOPTIONS);
	eackopt = RDP_OPT(pkt, EACKOPTIONS *);
	while (--neacks >= 0)
	{
	    he_eacked (rdpcb, ntohl(eackopt->rh_eackno));
	    eackopt ++;
	}
    }

    if (pkt->rh_dlen > rdpcb->r_ourmaxlen)
    {
	log(LOG_INFO, "RDP pkt too large %d > %d\n",
	    pkt->rh_dlen, rdpcb->r_ourmaxlen);
theygoofed :
	rdp_uncon_rst(pkt);
	set_error(rdpcb, ECONNRESET);
	user_cantsendmore(rdpcb);
	user_cantreadmore(rdpcb);
	cancel_timers(rdpcb);
	rdpcb->r_timers[RDP_tCLOSEWAIT] = rdpcb->r_closewait;
	return (RDP_sCLOSEWAIT);
    }

    if (pkt->rh_flags & RDP_fNULL)
    {
	if (pkt->rh_dlen != 0)
	{
	    log(LOG_INFO, "RDP %d length NULL pkt\n", pkt->rh_dlen);
	    goto theygoofed;
	}
	if (RDP_SEQNO(pkt) != rdpcb->r_rcvq.rq_baseseq)
	{
	    log(LOG_INFO, "RDP NULL 0x%x rcvq baseseq 0x%x\n",
		RDP_SEQNO(pkt), rdpcb->r_rcvq.rq_baseseq);
	    goto theygoofed;
	}
	rdpcb->r_rcvq.rq_msgs[rdpcb->r_rcvq.rq_front] = NULL;
	rdpcb->r_rcvq.rq_front =
	    (rdpcb->r_rcvq.rq_front +1) % rdpcb->r_rcvq.rq_maxqlen;
	rdpcb->r_rcvq.rq_baseseq ++;

	(void) rdp_sendpkt (rdpcb, (MBUF *) NULL, 0, rdpcb->r_sndnxt);
	m_freem(dtom(pkt));
    }
    else if (pkt->rh_dlen) 
    {
#ifdef RDP_CS
	if (rdp_qinsert (&rdpcb->r_rcvq, dtom(pkt), RDP_SEQNO(pkt)) == -1)
	    rdpcb->r_rcvd.r_retrans ++;
#else
	(void) rdp_qinsert (&rdpcb->r_rcvq, dtom(pkt), RDP_SEQNO(pkt));
#endif
    }
    else
	/* Was an ACK-only packet */
	m_freem(dtom(pkt));


    if (usr_rbuf_is_empty(rdpcb)) 
    {
	register MBUF *m;

	if (m = rdp_qremove(&rdpcb->r_rcvq, !rdpcb->r_sequential)) 
	{
	    /*
	     * IP and RDP headers should be in the first mbuf.
	     * User does not see them.
	     */
	    pkt = (RDPHDR *) (mtod(m, char *) + sizeof(struct ip));
#ifdef RDP_CS
	    rdpcb->r_rcvd.r_nbytes += pkt->rh_dlen;
#endif
	    m->m_off += sizeof(struct ip) + hdrlen(pkt);
	    m->m_len -= sizeof(struct ip) + hdrlen(pkt);

	    usr_rbuf_append(rdpcb, m);
	    wakeup_reader(rdpcb);
	}
    }

    /*
     * datagrams go straight out in response to the send(2) PRU_SEND,
     * so getting (e)acks doesn't cause an outgoing datagram.
     * Hold off on (e)ack of incoming packet until user receives it
     * and we know that by PRU_RCV.
     */
    return (RDP_sSAME);
}

/*
 *	state: RDP_sESTAB
 *	input: RDP_iUCLOSE
 */
/*ARGSUSED*/
rdp_estab_close (rdpcb, nil)
register RDPCB *rdpcb;
{
    /* send RST */
    rdpcb->r_sendrst = TRUE;
    (void) rdp_sendpkt (rdpcb, (MBUF *) NULL, 0, rdpcb->r_sndnxt);

    /*
     * Don't retransmit unacked datagrams, since user can't pick them
     * up anymore once he's been reset (according to specification).
     * Reliable delivery and acceptance must be determined by the
     * application before closing.
     */
    cancel_timers(rdpcb);
    rdpcb->r_timers[RDP_tCLOSEWAIT] = rdpcb->r_closewait;
    return (RDP_sCLOSEWAIT);
}

/*
 *	state: RDP_sESTAB
 *	input: RDP_iTIMER
 */
rdp_estab_timer (rdpcb, timer)
register RDPCB	*rdpcb;
{
    register MBUF	*rxmit_data;
    register int	index, passes;
    rdpsequence	seqno;

    switch (timer)
    {
      case RDP_tRTTL:
	/* retransmission took too long */
	rttl(rdpcb);
	return (RDP_sSAME);

      case RDP_tRXMIT:
	/*
	 * ensure keep checking even if no packet goes
	 * out this time.  ACK will stop this.
	 */
	rdpcb->r_timers[RDP_tRXMIT] = RDP_tvRXCHECK;

	index	= rdpcb->r_sendq.rq_front;
	passes	= rdpcb->r_sndnxt - rdpcb->r_snduna;
	seqno	= rdpcb->r_sendq.rq_baseseq; /* == r_snduna */

	while (--passes >= 0)
	{
	    if (rdpcb->r_rxtimers[index])
	    {
		rdpcb->r_rxtimers[index] --;
		if (rdpcb->r_rxtimers[index] == 0)
		{
		    MBUF *m;

		    /*
		     * Over lossy networks, do not let
		     * the round trip time estimate
		     * drift unecessarily high.  If we're
		     * considering the round-trip-time-
		     * measuring packet lost, and are
		     * retransmitting it, then we should
		     * reset the round trip time measurment
		     */
		    if (rdpcb->r_rttiming)
			if (seqno == rdpcb->r_rttimed)
			    rdpcb->r_rttiming = FALSE;

		    m = rdpcb->r_sendq.rq_msgs[index];
		    if (m == RDP_NULLMSG)
		    {
			rdpcb->r_sendnull = TRUE;
			if (rdp_sendpkt (rdpcb, (MBUF *) NULL, 0, seqno) == 0)
			    time_rtt(rdpcb, seqno);

			/*
			 * Back off on retransmissions,
			 * because the host might be
			 * down or the network could be
			 * jammed.  rxmitime will drop
			 * to normal when we get the ACK
			 */
			rdpcb->r_rxmitime = MIN (RDP_tvRXMAX,
						 rdpcb->r_rxmitime << 1);

			if (++rdpcb->r_nullsent > RDP_MAXNULL)
			{
			    /* advisory only */
			    set_error(rdpcb, ETIMEDOUT);
			    wakeup_reader(rdpcb);
			    /* writer timeout via rttl */

			    /* avoid rollover to zero
			     *
			     * NOTE: user will get
			     * ETIMEDOUT on every
			     * rxmit, another reason
			     * to back off above.
			     */
			    rdpcb->r_nullsent --;
			}
		    }
		    else 
		    {
			if (rxmit_data =m_copy(m, 0, M_COPYALL))
			    /*
			     * When we 1st sent it, we
			     * remembered the len in m_act
			     */
			    if (rdp_sendpkt(rdpcb,rxmit_data,(int)m->m_act,seqno)==0)
			        time_rtt(rdpcb, seqno);

			/*
			 * We aren't backing off here,
			 * since the single number is
			 * used for all datagrams,
			 * each of which may be at a
			 * different nth rxmission
			 */
		    }

#ifdef RDP_CS
		    rdpcb->r_sent.r_retrans ++;
#endif
		    set_rxtimer (rdpcb, index);
		}
	    }
	    index = (index + 1) % rdpcb->r_sendq.rq_maxqlen;
	    seqno ++;
	}
	break;

      case RDP_tACKDELAY:
	(void) rdp_sendpkt (rdpcb, (MBUF *) NULL, 0, rdpcb->r_sndnxt);
	break;

      case RDP_tNULL:
	/*
	 * If we're retransmitting, then we don't need to
	 * send NULL messages.  The NULL timer drops to zero
	 * and gets restarted when we get some packet from
	 * them (rdp_estab_netr).  User will get ETIMEDOUT
	 * from retransmit took too long if we don't get a
	 * packet.
	 */
	if (rdpcb->r_rttlindex < 0)
	{
	    /* are not retransmitting */

	    /*
	     * Idea:  The connection has been idle for too
	     * long. send a NULL packet which has its own
	     * sequence number (so can distinguish slow to
	     * arrive ack from ack of this NULL) and
	     * retransmit it via normal packet
	     * retransmission algorithm.
	     */

	    if (rdp_qinsert(&rdpcb->r_sendq, RDP_NULLMSG, rdpcb->r_sndnxt) != 1)
		panic("rdp RDP_tNULL");

	    rdpcb->r_sendnull = TRUE;
	    (void) rdp_sendpkt (rdpcb, (MBUF *) NULL, 0, rdpcb->r_sndnxt);
	    index = rdpcb->r_sendq.rq_front + (rdpcb->r_sndnxt - rdpcb->r_snduna);
	    index %= rdpcb->r_sendq.rq_maxqlen;
	    set_rxtimer (rdpcb, index);
	    rdpcb->r_sndnxt ++;
	    rdpcb->r_nullsent = 1;
	}
	break;

      default:
	log(LOG_INFO, "rdp_estab_timer:  timer %d\n", timer);
    }

    return(RDP_sSAME);
}

/*
 *	state: RDP_sESTAB
 *	input: RDP_iRCV
 */
/*ARGSUSED*/
rdp_estab_rcv (rdpcb, nil)
register RDPCB	*rdpcb;
{
    MBUF	*m;

    /*
     * Now that user has received the packet, bump the front so that
     * we can ACK it and move the window along.
     */
    rdp_received (&rdpcb->r_rcvq);

    /*
     * user picked up the packet we left on the socket for him.
     * Let's put another one there.
     */
    if (m = rdp_qremove(&rdpcb->r_rcvq, !rdpcb->r_sequential))
    {
	RDPHDR *pkt;

	/*
	 * IP and RDP headers should be in the first mbuf.
	 * User does not see them.
	 */
	pkt = (RDPHDR *) (mtod(m, char *) + sizeof(struct ip));
#ifdef RDP_CS
	rdpcb->r_rcvd.r_nbytes += pkt->rh_dlen;
#endif
	m->m_off += sizeof(struct ip) + hdrlen(pkt);
	m->m_len -= sizeof(struct ip) + hdrlen(pkt);

	usr_rbuf_append(rdpcb, m);
	/* wakeup_reader(rdpcb); is awake, performing read(2) */
    }

    /*
     * Send an ACK, but apply an ACK-delay algorithm in order to
     * reduce CPU loading on both hosts involved.  Reduces network
     * load, too.  Skip at most one ACK.
     */
    if (rdpcb->r_timers[RDP_tACKDELAY])
	(void) rdp_sendpkt (rdpcb, (MBUF *) NULL, 0, rdpcb->r_sndnxt);
    else
	rdpcb->r_timers[RDP_tACKDELAY] = 1;

    return (RDP_sSAME);
}

/*
 *	state: RDP_sESTAB
 *	input: RDP_iSEND
 */
rdp_estab_send (rdpcb, m)
register RDPCB	*rdpcb;
register MBUF	*m;
{
    register MBUF	*copym;
    register int	 len;
    register int	 index;

    /*
     * q message on send q.
     */
    if (rdp_qinsert(&rdpcb->r_sendq, m, rdpcb->r_sndnxt) != 1)
	panic("rdp_estab_send");

    /*
     * Remember the length of the datagram for sending now,
     * and for retransmissions later.
     */
    len = 0;
    for (copym = m; copym; copym = copym->m_next)
	len += copym->m_len;
    m->m_act = ((MBUF *) len);

    /*
     * if reached end of window, block socket code from allowing
     * sends until get an ACK
     */
    if (SEQ_GEQ(rdpcb->r_sndnxt, rdpcb->r_snduna + rdpcb->r_hisnbuf -1))
	sendbufisfull(rdpcb);

    /*
     * send a copy of the datagram
     */
    if (copym = m_copy(m, 0, M_COPYALL))
	if (rdp_sendpkt(rdpcb, copym, len, rdpcb->r_sndnxt) == 0)
	    time_rtt (rdpcb, rdpcb->r_sndnxt);

    index = rdpcb->r_sendq.rq_front + (rdpcb->r_sndnxt - rdpcb->r_snduna);
    index %= rdpcb->r_sendq.rq_maxqlen;
    set_rxtimer(rdpcb, index);

    rdpcb->r_sndnxt ++;

    return (RDP_sSAME);
}

/************************************************************************/

/*
 *	state: RDP_sCLOSEWAIT
 *	input: RDP_iNETR
 */
rdp_closew_netr (rdpcb, pkt)
RDPCB	*rdpcb;
RDPHDR	*pkt;
{
    rdpstate newstate;

    if (pkt->rh_flags & RDP_fRST)
    {
	/*
	 * We've both agreed to shut down the connection
	 */
	trash_pcbs(rdpcb);
	newstate = RDP_sCLOSED;
    }
    else
	newstate = RDP_sSAME;

    m_freem(dtom(pkt));
    return(newstate);
}

/*
 *	state: RDP_sCLOSEWAIT
 *	input: RDP_iUCLOSE
 */
/*ARGSUSED*/
rdp_closew_close (rdpcb, nil)
register RDPCB *rdpcb;
{
    /*
     * rdp_usrreq() only allows one close call to the finite state machine.
     * Therefore, we entered CLOSEWAIT in response to a RST, not a close.
     * So, now both sides agree to close co-operatively.
     */
    rdpcb->r_sendrst = TRUE;
    (void) rdp_sendpkt (rdpcb, (MBUF *) NULL, 0, rdpcb->r_sndnxt);

    trash_pcbs(rdpcb);
    return(RDP_sCLOSED);
}

/*
 *	state: RDP_sCLOSEWAIT
 *	input: RDP_iTIMER
 */
rdp_closew_timer (rdpcb, timer)
RDPCB	*rdpcb;
{
    if (timer != RDP_tCLOSEWAIT)
    {
	log(LOG_INFO, "rdp_closew_timer:  timer %d\n", timer);
	return(RDP_sSAME);
    }

    trash_pcbs(rdpcb);
    return(RDP_sCLOSED);
}

/*
 *	state: RDP_sCLOSEWAIT
 *	input: RDP_iRCV
 */
/*ARGSUSED*/
rdp_closew_rcv(rdpcb, nil)
{
    /*
     * Technically, an illegal transition.  However, socket code drops
     * system priority level, allowing processing of a network packet
     * containing RDP reset to cause ESTAB -> CLOSEWAIT in the middle of
     * passing the user a packet.
     *
     * ESTAB ... user receives packet, priority dropped for uiomove()
     *      --- network packet processed ---
     * CLOSEWAIT ... socket code continues, causing this action.
     *
     * ### This can be a serious problem in general.
     */
}
#endif
