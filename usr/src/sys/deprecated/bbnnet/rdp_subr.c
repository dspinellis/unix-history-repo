/*
 $Log:	rdp_subr.c,v $
 * Revision 2.7  84/11/21  12:06:30  walsh
 * *** empty log message ***
 * 
 * Revision 2.6  84/11/08  16:12:53  walsh
 * Added code to gather statistics on RDP traffic.  This makes the RDPCB
 * too big unles you make mbufs 512 bytes large.  RDP_CS should be turned off
 * unless you do.
 * 
 * Revision 2.5  84/11/06  13:54:21  walsh
 * *** empty log message ***
 * 
 * Revision 2.4  84/11/05  16:25:18  walsh
 * tied rdp to icmp source quenches.  See icmp_quench and rdp_quench.
 * 
 * Revision 2.3  84/11/05  15:55:13  walsh
 * update_nulltimer() macro began to look inappropriate with recent
 * changes, so its been stripped out and put in-line.
 * 
 * Revision 2.2  84/11/02  18:25:47  walsh
 * Protocol specifiers want NULL message to have own sequence number in
 * case of slow (t>NULL msg timeout) packets.  I don't see this as a problem,
 * and even if happened (dubious) would only delay discovery, but I
 * didn't win this one.  Initially not designed for this, but fixes are
 * in almost neatly.
 * 
 * Revision 2.1  84/11/02  10:15:35  walsh
 * Fixed to include RCS comments in checked out source.
 * 
 *
 * description:
 * Some subroutines for manipulating the datagram q's for RDP.
 * 
 * revision 1.11        
 * date: 84/07/20 10:30:42;  author: walsh;  state: Exp;  lines added/del: 21/1
 * Tied RDP acknowledgements to ping reduction, just like TCP.
 * 
 * revision 1.10        
 * date: 84/07/19 10:22:33;  author: walsh;  state: Exp;  lines added/del: 1/17
 * Organized macros and classified their definitions in rdp_macros.h.
 * 
 * revision 1.9        
 * date: 84/07/17 22:35:26;  author: walsh;  state: Exp;  lines added/del: 3/0
 * Ensure cannot bind port number greater than RDP_pMAX.
 * 
 * revision 1.8        
 * date: 84/07/12 10:12:48;  author: walsh;  state: Exp;  lines added/del: 14/18
 * some small optimizations.
 * 
 * revision 1.7        
 * date: 84/07/12 09:39:06;  author: walsh;  state: Exp;  lines added/del: 2/4
 * small optimizations.  ( a = (a+1)%b quicker than a++; a %= b)
 * 
 * revision 1.6        
 * date: 84/07/10 14:58:24;  author: walsh;  state: Exp;  lines added/del: 10/3
 * Now no unecessary wakeups of the user process are done.
 * 
 * revision 1.5        
 * date: 84/07/10 10:38:24;  author: walsh;  state: Exp;  lines added/del: 13/13
 * added register declarations.
 * 
 * revision 1.4        
 * date: 84/07/06 14:28:53;  author: wjacobso;  state: Exp;  lines added/del: 6/6
 * *** empty log message ***
 * 
 * revision 1.3        
 * date: 84/07/06 14:17:02;  author: wjacobso;  state: Exp;  lines added/del: 8/8
 * added register var definitions
 * 
 * revision 1.2        
 * date: 84/07/06 09:51:12;  author: root;  state: Exp;  lines added/del: 2/1
 * This version seems to run bug-free.
 * 
 * revision 1.1        
 * date: 84/06/26 14:18:30;  author: walsh;  state: Exp;  
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
#include "../h/syslog.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../bbnnet/in.h"
#include "../bbnnet/net.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/icmp.h"
#include "../bbnnet/rdp.h"
#include "../bbnnet/seq.h"
#include "../bbnnet/rdp_macros.h"

/*
 * Called on ACK of a message we sent.
 */
he_acked(rdpcb, msgnum)
register RDPCB		*rdpcb;
rdpsequence msgnum;
{
    register int		 index;
    register int		 i;
    register MBUF		*m;

    index = msgnum - rdpcb->r_sendq.rq_baseseq;
    if (index < 0 || index >= rdpcb->r_sendq.rq_maxqlen)
	return;

    /*
     * an ACK is cumulative and may be for more than one message
     */
    for (i=0; i<=index; i++)
    {
	register int j;

	j = (rdpcb->r_sendq.rq_front + i) % rdpcb->r_sendq.rq_maxqlen;
	m = rdpcb->r_sendq.rq_msgs[j];
	/*
	 * ignore redundant ACKs.  May have been EACKed (RDP_DELIVERED).
	 */
	if (m)
	{
	    if (m == RDP_NULLMSG)
	    {
		/* and restart connection loss detection */
		rdpcb->r_nullsent = 0;
		rdpcb->r_timers[RDP_tNULL] = rdpcb->r_tvnull;
	    }
	    else if (m != RDP_DELIVERED) 
	    {
#ifdef RDP_CS
		/* count when acked, not queued */
		rdpcb->r_sent.r_nbytes += (int) m->m_act;
#endif
		m_freem(m);
	    }
	    rdpcb->r_sendq.rq_msgs[j] = NULL;
	}
	clear_rxtimer (rdpcb, j);
    }

    /*
     * Ensure front is always NULL or an undelivered (unacked) message.
     */
    rdpcb->r_sendq.rq_front   += (index +1);
    rdpcb->r_sendq.rq_front   %= rdpcb->r_sendq.rq_maxqlen;
    rdpcb->r_sendq.rq_baseseq += (index +1);	/* bumps r_snduna */

    /*
     * and, did this ack allow us to measure current round trip time?
     */
    if (rdpcb->r_rttiming)
    {
	if (SEQ_GT(rdpcb->r_sendq.rq_baseseq, rdpcb->r_rttimed))
	{
	    update_rttestimate(rdpcb);
	    update_rxmitime(rdpcb);
	    rdpcb->r_rttiming = FALSE;
	}
    }

#ifdef BBNPING
    /*
     * We've sent him NEW data, perhaps by a gateway, that he
     * has successfully received.  If that's the case, then
     * we know the route works and we don't have to ping that
     * gateway.
     *
     * see check_ping()
     */
    {
	register struct rtentry *rt;

	if (rt = rdpcb->r_inpcb->inp_route.ro_rt)
	    if (rt->rt_flags & RTF_GATEWAY)
		rt->irt_pings = (-1);
    }
#endif

    /*
     * and let sender send more pkts now that we have space.
     */
    sendbufhasspace(rdpcb);
}

/*
 * Called on EACK of a message we sent.
 */
he_eacked(rdpcb, msgnum)
register RDPCB		*rdpcb;
rdpsequence msgnum;
{
    register int		 index;
    register MBUF		*m;

    index = msgnum - rdpcb->r_sendq.rq_baseseq;
    if (index < 0 || index >= rdpcb->r_sendq.rq_maxqlen)
	return;

    index = (index + rdpcb->r_sendq.rq_front) % rdpcb->r_sendq.rq_maxqlen;
    m = rdpcb->r_sendq.rq_msgs[index];
    /*
     * ignore redundant EACKs
     */
    if (m && (m != RDP_DELIVERED)) 
    {
	if (m == RDP_NULLMSG)
	{
	    /* and restart connection loss detection */
	    rdpcb->r_nullsent = 0;
	    rdpcb->r_timers[RDP_tNULL] = rdpcb->r_tvnull;
	    log(LOG_INFO, "Incorrect ACK strategy on rdpcb 0x%x\n", rdpcb);
	}
	else 
	{
#ifdef RDP_CS
	    rdpcb->r_sent.r_nbytes += (int) m->m_act;
#endif
	    m_freem(m);
	}
	rdpcb->r_sendq.rq_msgs[index] = RDP_DELIVERED;
	clear_rxtimer(rdpcb, index);

	/*
	 * did this eack allow us to measure current round trip time?
	 */
	if (rdpcb->r_rttiming)
	{
	    if (msgnum == rdpcb->r_rttimed)
	    {
		update_rttestimate(rdpcb);
		update_rxmitime(rdpcb);
		rdpcb->r_rttiming = FALSE;
	    }
	}
    }
}


/*
 * Grab a message for passing to the user.  msgq is our rcvq.
 * Called on net reception if user recv q is empty.
 * Called on PRU_RECV after user picks up current packet on socket.
 * Only one packet is attached to the socket at a time.
 */
MBUF *rdp_qremove(msgq, async)
register RDP_MSGQ	*msgq;
{
    MBUF	*m;
    int	index;
    int	pass;

    index = msgq->rq_front;
    pass = msgq->rq_maxqlen;
    do
    {
	m = msgq->rq_msgs[index];
	if (m && m != RDP_DELIVERED)
	{
	    msgq->rq_msgs[index] = RDP_DELIVERED;
	    return (m);
	}
	index = (index +1) % msgq->rq_maxqlen;
    }
    while (async && (--pass > 0));

    return (NULL);
}

/*
 * rdp_qremove() grabbed a message to pass to the user.  When he picks it up,
 * PRU_RCVD occurs.  At that point, we bump front and we send an ACK.
 */
rdp_received(msgq)
register RDP_MSGQ	*msgq;
{
    register MBUF		*m;
    register int		 index;

    do
    {
	index	= msgq->rq_front;
	m	= msgq->rq_msgs[index];
	if (m == RDP_DELIVERED)
	{
	    msgq->rq_front = (msgq->rq_front +1) % msgq->rq_maxqlen;
	    msgq->rq_baseseq ++;
	    msgq->rq_msgs[index] = NULL;
	}
    }
    while (m == RDP_DELIVERED);
}

/*
 * Put a message on our send or rcv q.
 *
 *	0	internal error somewhere
 *	1	new message
 *	-1	duplicate message
 */
rdp_qinsert(msgq, m, msgnum)
register RDP_MSGQ	*msgq;
MBUF	*m;
rdpsequence msgnum;
{
    register int		 index;
    int isdup;

    index = msgnum - msgq->rq_baseseq;
    if ((index < 0) || (index >= msgq->rq_maxqlen)) 
    {
	m_freem(m);
	return(0);
    }

    index = (index + msgq->rq_front) % msgq->rq_maxqlen;
    if (msgq->rq_msgs[index] == RDP_DELIVERED)
    {
	/* rcvd duplicate of a message the user already has on socket */
	m_freem(m);
	isdup = -1;
    }
    else 
    {
	if (msgq->rq_msgs[index])
	{
	    m_freem(msgq->rq_msgs[index]);
	    isdup = -1;
	}
	else
	    isdup = 1;
	msgq->rq_msgs[index] = m;
    }
    return(isdup);
}
#endif
