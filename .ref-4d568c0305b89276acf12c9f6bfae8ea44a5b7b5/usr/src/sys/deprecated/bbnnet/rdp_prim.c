/*
 $Log:	rdp_prim.c,v $
 * Revision 2.15  85/03/12  08:41:13  walsh
 * Don't advise the user about ENOBUFS errors passed back from device drivers+
 * ip_send() when the protocol has a retransmission strategy.
 * 
 * Revision 2.14  85/03/06  10:06:16  walsh
 * Corrected some error handling and reporting.
 * 
 * Revision 2.13  85/02/26  08:26:56  walsh
 * First pass at using IP source routing information to establish connections
 * (possibly with hosts not known by the Internet gateways.)  The hooks with
 * TCP could be better done - particularly dealing with IP addresses in the
 * header for checksums and tcpdb lookups.
 * 
 * Revision 2.12  84/11/15  09:56:04  walsh
 * redid how we deal with compiler padding in the RDP header structure.
 * 
 * Revision 2.11  84/11/08  16:11:51  walsh
 * Added code to gather statistics on RDP traffic.  This makes the RDPCB
 * too big unles you make mbufs 512 bytes large.  RDP_CS should be turned off
 * unless you do.
 * 
 * Revision 2.10  84/11/06  15:24:26  walsh
 * *** empty log message ***
 * 
 * Revision 2.9  84/11/06  14:30:23  walsh
 * intorduced RDP_HLSHIFT
 * 
 * Revision 2.8  84/11/06  09:09:16  walsh
 * added missing include.
 * 
 * Revision 2.7  84/11/05  15:55:28  walsh
 * update_nulltimer() macro began to look inappropriate with recent
 * changes, so its been stripped out and put in-line.
 * 
 * Revision 2.6  84/11/05  15:23:23  walsh
 * *** empty log message ***
 * 
 * Revision 2.5  84/11/05  15:17:53  walsh
 * added comments on acknowledgement strategy.
 * 
 * Revision 2.4  84/11/05  11:05:20  walsh
 * comment and adjust number for rdp_iss in a mathematically correct way
 * as a result of benchmarks (cf. operationally correct).
 * 
 * Revision 2.3  84/11/02  18:24:09  walsh
 * Protocol specifiers want NULL message to have own sequence number in
 * case of slow (t>NULL msg timeout) packets.  I don't see this as a problem,
 * and even if happened (dubious) would only delay discovery, but I
 * didn't win this one.  Initially not designed for this, but fixes are
 * in almost neatly.
 * 
 * Revision 2.2  84/11/02  15:28:56  walsh
 * Allow for RDP header fields not on natural boundries.  (Protocol
 * specifiers say will be part of next version in 6-12 months).
 * Until then, there goes the speed...  Yucho modifications.
 * 
 * Revision 2.1  84/11/02  10:13:48  walsh
 * Fixed to include RCS comments in checked out source.
 * 
 *
 * description:
 * some primitives for RDP.
 * 
 * revision 1.10        
 * date: 84/07/19 10:21:35;  author: walsh;  state: Exp;  lines added/del: 1/0
 * Organized macros and classified their definitions in rdp_macros.h.
 * 
 * revision 1.9        
 * date: 84/07/18 18:50:44;  author: walsh;  state: Exp;  lines added/del: 5/0
 * Added provision for sending of NULL messages.  These are sent on an idle
 * connection to determine that the other side still exists.
 * 
 * revision 1.8        
 * date: 84/07/12 20:04:16;  author: walsh;  state: Exp;  lines added/del: 1/2
 * *** empty log message ***
 * 
 * revision 1.7        
 * date: 84/07/12 13:48:14;  author: walsh;  state: Exp;  lines added/del: 31/16
 * Rather than in-line stuffing of IP/RDP headers, at least half of which are
 * constant, copy headers in from a template of what the headers are like.  The
 * bcopy() call is turned into a movc3 instruction on the VAX by a sed script
 * run over the assembler output of the C compiler.  Marginal speed-up.
 * 
 * revision 1.6        
 * date: 84/07/12 09:29:48;  author: walsh;  state: Exp;  lines added/del: 6/9
 * Found:
 * 1.  If MGET saves anything over mget, it's down in the noise, so skip it.
 * 2.  stuff_eacks as macro DOES save time.
 * 3.  Optimized stuff_eacks by timing and looking at a lot of assembler output
 *	from the C compiler.
 * 
 * revision 1.5        
 * date: 84/07/10 09:41:52;  author: walsh;  state: Exp;  lines added/del: 43/41
 * Corrected problem with conversion of stuff_eacks from a function to a
 * macro.
 * 
 * Changed rdp_sendpkt to avoid unecessary allocation and deallocation of
 * an mbuf for syn and eack options.
 * 
 * revision 1.4        
 * date: 84/07/09 14:39:54;  author: walsh;  state: Exp;  lines added/del: 1/0
 * Part of ACK-delay algorithm.  Whenever send a packet with an ACK, set
 * ACK-delay timer to zero.
 * 
 * revision 1.3        
 * date: 84/07/06 14:10:35;  author: wjacobso;  state: Exp;  lines added/del: 34/34
 * stuff_eacks made into macro; added register var definitions
 * 
 * revision 1.2        
 * date: 84/07/06 09:49:37;  author: root;  state: Exp;  lines added/del: 11/4
 * This version seems to run bug-free.
 * 
 * revision 1.1        
 * date: 84/06/26 14:17:39;  author: walsh;  state: Exp;  
 * Initial revision
 * 
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
#include "../h/protosw.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../bbnnet/in.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/net.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/rdp.h"
#include "../bbnnet/rdp_macros.h"

extern struct inpcb rdp;

rdp_template(rdpcb)
RDPCB	*rdpcb;
{
    register struct ip	*ip;
    register RDPHDR		*pkt;
    register INPCB		*inp;

    ip	= (struct ip *) rdpcb->r_template;
    pkt	= (RDPHDR *) (ip+1);
    inp	= rdpcb->r_inpcb;

    ip->ip_p	= IPPROTO_RDP;
    ip->ip_tos	= 0;
    ip->ip_src	= inp->inp_laddr;
    ip->ip_dst	= inp->inp_faddr;

    pkt->rh_ver	= RDP_VERSION;
    pkt->rh_hdrlen	= RDPHDRSZ >> RDP_HLSHIFT;
    pkt->rh_sport	= inp->inp_lport;
    pkt->rh_dport	= inp->inp_fport;
    pkt->rh_flags	= 0;
    RDP_CKSUM(pkt)	= 0;
}

/***********************************************************************
 Comments on the Acknowledgement Strategy

 I.  It would be desirable to save network, CPU, and buffering resources
	by not retransmitting packets which have arrived at the destination
	host.  One might think of doing this by using the cumulative ack
	(ACK) for flow control and the extended ack (EACK) for reliability.
	[EACK upon reception by protocol module, ACK upon reception by
	application]

 You can't do this.

 1.  The protocol specifiers state that an implementation may choose
	to treat ACK#1 EACK#2 EACK#3 as ACK#3.  We want to avoid receiving
	packets we can't buffer.  (see goals above)
 2.  NULL packets are sent on idle connections (all packets acknowledged)
	and occupy sequence number space.  We want to avoid situations
	where a NULL packet goes out of our window, doesn't get acked,
	and the connection gets dropped.  (connection would be idle since
	no transmissions/retransmissions would be going on if packets
	received by RDP, but destination process busy doing something
	else.)
 3.  Relying on ACKs to be sent to move the window's edge after the
	datagram is picked up by the user process has to deal with the
	fact that that ACK may be lost.  In TCP, one uses a persistence
	timer to deal with this.  Use of ACK/EACK above implies that
	there is no data with which to perform persistence (if buffers
	reclaimed at EACK and not at ACK.)  One could think of using
	NULL messages to reopen the window, but a) NULL messages only
	start up after LONG delays,  b) implementations are only required
	to respond to, not initiate, NULL messages.

 2 would be a correctness error.  1+3 would be stylistic problems.
 Therefore, the acknowledgement strategy implemented here is to
 ACK or EACK messages only after they have been received by the destination
 process.  This means that some unecessary retransmissions might occur,
 but such is life.  (Unecessary from the point of view that the
 information made it through the network a-o.k.)

 If the application is slow to pick up messages from the protocol,
 then that will be reflected in the round trip time estimate (and
 retransmission time) for the other end.  So, maybe this isn't all
 that bad.  Especially since most processes will be fairly interested
 and responsive to network input.

 ***********************************************************************/

/*
 * rdp_init ensures max # of options fit in mbuf
 */
#define stuff_eacks(rdpcb, optm) \
{ \
	register int		 pass;					\
	register int		 index;					\
		 EACKOPTIONS	*eopt;					\
 \
	pass	= 0;							\
	index	= rdpcb->r_rcvq.rq_front;				\
 \
	do {								\
		if (rdpcb->r_rcvq.rq_msgs[index] == RDP_DELIVERED){	\
			if (optm == NULL){				\
				optm = m_get(M_DONTWAIT, MT_HEADER);	\
				if (optm == NULL)			\
					break;				\
				optm->m_len = 0;			\
				eopt = mtod(optm, EACKOPTIONS *);	\
			}						\
			eopt->rh_eackno =				\
				htonl(rdpcb->r_rcvq.rq_baseseq + pass); \
			eopt ++;					\
			optm->m_len += sizeof(EACKOPTIONS);		\
		}							\
		index = (index + 1) % rdpcb->r_rcvq.rq_maxqlen;		\
	} while (++pass < rdpcb->r_rcvq.rq_maxqlen);			\
}

rdp_sendpkt (rdpcb, data, datalen, seqnum)
register RDPCB	*rdpcb;
MBUF	*data;
int datalen;	/* length of data mbuf chain in bytes */
rdpsequence seqnum;	/* host order.  seq# of this packet */
{
    register MBUF	*m;
    register RDPHDR	*pkt;

    m = m_get(M_DONTWAIT, MT_HEADER);
    if (m == NULL)
    {
	m_freem(data);
	return (ENOBUFS);
    }

    m->m_len = sizeof(struct ip) + RDPHDRSZ;
    m->m_off = MMAXOFF
	- (sizeof(struct ip) + RDPHDRSZ + sizeof(SYNOPTIONS));
    m->m_next = data;

    pkt = (RDPHDR *) (mtod(m, caddr_t) + sizeof(struct ip));
    /*
     * Fills in most IP and RDP header fields
     */
    bcopy (rdpcb->r_template, mtod(m, caddr_t), (unsigned)RDP_TEMPLSIZE);

    pkt->rh_dlen	= htons((u_short)datalen);
    RDP_SEQNO(pkt)	= htonl((u_long)seqnum);

    if (rdpcb->r_sendrst)
    {
	pkt->rh_flags |= RDP_fRST;
#ifdef RDP_CS
	rdpcb->r_sent.r_rstpkts ++;
#endif
    }
    else 
    {
	if (! rdpcb->r_synacked)
	{
	    register SYNOPTIONS *synopt;

	    pkt->rh_flags	|= RDP_fSYN;
	    pkt->rh_hdrlen	+= sizeof(SYNOPTIONS) >> RDP_HLSHIFT;
	    m->m_len	+= sizeof(SYNOPTIONS);
	    synopt	= RDP_OPT(pkt, SYNOPTIONS *);
	    synopt->rh_nbuf = htons((u_short)rdpcb->r_ournbuf);
	    synopt->rh_maxlen = htons((u_short)rdpcb->r_ourmaxlen + HDRSLOP);
	    synopt->rh_options = 0;
	    if (rdpcb->r_sequential)
		synopt->rh_options |= RDP_oSEQUENTIAL;
	    synopt->rh_options = htons(synopt->rh_options);
#ifdef RDP_CS
	    rdpcb->r_sent.r_synpkts ++;
#endif
	}
	else 
	{
	    register MBUF *optm;

	    /* possible EACK */
	    optm = NULL;
	    stuff_eacks(rdpcb, optm);

	    if (optm)
	    {
#define OKSZ (datalen+RDPHDRSZ+sizeof(struct ip)+optm->m_len <= rdpcb->r_hismaxlen)
		if (OKSZ) 
		{
		    pkt->rh_flags |= RDP_fEACK;
		    pkt->rh_hdrlen += optm->m_len >> RDP_HLSHIFT;
		    optm->m_next = m->m_next;
		    m->m_next = optm;
		}
		else
		    m_free(optm);
#undef OKSZ
	    }

	    if (rdpcb->r_sendnull)
	    {
		rdpcb->r_sendnull = FALSE;
		pkt->rh_flags |= RDP_fNULL;
#ifdef RDP_CS
		rdpcb->r_sent.r_nullpkts ++;
#endif
	    }
	}

	if (rdpcb->r_synrcvd)
	{
	    rdpcb->r_timers[RDP_tACKDELAY] = 0;
	    pkt->rh_flags |= RDP_fACK;
	    RDP_ACKNO(pkt) = htonl(rdpcb->r_rcvq.rq_baseseq -1);
	}
    }

    RDP_CKSUM(pkt) = rdp_cksum (m);

#ifdef RDP_CS
    rdpcb->r_sent.r_total ++;
#endif

    if (debug_rdpcb(rdpcb))
	rdp_debug (rdpcb, mtod(m, caddr_t), -1, RDP_sSAME);

    /*
     * and ship packet off via IP.  Remember that since this protocol
     * involves retransmissions, errors can occur asynchronous to a
     * (write) system call, and that therefore we can not send the
     * error all the way back up through subroutine return values.  We
     * must also post it back via advise_user() at some point, and this
     * looks like a good point to try it.
     */
    {
	register int	error;

	error = ip_send (rdpcb->r_inpcb, m,(int)( datalen + hdrlen(pkt)),FALSE);

	if (error)
	    /*
	     * Since we use retransmissions, don't need to tell user
	     * process about this.  (Can be as simple as interface
	     * or host structure queues are too long due to current
	     * heavy traffic.  Backing off will take care of that.)
	     */
	    if (error != ENOBUFS)
		advise_user(rdpcbtoso(rdpcb), error);
	return (error);
    }
}

rdp_timeo()
{
    register struct inpcb *inp, *next;
    register RDPCB	*rdpcb;
    register int	 timer;
    int s;
    register rdpstate newstate;

    s = splnet();
    rdp_iss += RDP_ISSINCR;
    /*
     * Remember, if CLOSEWAIT timer goes off, lose rdpcb's mbuf and
     * and next pointer in it.
     */
    inp = rdp.inp_next;
    while (inp != &rdp)
    {
	next = inp->inp_next;
	if (rdpcb = inptordpcb(inp))
	{
	    rdpcb->r_rtt ++;

	    for (timer = 0; timer < RDP_NTIMERS; timer++)
	    {
		if (rdpcb->r_timers[timer])
		{
		    rdpcb->r_timers[timer] --;
		    if (rdpcb->r_timers[timer] == 0)
		    {
			RDP_ACTION (RDP_iTIMER, rdpcb, timer, newstate)
			if (newstate == RDP_sCLOSED)
			    /* next rdpcb */
			    break;
		    }
		}
	    }
	}
	inp = next;
    }
    splx(s);
}
#endif
