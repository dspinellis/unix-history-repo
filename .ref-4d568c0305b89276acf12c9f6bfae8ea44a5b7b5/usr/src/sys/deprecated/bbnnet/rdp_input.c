/*
 $Log:	rdp_input.c,v $
 * Revision 2.10  85/06/18  14:37:38  walsh
 * check for version mismatch.
 * 
 * Revision 2.9  85/04/08  14:35:11  root
 * *** empty log message ***
 * 
 * Revision 2.8  85/02/26  08:26:48  walsh
 * First pass at using IP source routing information to establish connections
 * (possibly with hosts not known by the Internet gateways.)  The hooks with
 * TCP could be better done - particularly dealing with IP addresses in the
 * header for checksums and tcpdb lookups.
 * 
 * Revision 2.7  84/11/15  09:55:52  walsh
 * redid how we deal with compiler padding in the RDP header structure.
 * 
 * Revision 2.6  84/11/08  16:11:17  walsh
 * Added code to gather statistics on RDP traffic.  This makes the RDPCB
 * too big unles you make mbufs 512 bytes large.  RDP_CS should be turned off
 * unless you do.
 * 
 * Revision 2.5  84/11/06  14:30:09  walsh
 * intorduced RDP_HLSHIFT
 * 
 * Revision 2.4  84/11/05  16:33:07  walsh
 * fix coding error.
 * 
 * Revision 2.3  84/11/05  10:51:53  walsh
 * flush debugging log if new state is RDP_sCLOSED so that packet printer/
 * system analyst sees final transitions.
 * 
 * Revision 2.2  84/11/02  15:28:19  walsh
 * Allow for RDP header fields not on natural boundries.  (Protocol
 * specifiers say will be part of next version in 6-12 months).
 * Until then, there goes the speed...  Yucho modifications.
 * 
 * Revision 2.1  84/11/02  10:12:58  walsh
 * Fixed to include RCS comments in checked out source.
 * 
 *
 * description:
 * Packet input processing for Reliable Datagram Protocol.
 * 
 * revision 1.6        
 * date: 84/07/19 10:21:22;  author: walsh;  state: Exp;  lines added/del: 2/1
 * Organized macros and classified their definitions in rdp_macros.h.
 * 
 * revision 1.5        
 * date: 84/07/10 09:59:38;  author: walsh;  state: Exp;  lines added/del: 10/10
 * declared some register variables.
 * 
 * revision 1.4        
 * date: 84/07/06 14:43:19;  author: wjacobso;  state: Exp;  lines added/del: 2/2
 * *** empty log message ***
 * 
 * revision 1.3        
 * date: 84/07/06 13:50:26;  author: wjacobso;  state: Exp;  lines added/del: 6/3
 * use RDP_ACTION macro instead of rdp_action
 * 
 * revision 1.2        
 * date: 84/07/06 09:49:20;  author: root;  state: Exp;  lines added/del: 27/45
 * This version seems to run bug-free.
 * 
 * revision 1.1        
 * date: 84/06/26 14:17:19;  author: walsh;  state: Exp;  
 * Initial revision
 */


#ifdef RDP
#ifdef	RCSIDENT
static char rcsident[] = "$Header: rdp_input.c,v 2.10 85/06/18 14:37:38 walsh Exp $";
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
#include "../bbnnet/in_var.h"
#include "../bbnnet/net.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/nopcb.h"
#include "../bbnnet/rdp.h"
#include "../bbnnet/rdp_macros.h"
#ifdef	HMP
#include "../bbnnet/hmp_traps.h"
#endif

extern int nosum;

/*
 * this is called from ip_input() upon reception of an RDP packet.
 */
rdp_input(mp)
register struct mbuf *mp;
{
    register RDPHDR		*pkt;
    register struct ip	*ip;
    rdpchecksum pktcksum;
    rdpchecksum cksum;
    register int hlen;
    register struct inpcb *inp;

    rdpstat.r_total++;

    /*
     * see ip_input().  Get access to constant part of RDP header.
     */
#define SZ (RDPHDRSZ + sizeof(struct ip))
    if ((mp->m_off > MMAXOFF) || (mp->m_len < SZ))
    {
	if ((mp = m_pullup(mp, SZ)) == NULL)
	{
	    rdpstat.r_tooshort ++;
	    return;
	}
    }
#undef SZ

    ip	= mtod(mp, struct ip *);
    pkt	= (RDPHDR *) (ip + 1);

    /* make sure header, incl. option region, does not overflow mbuf */

    hlen = hdrlen(pkt) + sizeof(struct ip);
    if (hlen > mp->m_len) 
    {
	if ((mp = m_pullup(mp, hlen)) == NULL)
	{
	    ip_log(ip, "rdp header overflow");
#ifdef HMPTRAPS
	    /* hmp_trap(T_TCP_OVFLO, (caddr_t)0,0); */
#else
	    /* netlog(mp); */
#endif
	    return;
	}
	ip = mtod(mp, struct ip *);
	pkt = (RDPHDR *) (ip + 1);
    }

    if (pkt->rh_ver != RDP_VERSION) 
    {
	ip_log (ip, "rdp version mismatch");
	netlog (mp);
	return;
    }

    /*
     * do checksum calculation, drop packet if bad
     * Checksum must be done on header in net form due to byte ordering
     * and rotations.
     */

    pktcksum = RDP_CKSUM(pkt);
    RDP_CKSUM(pkt) = 0;
    cksum = rdp_cksum(mp);
    if (cksum != pktcksum)
    {
	rdpstat.r_badsum++;
	if (! nosum)
	{
	    inet_cksum_err ("rdp", ip, (u_long) pktcksum, (u_long) cksum);
	    netlog(mp);
	    return;
	}
    }

    /* byte swap header */

    pkt->rh_dlen  = ntohs(pkt->rh_dlen);
    RDP_SEQNO(pkt) = ntohl(RDP_SEQNO(pkt));
    RDP_ACKNO(pkt) = ntohl(RDP_ACKNO(pkt));

    if (ip->ip_len != hdrlen(pkt) + pkt->rh_dlen)
    {
	ip_log(ip, "rdp length error");
	log(LOG_INFO, "%d + %d != %d\n", hdrlen(pkt), pkt->rh_dlen,
	    ip->ip_len);
	netlog(mp);
	return;
    }

    inp = in_pcblookup(&rdp, ip->ip_src.s_addr, (u_short)pkt->rh_sport,
			     ip->ip_dst.s_addr, (u_short)pkt->rh_dport, TRUE);
    if (inp == NULL)
    {
	/* nobody wants it */
	rdpstat.r_drops ++;
	rdp_uncon_rst (pkt);
    }
    else 
    {
	register rdpstate newstate;
	register RDPCB	*rdpcb;

	rdpcb = (RDPCB *)inp->inp_ppcb;

#ifdef RDP_CS
	rdpcb->r_rcvd.r_total ++;
	if (pkt->rh_flags & (RDP_fNULL|RDP_fRST|RDP_fSYN))
	{
	    if (pkt->rh_flags & RDP_fNULL)
		rdpcb->r_rcvd.r_nullpkts ++;
	    if (pkt->rh_flags & RDP_fRST)
		rdpcb->r_rcvd.r_rstpkts ++;
	    if (pkt->rh_flags & RDP_fSYN)
		rdpcb->r_rcvd.r_synpkts ++;
	}
#endif
	/* found a protocol control block for the message */
	RDP_ACTION(RDP_iNETR, rdpcb, ((int) pkt), newstate);
    }
}


/*
 * Call a subroutine specifically tailored to deal with this state
 * transition.
 */
rdpaction (input, rdpcb, arg)
register RDPCB	*rdpcb;
{
    register rdpstate newstate;

    RDP_ACTION (input, rdpcb, arg, newstate)
}

rdp_uncon_rst (pkt)
register RDPHDR	*pkt;
{
    register struct ip *ip;
    register struct mbuf *mp;
    struct in_addr tempinaddr;
    rdpportnum tempport;
    long his_seqno;
    int error;

    mp = dtom(pkt);

    /* make sure we don't send a RST in response to an RST */

    if (pkt->rh_flags & RDP_fRST) 
    {
	m_freem(mp);
	return;
    }
    ip = (struct ip *) (((caddr_t) pkt) - sizeof(struct ip));

    /* free everything but the header */

    m_freem(mp->m_next);
    mp->m_next = NULL;
    mp->m_len = sizeof(struct ip) + RDPHDRSZ;

    /* direct the packet back to the originator */

    tempinaddr = ip->ip_dst;
    ip->ip_dst = ip->ip_src;
    ip->ip_src = tempinaddr;

    tempport = pkt->rh_sport;
    pkt->rh_sport = pkt->rh_dport;
    pkt->rh_dport = tempport;

    /*
     * and initialize (seqno, ackno, flags) so that it's "in window"
     * and resets him independent of his state (is acceptable to all
     * net reception subroutines.)
     */
    his_seqno = RDP_SEQNO(pkt);
    RDP_SEQNO(pkt) = htonl(RDP_ACKNO(pkt) + 1);
    RDP_ACKNO(pkt) = htonl(his_seqno);
    if (pkt->rh_flags & RDP_fSYN)
	pkt->rh_flags = RDP_fRST|RDP_fACK;
    else
	pkt->rh_flags = RDP_fRST;

    /* and send it */

    pkt->rh_hdrlen	= RDPHDRSZ >> RDP_HLSHIFT;
    pkt->rh_dlen	= 0;
    RDP_CKSUM(pkt)	= 0;

    RDP_CKSUM(pkt)	= rdp_cksum(mp);

    NOPCB_IPSEND (mp, RDPHDRSZ, FALSE, error);
#ifdef lint
    error = error;
#endif
}

struct mbuf *rdpdebuf;
#ifdef RDPDEBUG
int rdprint;
#endif

/*
 * Write a record in the rdp debugging log
 */
rdp_debug(rdpcb, arg, input, newstate)
register RDPCB *rdpcb;
rdpstate newstate;
{
    register struct r_debug *dp;
    register struct mbuf *m;

#ifdef RDPDEBUG
    if (rdprint)
    {
	/*
	 * Print debugging info directly on the console (use this for 
	 * intial testing only).
	 */
	printf("RDP(0x%x) %s X %s", rdpcb, rdpstates[rdpcb->r_state],
	    (input < 0 ? "send pkt" : rdpinputs[input]) );

	if (input == RDP_iTIMER)
	    printf("(%s)", rdptimers[arg]);

	printf(" --> %s\n",
	    rdpstates[newstate==RDP_sSAME ? rdpcb->r_state : newstate];
    }
#endif

    /*
     * Get an mbuf to write the debugging record into.  If we don't already
     * have one, allocate a new one.
     */
    if ((m = rdpdebuf) == NULL)
    {
	register struct mbuf *c;

	if ((rdpdebuf = m = m_get(M_DONTWAIT, MT_DATA)) == NULL)
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
	    m->m_act = (struct mbuf *) RCDBLEN;
	}
	else
	m->m_act = (struct mbuf *) RDBLEN;
	m->m_len = 0;
    }

    dp = (R_DEBUG *) (mtod(m, char *) + m->m_len);
    /*
     * Set up the debugging record.
     */
    dp->rd_iptime	= iptime();
    dp->rd_input	= input;
    dp->rd_newstate	= newstate;
    dp->rd_rdpcb	= (*rdpcb);	/* structure copy */

    /*
     * input == RDP_iNETR           incoming packet
     *       == -1                  monitor outgoing packet.  Not a true
     *                              transition CAUSING event, but useful.
     */
    if ((input == RDP_iNETR) || (input < 0)) 
    {
	register struct ip *ip;
	register RDPHDR *pkt;

	ip = (struct ip *) arg;
	pkt = (RDPHDR *) (ip + 1);
	dp->rd_iphdr	= (*ip);	/* structure copy */
	dp->rd_rdphdr	= (*pkt);	/* structure copy */
    }
    else if (input == RDP_iTIMER)
	dp->rd_timer	= arg;

    /*
     * If the mbuf is full, dispatch it to a raw listener.
     * Also for transition to closed state so oberver sees all and
     * can debug stuff more easily.
     */
    m->m_len += sizeof(struct r_debug);
    if ((m->m_len >= ((int) m->m_act)) || (newstate == RDP_sCLOSED)) 
    {
	m->m_act = 0;
	rdpdebuglog(m);
	rdpdebuf = NULL;
    }
}
#endif
