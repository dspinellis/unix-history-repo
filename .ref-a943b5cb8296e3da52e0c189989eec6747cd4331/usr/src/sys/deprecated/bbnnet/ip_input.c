#ifdef	RCSIDENT
static char rcsident[] = "$Header: ip_input.c,v 1.39 85/07/31 09:31:26 walsh Exp $";
#endif

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/errno.h"
#include "../h/protosw.h"
#include "../h/domain.h"
#include "../h/syslog.h"

#include "../net/if.h"
#include "../net/route.h"
#include "../net/raw_cb.h"

#include "../bbnnet/in.h"
#include "../bbnnet/net.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/icmp.h"
#ifdef	HMP
#include "../bbnnet/hmp_traps.h"
#endif

#ifdef	RCSIDENT
static char rcsiphdr[] = RCSIPHDR;
#endif

#define rawreaders(r)	 	((r)->rcb_next != (r))
#define any_rawreaders()	rawreaders(&rawcb)

#define FIELD_OFF(fld, type) (((char *) (&(((type *) 0)->fld))) - ((char *) 0))

struct in_stat otherstat;
struct ip_stat ipstat;
struct {
 	struct ipq *n_ip_head;		/* -> top of ip reass. queue */
	struct ipq *n_ip_tail;		/* -> end of ip reass. queue */
} ipfrags;


/*
 * The protocol layers above IP assume the IP header and the protocol
 * header are contiguous.  However, need to save the options in case
 * a connection oriented protocol (RDP/TCP) wants to respond to an
 * incoming packet (SYN) over the same route if the packet got here
 * using IP source routing.  This allows connection establishment and
 * maintenance when the remote end is on a network that is not known
 * to our gateways.  Only applicable options are Loose/Strict source
 * routing, so rather than saving them in ip_stripopt() and reinterpreting
 * them, we'll set up something specific and appropriate here.
 */

int nosum = 0;
int	ip_nhops = 0;
struct in_addr ip_hops[MAX_IPOPTLEN / sizeof(struct in_addr)];

/* use a dispatch table for protocol dispatching */
struct ipswitch ipsw[IPPROTO_MAX];

extern struct ip *ip_reass();


ip_init()
{
    register struct protosw *pr;
    register int i;

    pr = pffindproto(PF_INET, IPPROTO_IP, SOCK_RAW);

    /* build our internal switch table */
    for(i=0; i < IPPROTO_MAX; i++)
    {
	ipsw[i].ipsw_raw = pr;
	ipsw[i].ipsw_user = 0;
	ipsw[i].ipsw_hlen = sizeof(struct ip);
    }

    for(pr = inetdomain.dom_protosw; pr < inetdomain.dom_protoswNPROTOSW; pr++)
    {
	if (pr->pr_protocol >= IPPROTO_MAX)
	    panic("ip_init");

	if (pr->pr_type == SOCK_RAW)
	    ipsw[pr->pr_protocol].ipsw_raw = pr;
	else
	    ipsw[pr->pr_protocol].ipsw_user = pr;
    }

    ipintrq.ifq_maxlen = IFQ_MAXLEN;	/* got a better number? */
}

/*
 * Being global might be a little gross, but we just got the interface pointer
 * passed up the week before 4.3 release.
 */
struct ifnet *inetifp;

/*
 * IP network software interrupt service routine.  Dequeue a message from the
 * ip input queue, and pass to internal ip input processor (ip_input).
 */
ipintr()
{
    extern char *panicstr;
    register struct mbuf *m;
    register int s;

    /*
     * Half-hearted attempt to prevent recursive panics due to network
     * bugs since panic calls boot, which lowers spl.  Note that the
     * drivers will still store packets in mbufs and that some processing
     * (ARP, Chaosnet) that occurs at splimp() will still proceed.  At
     * least this should preserve state information for post-mortem.
     */
    if (panicstr == NULL)
    {
	for (;;) 
	{
	    /* for 4.3, Berkeley finally changed imp code to queue up messages
	     * for ctlinput path, so don't need to check for them here with our
	     * old queueing mechanism
	     */

	    s = splimp();
	    IF_DEQUEUEIF (&ipintrq, m, inetifp);
	    splx(s);
	    if (m == NULL)
		return;

	    ip_input(m);
	}
    }
}

/*
 * IP level input routine 
 *
 * Called from local net level upon recpt of an internet datagram or fragment.
 * This routine does fragment reassembly, if necessary, and passes completed
 * datagrams to higher level protocol processing routines on the basis of the 
 * ip header protocol field.  It is passed a pointer to an mbuf chain 
 * containing the datagram/fragment. The mbuf offset+length are set to point 
 * at the ip header.
 */
ip_input(mp)
register struct mbuf *mp;
{
    register struct ip *ip;
    register int hlen;
    register int i;
    register struct mbuf *m;
    register struct ipq *fp;
    int fragsize = 0;
    struct in_ifaddr *ia;
    int j;
    struct inpcb fwdinp;

    ipstat.ip_total ++;
    /*
     * make sure dtom() macro works by getting header out of mbufs using
     * pages, also make sure header contiguous in first mbuf.
     */
    if ((mp->m_off > MMAXOFF) || (mp->m_len < sizeof(struct ip)))
    {
	/*
	 * Might as well avoid doing m_pullup twice.
	 * Common case is using cluster for large chunk of data.
	 */
	if (mp->m_len < FIELD_OFF(ip_p, struct ip) + sizeof(ip->ip_p))
	    i = sizeof(struct ip);
	else
	{
	    ip = mtod(mp, struct ip *);
	    i = ipsw[ip->ip_p].ipsw_hlen;
	}

	if ((mp = m_pullup(mp, i)) == NULL)
	{
	    ipstat.ip_tooshort ++;
	    return;
	}
    }

    ip = mtod(mp, struct ip *);

    /*
     * make sure header does not overflow mbuf  (is contiguous)
     */
    hlen = ip->ip_hl << IP_HLSHIFT;
    if (hlen > mp->m_len)
    {
	if ((mp = m_pullup(mp, hlen)) == NULL)
	{
	    ip_log(ip, "ip header overflow");
#ifdef HMPTRAPS
	    /* hmp_trap(T_IP_OVFLO, (caddr_t)0,0); */
#else
	    /* netlog(mp); no longer have mbuf list */
#endif
	    return;
	}
	ip = mtod(mp, struct ip *);
    }

    /*
     * Adjust msg length to remove any driver padding.  Make sure that 
     * message length matches ip length.
     */
    for (i = 0, m = mp; m->m_next != NULL; m = m->m_next) 
	i += m->m_len;
    i -= ntohs((u_short)ip->ip_len) - m->m_len;
    if (i != 0) 
    {
	if (i > (int)m->m_len)
	    m_adj(mp, -i);
	else if (i < 0) 
	{
	    ip_log(ip, "truncated ip packet");
#ifdef HMPTRAPS
	    /* hmp_trap(T_IP_TRUNC, (caddr_t)0, 0); */
#else
	    netlog(mp);
#endif
	    return;
	}
	else
	    m->m_len -= i;
    }

    i = (u_short)ip->ip_sum;
    ip->ip_sum = 0;

    /* used to do inline cksum here via sed */
    if (i != (j = (u_short)in_cksum(dtom(ip), hlen))) 
    {
	ipstat.ip_badsum++;
	if (!nosum) 
	{
#ifdef HMPTRAPS
	    /* hmp_trap(T_IP_CKSUM, (caddr_t)0,0); */
#endif
	    inet_cksum_err ("ip", ip, (u_long) i, (u_long) j);

	    ip->ip_sum = i;
	    ic_errmsg (icmp_addr(ip), ip->ip_src,
		ICMP_PARM, 0, FIELD_OFF(ip_sum, struct ip),
		hlen + ICMP_ERRLEN, (char *) ip);

	    netlog(mp);
	    return;
	}
    }

    /*
     * Make sure this packet is addressed to us before we put fields
     * in host order and do reassembly (fragments may reach destination
     * by separate routes.)
     * Remember that IP source routing option can change ip_dst, so have
     * to do option processing for all incoming packets with options.
     * Even if the packet turns out to be for us and we strip them.
     */
    fwdinp.inp_route.ro_rt = NULL; /* in case fwd, but no options */
    ip_nhops = 0;	/* for source routed TCP/RDP... connections */
    if (hlen > sizeof(struct ip))
	/*
	 * Any route allocated by ip_opt() 1. will be used by
	 * ip_forward(), and 2. will only be needed by ip_forward()
	 */
	if (! ip_opt (ip, hlen, &fwdinp))
	{
	    ip_log (ip, "ip option error");
	    netlog (mp);
	    return;
	}

    /*
     * if packet is not for us then forward
     */
    if ((ia = in_iawithaddr(ip->ip_dst, TRUE)) == NULL)
    {
	ip_forward (&fwdinp, ip, mp, hlen);
	return;
    }

    ip->ip_len = ntohs((u_short)ip->ip_len);
    if ((int)(ip->ip_len -= hlen) < 0) 
    {     /* length of data */
	ip_log(ip, "ip header length error");
#ifdef HMPTRAPS
	/* hmp_trap(T_IP_HLEN, (caddr_t)0,0); */
#else
	netlog(mp);
#endif
	return;
    }
    ip->ip_off = ntohs((u_short)ip->ip_off);
    ip->ip_mff = ((ip->ip_off & ip_mf) ? TRUE : FALSE);
    ip->ip_off <<= IP_OFFSHIFT;

    /* look for chain on reassembly queue with this header */

    for (fp = ipfrags.n_ip_head; (fp != NULL && (
	ip->ip_src.s_addr != fp->iqh.ip_src.s_addr ||
	ip->ip_dst.s_addr != fp->iqh.ip_dst.s_addr ||
	ip->ip_id != fp->iqh.ip_id ||
	ip->ip_p != fp->iqh.ip_p)); fp = fp->iq_next);

    if (!ip->ip_mff && ip->ip_off == 0) 
    {    /* not fragmented */

	if (fp != NULL) 
	{               /* free existing reass chain */
	    struct ip *q;
	    struct ip *p;

	    q = fp->iqx.ip_next; /* free mbufs assoc. w/chain */
	    while (q != (struct ip *)fp) 
	    {
		p = q->ip_next;
		m_freem(dtom(q));
		q = p;
	    }
	    ip_freef(fp); /* free header */
	}

	/*
	 * The options aren't of any use to higher level
	 * protocols or of any concern to the user process.
	 */
	if (hlen > sizeof(struct ip))
	    ip_stripopt (ip, hlen);

    }
    else 
    {
	ip = ip_reass(ip, fp, &fragsize);
	if (ip == NULL)
	    return;
	mp = dtom(ip);
    }

    /* call next level with completed datagram */

    /* any raw readers?? */
    /* if (rawreaders((struct rawcb *)ipsw[ip->ip_p].ipsw_raw->pr_ppcbq)) */
    if (any_rawreaders())
    {
	otherstat.in_total++;
	if (m = m_copy(mp, 0, M_COPYALL))
	    ipsw[ip->ip_p].ipsw_raw->pr_input(m);
    }

    if (ip->ip_p == IPPROTO_TCP)
	/* wish I didn't need this special case for fragsize */
	tcp_input(mp, fragsize);
    else if (ipsw[ip->ip_p].ipsw_user != 0)
	/* There's a protocol implementation for these packets */
	ipsw[ip->ip_p].ipsw_user->pr_input(mp);
    else if (ip->ip_p == IPPROTO_ICMP)
	/*
	 * Since don't want user to get a non-raw ICMP socket, did not make
	 * an entry in the protocol jump table; also wanted to be able to
	 * make raw ICMP socket.
	 */
	icmp (mp);
    else
    {
	/*
	 * Don't bother everyone on the net, and remember some other
	 * host may support the protocol.
	 */
	if ((!in_broadcast(ip->ip_src)) && (!in_broadcast(ip->ip_dst)))
	    ic_errmsg (icmp_addr(ip), ip->ip_src,
		ICMP_UNRCH, ICMP_UNRCH_PR, FIELD_OFF(ip_p, struct ip),
		sizeof(struct ip) + ICMP_ERRLEN, (char *) ip);

	/* get rid of the packet */
	ipstat.ip_drops++;
	m_freem(mp);
    }
}


/*
 * We've received an IP fragment.  Try to perform IP reassembly.
 */
struct ip *ip_reass(ip, fp, fragsize)
register struct ip *ip;
register struct ipq *fp;
int *fragsize;
{
    register struct ip *q, *savq;
    register struct mbuf *mp;
    int hlen;
    int i;

    mp = dtom(ip);
    hlen = ip->ip_hl << IP_HLSHIFT;

    if (ip->ip_off != 0) 
    {
	/*
	 * Only the first fragment retains the IP header.
	 */
	mp->m_off += hlen;
	mp->m_len -= hlen;
    }

    if (fp == NULL) 
    {
	/*
	 * This is the first fragment of the IP datagram that we've
	 * received.  Set up reassembly q header.
	 */
	struct mbuf *m;

	if ((m = m_get(M_WAIT, MT_FTABLE)) == NULL) 
	{
	    m_freem(mp);
	    return(NULL);
	}

	fp = mtod(m, struct ipq *);
	fp->iqx.ip_next = fp->iqx.ip_prev = (struct ip *)fp;
	bcopy((caddr_t)ip, (caddr_t)&fp->iqh, sizeof(struct ip));
	fp->iqx.ip_ttl	= MAXTTL;
	fp->iq_size	= 0;
	/*
	 * and enter this into the list of fragmented IP datagrams
	 */
	fp->iq_next	= NULL;
	fp->iq_prev	= ipfrags.n_ip_tail;
	if (ipfrags.n_ip_head != NULL) 
	    ipfrags.n_ip_tail->iq_next = fp;
	else 
	    ipfrags.n_ip_head = fp;
	ipfrags.n_ip_tail = fp;
    }

    /*
     * Merge fragment into reass.q
     *
     * Algorithm:   Match  start  and  end  bytes  of new
     * fragment  with  fragments  on  the  queue.   If no
     * overlaps  are  found,  add  new  frag. to the queue.
     * Otherwise, adjust start and end of new frag.  so  no
     * overlap   and   add  remainder  to  queue.   If  any
     * fragments are completely covered by the new one,  or
     * if  the  new  one is completely duplicated, free the
     * fragments.
     */
    q = fp->iqx.ip_next; /* -> top of reass. chain */
    ip->ip_end = ip->ip_off + ip->ip_len - 1;

    /* record the maximum fragment size for TCP */

    fp->iq_size = MAX(ip->ip_len, fp->iq_size);

    /* skip frags which new doesn't overlap at end */

    while ((q != (struct ip *)fp) && (ip->ip_off > q->ip_end))
	q = q->ip_next;

    if (q == (struct ip *)fp) 
    {	/* frag at end of chain */
	ip_enq(ip, fp->iqx.ip_prev);
    }
    else 
    {			/* frag doesn't overlap any */
	if (ip->ip_end < q->ip_off) 
	{
	    ip_enq(ip, q->ip_prev);

	    /* new overlaps beginning of next frag only */

	}
	else if (ip->ip_end < q->ip_end) 
	{
	    if ((i = ip->ip_end-q->ip_off+1) < ip->ip_len) 
	    {
		ip->ip_len -= i;
		ip->ip_end -= i;
		m_adj(mp, -i);
		ip_enq(ip, q->ip_prev);
	    }
	    else
		m_freem(mp);

	    /* new overlaps end of previous frag */

	}
	else 
	{
	    savq = q;
	    if (ip->ip_off <= q->ip_off) 
	    {

		/* complete cover */

		savq = q->ip_prev;
		ip_deq(q);
		m_freem(dtom(q));

	    }
	    else 
	    {	/* overlap */
		if ((i = q->ip_end-ip->ip_off+1) 
		    < ip->ip_len) 
		{
		    ip->ip_off += i;
		    ip->ip_len -= i;
		    m_adj(mp, i);
		}
		else
		    ip->ip_len = 0;
	    }

	    /* new overlaps at beginning of successor frags */

	    q = savq->ip_next;
	    while ((q != (struct ip *)fp) &&
		(ip->ip_len != 0) && 
		(q->ip_off <= ip->ip_end)) 

		/* complete cover */

		if (q->ip_end <= ip->ip_end) 
		{
		    struct ip *next;

		    next = q->ip_next;
		    ip_deq(q);
		    m_freem(dtom(q));
		    q = next;
		}
		else 
		{        /* overlap */
		    if ((i = ip->ip_end-q->ip_off+1) < ip->ip_len) 
		    {
			ip->ip_len -= i;
			ip->ip_end -= i;
			m_adj(mp, -i);
		    }
		    else
			ip->ip_len = 0;
		    break;
		}

	    /* enqueue whatever is left of new before successors */

	    if (ip->ip_len != 0) 
	    {
		ip_enq(ip, savq);
	    }
	    else
		m_freem(mp);
	}
    }

    /* check for completed fragment reassembly */

    if ((i = ip_done(&fp->iqx)) == 0)
	return(NULL);

    ip = fp->iqx.ip_next; /* -> top mbuf */
    ip->ip_len = i; /* total data length */
    *fragsize = fp->iq_size;	/* remember for TCP */
    /* option processing */

    if ((hlen = ip->ip_hl<<IP_HLSHIFT) > sizeof(struct ip))
	ip_stripopt (ip, hlen);

    ip_mergef(&fp->iqx); /* clean frag chain */

    /* copy src/dst internet address to header mbuf */

    ip->ip_src = fp->iqh.ip_src;
    ip->ip_dst = fp->iqh.ip_dst;

    ip_freef(fp); /* dequeue header */
    return(ip);
}

/*
 * Let people control gateway action by patching this:
 */
int	ip_forwarding = FALSE;

/*
 * Try to forward the packet.  Act like a gateway.
 */
ip_forward (fwdinp, ip, mp, hlen)
struct inpcb	*fwdinp;
register struct ip	*ip;
struct mbuf	*mp;
int hlen;
{
    register int		 type, code;
    register u_short	 len;
    register struct mbuf	*mcopy;
    register unsigned		 icmplen;
    int error;

    /*
     * Also copy forwarded packets, just like copy TCP/UDP/RDP...
     * packets sent to us so that can debug gateway action problems.
     * It's easy enough for the user-level program to filter these
     * out.
     */
    if (any_rawreaders())
	if (mcopy = m_copy(mp, 0, M_COPYALL))
	    raw_ip_input (mcopy);

    if (ip->ip_ttl)
	ip->ip_ttl --;

    len = ntohs(ip->ip_len);
    icmplen = MIN(len, hlen + ICMP_ERRLEN);

    if (ip_forwarding && ip->ip_ttl) 
    {
	/*
	 * Save chunk of ip packet in case there is an error
	 */
	mcopy = m_copy (mp, 0, (int)icmplen);

	/*
	 * The packet is not sourced by the local machine, so
	 * save ourselves a useless call to ip_route in ip_send().
	 * Also, don't want ip_send to work if sending from
	 * 8.7.0.2 to 192.1.11.1 via 8.0.0.16, and 8.0.0.16
	 * knows about a default gateway on net 8.  This can
	 * cause an ENETUNREACH if 192.1.11 is not a network
	 * that default gateway knows about.  [8.0.0.16 is on
	 * 192.1.11 and ip_route uses default gateway on net 8
	 * while rtalloc uses local interface]
	 *
	 * This route should be found by rtalloc, so let's do
	 * it here.
	 */
	if (fwdinp->inp_route.ro_rt == NULL) 
	{
	    /* Isn't a source routed packet */
	    struct rtentry *rt;
	    struct sockaddr_in *sin;

	    bzero ((caddr_t) fwdinp, sizeof(*fwdinp));
	    sin = (struct sockaddr_in *) &fwdinp->inp_route.ro_dst;
	    sin->sin_family = AF_INET;
	    sin->sin_addr = ip->ip_dst;
	    rtalloc (&fwdinp->inp_route);

	    /*
	     * Check to see if should send ICMP redirect.  Don't
	     * send redirect if source routing was used.
	     */
	    if ((rt = fwdinp->inp_route.ro_rt) != NULL) 
	    {
		sin = (struct sockaddr_in *) &rt->rt_gateway;
		if (! (rt->rt_flags & (RTF_GATEWAY|RTF_HOST)))
		    send_redirect (ip, ip->ip_dst, ICMP_REDIR_HOST, icmplen);
		else if (iptonet(ip->ip_src) == iptonet(sin->sin_addr))
		    send_redirect (ip, sin->sin_addr, ICMP_REDIR_NET, icmplen);
	    }
	}

	if (fwdinp->inp_route.ro_rt == NULL)
	{
	    /* no way to get there from here */
	    m_freem(mp);
	    type = ICMP_UNRCH;
	    code = ICMP_UNRCH_NET;
	}
	else 
	{
	    error = ip_send (fwdinp, mp, (int)len, TRUE);
	    rtfree(fwdinp->inp_route.ro_rt);
	    fwdinp->inp_route.ro_rt = NULL;

	    if (! error) 
	    {
		ipstat.ip_forwarded ++;
		if (mcopy)
		    m_freem(mcopy);
		return;
	    }

	    type = ICMP_UNRCH;
	    switch (error)
	    {
	      case ENETUNREACH:
	      case ENETDOWN:
		code = ICMP_UNRCH_NET;
		break;
	      case EMSGSIZE:
		code = ICMP_UNRCH_FRAG;
		break;
	      case EHOSTDOWN:
	      case EHOSTUNREACH:
		code = ICMP_UNRCH_HOST;
		break;

	      case ENOBUFS:
		type = ICMP_SRCQ;
		break;

	      default:
		log(LOG_INFO, "ip_forward: error %d\n", error);
	    }
	}
    }
    else 
    {
	if (ip->ip_ttl == 0) 
	{
	    type = ICMP_TIMEX;
	    code = ICMP_TIMEX_XMT;
	}
	else 
	{
	    type = ICMP_UNRCH;
	    code = ICMP_UNRCH_NET;
	}
	mcopy = mp;
	if (fwdinp->inp_route.ro_rt)
	{
	    /* was source routed by IP option */
	    rtfree (fwdinp->inp_route.ro_rt);
	    fwdinp->inp_route.ro_rt = NULL;
	}
    }

    if (mcopy)
    {
	ip = mtod(mcopy, struct ip *);
	ic_errmsg (redir_addr(ip), ip->ip_src, type, code, 0, icmplen, (char *) ip);
#ifdef HMPTRAPS
	/* hmp_trap(T_IP_ADDRS, (caddr_t) 0, 0); */
#else
	if (ip_forwarding)
	    /*
	     * If not acting as a gateway, don't want some one else's
	     * misconception to flood our console or logfile.  This error
	     * can be found through netstat an ip_drops.
	     */
	    ip_log(ip, "ip forwarding error");
	netlog(mcopy);
#endif
    }

    ipstat.ip_drops++;
}


/*
 * Check to see if fragment reassembly is complete
 */
ip_done(p)      
register struct ip *p;
{
    register struct ip *q;
    register next;

    q = p->ip_next;

    if (q->ip_off != 0)
	return(0);
    do
    {
	next = q->ip_end + 1;
	q = q->ip_next;
    }
    while ((q != p) && (q->ip_off == next));

    if ((q == p) && !(q->ip_prev->ip_mff))        /* all fragments in */
	return(next); /* total data length */
    else
	return(0);
}

/*
 * Merge mbufs of fragments of completed datagram 
 */
ip_mergef(p)    
register struct ip *p;
{
    register struct mbuf *m, *n;
    register struct ip *q;
    int dummy;

    q = p->ip_next; /* -> bottom of reass chain */
    n = (struct mbuf *)&dummy; /* dummy for init assignment */

    while (q != p) 
    {        /* through chain */
	/*
	 * If free mbuf holding q, cannot access q->ip_next in case
	 * that mbuf is used by device code for an incoming packet.
	 */
	register struct ip *next;

	next = q->ip_next;
	n->m_next = m = dtom(q);
	while (m != NULL) 
	{
	    if (m->m_len != 0) 
	    {
		n = m;
		m = m->m_next;
	    }
	    else /* free null mbufs */
		n->m_next = m = m_free(m);
	}
	q = next;
    }
}

/*
 * Dequeue and free reass.q header 
 */
ip_freef(fp)
register struct ipq *fp;
{
    if (fp->iq_prev != NULL)                               
	(fp->iq_prev)->iq_next = fp->iq_next;
    else 
	ipfrags.n_ip_head = fp->iq_next;

    if (fp->iq_next != NULL)                               
	(fp->iq_next)->iq_prev = fp->iq_prev;
    else
	ipfrags.n_ip_tail = fp->iq_prev;

    m_free(dtom(fp));
}

ip_stripopt (ip, hlen)
struct ip	*ip;
int hlen;
{
    int optlen;

    if ((optlen = (hlen - sizeof(struct ip))) > 0)
    {
	struct mbuf *m;
	caddr_t end_of_ip, end_of_opt;
	unsigned len;

	m = dtom(ip);
	end_of_ip = (char *) (ip +1);
	end_of_opt = end_of_ip + optlen;
	len = m->m_len - hlen;
	bcopy (end_of_opt, end_of_ip, len);
	m->m_len -= optlen;
    }
}

/*
 * Process ip options 
 * FALSE -> options were in error, and an icmp message has been sent
 */
#define OFF_OLEN	1
#define OFF_OFFSET	2
#define MIN_OFF		4	/* since option is a 1, not 0, based array */

/*
 * Record route in same form as ip_setopt()
 */
save_rte (option, dst)
u_char	*option;
struct in_addr dst;
{
    int olen;
    int off;
    u_char	*x;
    struct in_addr *p, *q;

    if (ip_nhops != 0)
    {
	/* Use both loose and strict source routing? */
	log(LOG_INFO, "ip_nhops %d\n", ip_nhops);
	ip_nhops = 0;
	return;
    }
    olen = option[OFF_OLEN];
    if (olen > sizeof(ip_hops))
    {
	log(LOG_INFO, "save_rte: olen %d\n", olen);
	return;
    }
    off = option[OFF_OFFSET];
    p = (struct in_addr *) (&option[off - 1]);
    q = (struct in_addr *) (&option[MIN_OFF -1]);

    x = (u_char *) ip_hops;
    x[0] = IP_NOP_OPT;
    x[1] = option[0];	/* loose/strict source routing */
    x[2] = (p - q) * sizeof(struct in_addr) + 3;
    x[3] = MIN_OFF;
    ip_nhops ++;	/* = 1 (1 long for opt hdr) */
    p--;	/* p points at first hop for return route */

    ip_hops[p-q+2] = (*p);	/* save first hop after return route option */
    p--;	/* it is in what will be ip_hops[ip_nhops]  */

    /* record return path as an IP source route */
    while (p >= q)
    {
	ip_hops[ip_nhops] = (*p);
	p--;
	ip_nhops ++;
    }
    /* remember eventual destination is in the option field */
    ip_hops[ip_nhops] = dst;
    ip_nhops ++;
}

ip_opt (ip, hlen, fwdinp)
register struct ip *ip;
struct inpcb *fwdinp;
{
    register u_char	*endopt;
    register u_char	*option;
    register int	 olen;
    register int	 off;
    int type;
    int code;
    struct in_addr nexthop;
    struct in_ifaddr *ia;

    endopt = ((u_char *) ip) + hlen;
    option = (u_char *) (ip +1);

    while (option < endopt)
    {
	switch (*option)
	{
	  case IP_END_OPT:
	    return (TRUE);

	  case IP_NOP_OPT:
	    olen = sizeof(u_char);
	    break;

	  case IP_SEC_OPT:
	    olen = option[OFF_OLEN];
	    /* so much for security */
	    break;

	  case IP_LRTE_OPT:
	    olen = option[OFF_OLEN];
	    off = option[OFF_OFFSET];
	    if (off < MIN_OFF)
	    {
		type = ICMP_PARM;
		code = &option[OFF_OFFSET] - ((u_char *) ip);
		goto err;
	    }
	    off--;	/* adjust for use by C */
	    if (in_iawithaddr(ip->ip_dst, TRUE) == NULL)
		/*
		 * With loose routing, may take a few hops
		 * to get to current nexthop.
		 */
		break;

	    if (off > (olen - sizeof(struct in_addr)))
	    {
		/* hints all used up. pkt for us */
		save_rte (option, ip->ip_src);
		break;
	    }

	    nexthop = *((struct in_addr *) (option + off));

	    /*
	     * record outgoing interface
	     */
	    if (! ip_opt_route(fwdinp, nexthop))
	    {
		type = ICMP_UNRCH;
		/* net? frag? host? */
		code = ICMP_UNRCH_SRC;
		goto err;
	    }
	    ip->ip_dst = nexthop;
	    option[OFF_OFFSET] += sizeof(struct in_addr);
	    if (fwdinp->inp_route.ro_rt == NULL)
		/*
		 * Destined for ourselves, and we're
		 * just going to strip the options off
		 */
		break;
	    *((struct in_addr *) (option + off)) =
		IA_INADDR(in_iafromif (fwdinp->inp_route.ro_rt->rt_ifp));
	    break;

	  case IP_SRTE_OPT:
	    olen = option[OFF_OLEN];
	    off = option[OFF_OFFSET];
	    if (off < MIN_OFF)
	    {
		type = ICMP_PARM;
		code = &option[OFF_OFFSET] - ((u_char *) ip);
		goto err;
	    }
	    off--;	/* adjust for use by C */
	    if (in_iawithaddr(ip->ip_dst, TRUE) == NULL)
	    {
		/* strict path -> someone goofed */
		/* should have come in on us for us */
		type = ICMP_UNRCH;
		code = ICMP_UNRCH_SRC;
		goto err;
	    }

	    if (off > (olen - sizeof(struct in_addr)))
	    {
		/* hints all used up */
		save_rte (option, ip->ip_src);
		break;
	    }

	    nexthop = *((struct in_addr *) (option + off));

	    if ((ia = in_iawithnet(nexthop)) == NULL)
	    {
		/* strict path -> someone goofed
		 * we should be directly connected to
		 * next hop
		 */
		type = ICMP_UNRCH;
		code = ICMP_UNRCH_SRC;
		goto err;
	    }
	    *((struct in_addr *) (option + off)) = IA_INADDR(ia);

	    ip->ip_dst = nexthop;

	    option[OFF_OFFSET] += sizeof(struct in_addr);
	    break;

	  case IP_TIME_OPT:
	    olen = option[OFF_OLEN];
	    off = option[OFF_OFFSET];
	    if (off < MIN_OFF)
	    {
		type = ICMP_PARM;
		code = &option[OFF_OFFSET] - ((u_char *) ip);
		goto err;
	    }
	    off--;	/* adjust for use by C */
	    if (off > (olen - sizeof(u_long)))
	    {
		/* increment overflow count */
		if ((option[3] & 0xf0) == 0xf0)
		{
		    /* overflow overflowed */
		    type = ICMP_PARM;
		    code = &option[OFF_OLEN] - ((u_char *) ip);
		    goto err;
		}
		option[3] += 0x10;
		break;
	    }

	    if (option[3] & 2)
	    {
		/* want specific host to stamp */
		ia = in_iawithaddr(*((struct in_addr *) (option + off)), FALSE);
		if (ia == NULL)
		    break;
	    }
	    else
		ia = in_ifaddr;

	    if (option[3] & 1)
	    {
		/* record stamping host */
		*((struct in_addr *) (option + off)) = IA_INADDR (ia);
		off += sizeof(struct in_addr);
		option[OFF_OFFSET] += sizeof(struct in_addr);
	    }
	    if (off > (olen - sizeof(u_long)))
	    {
		option[3] += 0x10;
		break;
	    }
	    *((u_long *) (option + off)) = iptime();
	    option[OFF_OFFSET] += sizeof(u_long);
	    break;

	  case IP_STRID_OPT:
	    olen = option[OFF_OLEN];
	    break;

	  case IP_RRTE_OPT:
	    olen = option[OFF_OLEN];
	    off = option[OFF_OFFSET];
	    if (off < MIN_OFF)
	    {
		type = ICMP_PARM;
		code = &option[OFF_OFFSET] - ((u_char *) ip);
		goto err;
	    }
	    off--;	/* adjust for use by C */
	    if (off > (olen - sizeof(u_long)))
		/* no space left for recording route */
		break;

	    /* record outgoing interface */
	    if (! ip_opt_route(fwdinp, ip->ip_dst))
	    {
		type = ICMP_UNRCH;
		code = ICMP_UNRCH_NET;
		goto err;
	    }
	    option[OFF_OFFSET] += sizeof(struct in_addr);
	    if (fwdinp->inp_route.ro_rt == NULL)
		/*
		 * Destined for us, and we're just
		 * going to strip options off
		 */
		break;
	    *((struct in_addr *) (option + off)) =
		IA_INADDR(in_iafromif (fwdinp->inp_route.ro_rt->rt_ifp));
	    break;
	}

	option += olen;
    }
    return (TRUE);

err :
    ic_errmsg (icmp_addr(ip), ip->ip_src,
	type, code, option - ((u_char *) ip), hlen, (char *) ip);
    return (FALSE);
}
#undef OFF_OLEN
#undef OFF_OFFSET
#undef MIN_OFF


ip_opt_route (fwdinp, dst)
register struct inpcb *fwdinp;
struct in_addr dst;
{
    register struct sockaddr_in *sin;

    /* in case they use several options involving routing */
    if (fwdinp->inp_route.ro_rt)
	return (TRUE);

    bzero ((caddr_t) fwdinp, sizeof(*fwdinp));

    sin = (struct sockaddr_in *) &fwdinp->inp_route.ro_dst;

    /* not sure ip_send cares about this stuff ... */
    sin->sin_family = AF_INET;
    sin->sin_addr = dst;

    /* Don't allocate route if not forwarding packet.
     * This saves us from doing a check in ip_input() to see
     * if we should do a rtfree() for an uncommon occurrence.
     */

    if (in_iawithaddr(dst, TRUE) != NULL)
	return (TRUE);

    rtalloc (&fwdinp->inp_route);
    return (fwdinp->inp_route.ro_rt != NULL);
}


/*
 * IP fragment reassembly timeout routine.
 */
ip_timeo()      
{
    register struct ip *p, *q;
    register struct ipq *fp, *next;
    register int s;

    static int timflag;

    /* check once per sec */

    if (timflag = !timflag)	/* looks strange, doesn't it? */
	return;


    /* search through reass.q */
    s = splnet();
    for (fp = ipfrags.n_ip_head; fp != NULL; fp = next) 
    {
	/*
	 * If fragment times out, mbufs are freed, and can't
	 * use next pointer since mbuf may be grabbed by
	 * an interface at splimp
	 */
	next = fp->iq_next;

	if (--(fp->iqx.ip_ttl) == 0) 
	{  /* time to die */

	    q = fp->iqx.ip_next; /* free mbufs assoc. w/chain */
	    while (q != (struct ip *)fp) 
	    {
		p = q->ip_next;
		/* ### generate timed out in reassembly msg */
#ifdef HMPTRAPS
		/* hmp_trap(T_IP_FDROP, (caddr_t)0,0); */
#else
		m_freem(dtom(q));
#endif
		q = p;
	    }
	    ip_freef(fp); /* free header */              
	}
    }
    splx(s);
}

/*
 * Called at splimp from uipc_mbuf.c
 * Network code needs to free up space!  IP fragments dropped.
 */
ip_drain()
{
    register struct ip *p, *q;
    register struct ipq *fp;

    while (fp = ipfrags.n_ip_head)
    {
	q = fp->iqx.ip_next;	/* free mbufs assoc w/chain */
	while (q != (struct ip *)fp) 
	{
	    p = q->ip_next;
	    m_freem(dtom(q));
	    q = p;
	}
	ip_freef(fp); /* free header */              
    }
}

#include "../h/syslog.h"

inet_cksum_err (protoname, ip, was, should_be)
char *protoname;
struct ip *ip;
u_long was;
u_long should_be;
{
    union { u_long ul; u_char c[4]; } s, d;

    s.ul = ip->ip_src.s_addr;
    d.ul = ip->ip_dst.s_addr;
    log (LOG_INFO,
	"%s checksum was 0x%x not 0x%x src %d.%d.%d.%d dst %d.%d.%d.%d\n",
	protoname, was, should_be,
	s.c[0], s.c[1], s.c[2], s.c[3],
	d.c[0], d.c[1], d.c[2], d.c[3]);
}

ip_log(ip, emsg)
struct ip *ip;
char *emsg;
{
    union { u_long ul; u_char c[4]; } s, d;

    s.ul = ip->ip_src.s_addr;
    d.ul = ip->ip_dst.s_addr;
    log(LOG_INFO, "%s: src %d.%d.%d.%d dst %d.%d.%d.%d\n",
	emsg,
	s.c[0], s.c[1], s.c[2], s.c[3],
	d.c[0], d.c[1], d.c[2], d.c[3]);
}

no_route (msg, from, to)
char	*msg;
struct in_addr	 from;
struct in_addr	 to;
{
    union { u_long ul; u_char c[4]; } f, t;

    f.ul = from.s_addr;
    t.ul = to.s_addr;
    log(LOG_INFO, "%s: no route %d.%d.%d.%d -> %d.%d.%d.%d\n",
	msg,
	f.c[0], f.c[1], f.c[2], f.c[3],
	t.c[0], t.c[1], t.c[2], t.c[3]);
}
