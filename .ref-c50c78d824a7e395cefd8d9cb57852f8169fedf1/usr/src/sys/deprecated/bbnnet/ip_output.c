#ifdef	RCSIDENT
static char rcsident[] = "$Header: ip_output.c,v 1.28 85/07/31 09:32:09 walsh Exp $";
#endif

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/protosw.h"
#include "../h/domain.h"
#include "../h/ioctl.h"
#include "../h/syslog.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../bbnnet/in.h"
#include "../bbnnet/net.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/icmp.h"

/*
 * If you're going to a specific host or via a gateway, the routing
 * entry gateway field holds the best way to get there.  Otherwise,
 * the routing entry tells you how to get onto that net -- it has
 * the net address portion of our local host:
 *
 * On bbn-labs-b:
 *
 *		rt_dst		rt_gateway	flags
 * il0  => 	0x00000b80	0x2010b80	UP
 * imp0 =>	0x00000008	0x2000708	UP
 * loop =>	0x0000007f	0x100007f	UP
 *
 * So you can see that the rt_gateway is our local address, and the
 * rt_dst may be the net number of the media.  If it's a route
 * to a net, the other guy is on this net and you want to route the
 * packet to him anyway.
 *
 * gateway	0               0x1000b80       UP, RTF_GATEWAY
 */

#define IF_SEND(ifp, mp, rt, retval) \
{\
    static struct sockaddr_in tmproute = {AF_INET};                           \
\
    if (! ((ifp)->if_flags & IFF_UP)){					      \
	/* goes with PRC_IFDOWN in in.c */				      \
	m_freem(mp);							      \
	retval = ENETUNREACH;						      \
    } else if ((rt)->rt_flags & (RTF_GATEWAY|RTF_HOST))                       \
	retval = (*(ifp)->if_output)(ifp, mp, &(rt)->rt_gateway);             \
    else {                                                                    \
	tmproute.sin_addr.s_addr = (mtod(mp, struct ip *))->ip_dst.s_addr;    \
	retval = (*(ifp)->if_output)(ifp, mp, (struct sockaddr *) &tmproute); \
}}

if_send(ifp, mp, rt)
register struct ifnet *ifp;
register struct mbuf *mp;
register struct rtentry *rt;
{
    int	retval;

    IF_SEND (ifp, mp, rt, retval);
    return (retval);
}


/*
 * Find a route to this destination.  Given the source and destination 
 * addresses, it returns a local net address
 * to send to (either the address of the destination itself or a gateway).
 * Taken mostly from rtalloc;  expanded to route according to
 * both ends of the connection.
 */


struct rtentry *ip_route(src, dst)
struct in_addr *src;
struct in_addr *dst;
{
    register struct rtentry *rt;
    register struct mbuf *m;
    register unsigned hash;
    net_t snet, dnet;
    int doinghost;
    struct rtentry *rtmin;
    struct mbuf **table;
    static struct in_addr wildcard;

    /* get network parts of src and dest addresses */

    snet = iptonet(*src);
    dnet = iptonet(*dst);

    rtmin = NULL;
    hash = HOSTHASH(dst->s_addr);
    table = rthost;
    doinghost = TRUE;
again :
    for (m = table[hash % RTHASHSIZ]; m; m = m->m_next) 
    {
	rt = mtod(m, struct rtentry *);
	if (rt->rt_hash != hash)
	    continue;
	if (! (rt->rt_flags & RTF_UP))
	    continue;
	if (! (rt->rt_ifp->if_flags & IFF_UP))
	    continue;
	if (rt->rt_dst.sa_family != AF_INET)
	    continue;

	/* packets go out an interface with our local IP address */
	if (iptonet(((struct sockaddr_in *)&(rt->rt_gateway))->sin_addr) != snet)
	    continue;

	/* does this route get us there? */
	if (doinghost) 
	{
	    if (((struct sockaddr_in *)&(rt->rt_dst))->sin_addr.s_addr !=
		dst->s_addr)
		continue;
	}
	else 
	{
	    /*
	     * iptonet == 0 => smart gateway (route to anywhere)
	     * iptonet != 0 => gateway to another net (route to net)
	     */
	    if (iptonet(((struct sockaddr_in *)&(rt->rt_dst))->sin_addr) != dnet)
		continue;
	}

	/* and try to share load across gateways */
	if (rtmin == NULL)
	    rtmin = rt;
	else if (rt->rt_use < rtmin->rt_use)
	    rtmin = rt;
    }

    if (rtmin == NULL)
    {
	if (doinghost) 
	{
	    doinghost = FALSE;
	    hash = NETHASH(*dst), table = rtnet;
	    goto again;
	}
	/*
	 * Check for wildcard gateway, by convention network 0.
	 */
	if (dst != &wildcard) 
	{
	    hash = 0;
	    dst = &wildcard;
	    dnet = 0;
	    goto again;
	}
	rtstat.rts_unreach++;
	return(NULL);
    }

    rtmin->rt_refcnt++;
    if (dst == &wildcard)
	rtstat.rts_wildcard++;
    return(rtmin);
}


/*
 * Ip_send is called from the higher protocol layer (TCP/RDP/UDP) and is passed
 * an mbuf chain containing a packet to send to the local network.  The first
 * mbuf contains the protocol header and an IP header which is partially
 * filled in.  After determining a route (outgoing interface + first hop) for
 * the packet, it is fragmented (if necessary) and sent to the local net 
 * through the local net send routine.
 *
 * For non-raw output, caller should have stuffed:
 *	ip protocol type, type of service, source addr, destin addr
 *
 * ip_tos is left to caller so that people using raw sockets can do whatever
 * they please.  (They don't have an inpcb in which to store such info.)
 *
 * The asis argument is TRUE for raw output and the gateway (packet forwarding)
 * code.  It indicates that the IP header is fully constructed.
 *
 * Errors at the IP layer and below occur synchronously, and can be reported
 * back via subroutine return values.  Higher level protocols should remember
 * that if they do things asynchronous to a system call (ie., packet
 * retransmission) that they should post error back to user via advise_user()
 * so that user gets error next time he rendezvous with the kernel.
 */
ip_send(inp, mp, len, asis)
struct inpcb *inp;
register struct mbuf *mp;
register int len;
int asis;
{
    register struct ip *p;
    register struct ifnet *ifp;
    register struct rtentry *rt;
    register int hlen;
    int free_route = FALSE;
    int retval;

    p = mtod(mp, struct ip *);	/* -> ip header */
    /*
     * Find route for datagram if one has not been assigned.
     */
    if ((rt = inp->inp_route.ro_rt) == NULL) 
    {
	if ((rt = ip_route(&p->ip_src, &p->ip_dst)) == NULL) 
	{
	    if (asis || (p->ip_src.s_addr == INADDR_ANY)) 
	    {
		/*
		 * asis: forwarding a packet not sourced by us
		 *      eg., by raw interface and user level repeater process
		 * INADDR_ANY: sending icmp packet for which
		 *      we're trying to avoid routing twice.
		 */
		struct route tmproute;
		struct sockaddr_in *sin;

		bzero ((caddr_t) &tmproute, sizeof(tmproute));
		sin = (struct sockaddr_in *) &tmproute.ro_dst;
		sin->sin_family = AF_INET;
		sin->sin_addr.s_addr = p->ip_dst.s_addr;
		rtalloc (&tmproute);
		rt = tmproute.ro_rt;

		if (rt && (p->ip_src.s_addr == INADDR_ANY))
		    p->ip_src = IA_INADDR(in_iafromif(rt->rt_ifp));
	    }

	    if (rt == NULL) 
	    {
		m_freem(mp);
		return(ENETUNREACH);
	    }
	}
	free_route = TRUE;
    }
    ifp = rt->rt_ifp;

    /*
     * Copy ip source route to header.  Know asis must be FALSE, if do.
     */
    if (inp->inp_optlen > 0) 
    {
	char	*q;

	if (mp->m_off - inp->inp_optlen >= MMINOFF)
	{
	    struct in_addr *ipa;

	    mp->m_off -= inp->inp_optlen;
	    mp->m_len += inp->inp_optlen;
	    q = (char *) p;
	    p = (struct ip *) (q - inp->inp_optlen);
	    bcopy(q, (caddr_t)p, sizeof(struct ip));
	    bcopy(inp->inp_options, (caddr_t)(p+1), (unsigned)inp->inp_optlen);
	    /*
	     * And replate eventual destination with first hop.
	     * Eventual destination is in source route just
	     * copied in.
	     */
	    ipa = (struct in_addr *) (&inp->inp_options[0]);
	    p->ip_dst = ipa[inp->inp_optlen/sizeof(struct in_addr)];
	}
	else
	    log(LOG_INFO, "ip_send: optlen %d inpcb 0x%x\n",
		(int)inp->inp_optlen, inp);
    }

    /*
     * fill in ip header fields
     */
    if (asis) 
    {
	/*
	 * RAW OUTPUT.  Must get len, hlen, off from packet header.
	 * Byte swap is ugly (since we must swap back below), but
	 * necessary in case we must fragment.
	 */
	hlen = p->ip_hl << IP_HLSHIFT;
	len = ntohs(p->ip_len);
	p->ip_off = ntohs(p->ip_off);
    }
    else 
    {
	static u_short next_ip_id; /* some day RDP may want to force for rxmit */

	hlen = sizeof(struct ip) + inp->inp_optlen;
	len += hlen;
	p->ip_v = IPVERSION;
	p->ip_hl = hlen >> IP_HLSHIFT;
	p->ip_off = 0;
	p->ip_ttl = MAXTTL; /* ### should come from route */
	p->ip_id = htons(next_ip_id++);
    }

    /*
     * let ip_frag do the send if needed, otherwise do it directly.
     */

    /* for testing IP reassembly code */
#ifdef FORCE_FRAG
#define MTU(ifp) (((ifp)->if_mtu >> FORCE_FRAG) & (~3))
#else
#define MTU(ifp) (ifp)->if_mtu
#endif

    if (len > MTU(ifp)) 
    {
	p->ip_len = len;
	retval = ip_frag(p, ifp, rt, hlen);
    }
    else 
    {
	/*
	 * complete header, byte swap, and send to local net 
	 */
	p->ip_len = htons((u_short)len);
	p->ip_off = htons(p->ip_off);
	/*
	 * No reason not to have kernel checksum, even for raw packets.
	 */
	p->ip_sum = 0;
	p->ip_sum = in_cksum(dtom(p), hlen);
	IF_SEND (ifp, mp, rt, retval);
    }

    rt->rt_use ++;	/* Yet another IP packet sent away */

    if (free_route) 
    {
	struct socket *so;

	if ((so = inp->inp_socket) &&
	    (so->so_proto->pr_flags & PR_CONNREQUIRED))
	    /*
	     * Found a new route after old one pinged out.
	     */
	    inp->inp_route.ro_rt = rt;
	else
	    rtfree(rt);
    }

    return(retval);
}

/*
 * Ip_frag is called with a packet with a completed ip header 
 * (except for checksum).  It fragments the packet, inserts the
 * IP checksum, and calls the appropriate local net output routine
 * to send it to the net.
 *
 * Previously, when there was only one kind of mbuf, it tried to
 * reduce space requirements by recycling the chain to be fragmented.
 * Preserving this approach is overly complicated, and should mbufs
 * change again, cause problems.  Therefore, have switched to copying
 * the chain to be fragmented.
 */
ip_frag(p, ifp, rt, hlen)
register struct ip *p;
struct ifnet *ifp;
struct rtentry *rt;
register int hlen;
{
    register struct mbuf *m;	/* original chunk */
    register struct mbuf *mhdr;	/* fragment */
    register struct ip *fip;	/* the fragment IP header */
    int off;	/* offset into entire IP datagram */
    int here;	/* offset into this chunk of it */
    register int len;	/* length of data in this chunk */
    int flags;	/* of this chunk to fragment */
    int max;	/* max data length in a fragment */
    int fdlen;	/* actual fragment data length */
    int error;

    m = dtom(p);

    if (p->ip_off & ip_df) 
    {	/* can't fragment */
	m_freem(m);
	return(EMSGSIZE);
    }
    max = MTU(ifp) - hlen;	/* max data length in frag */
    len = p->ip_len - hlen;	/* data length */

    /* 
     * this only needs to be this complicated if we are handed
     * an already-fragmented packet
     */
    flags	= p->ip_off&(ip_mf|ip_df);	/* save old flags */
    p->ip_off &= ~flags;	/* take them out of ip_off */
    off	= p->ip_off << IP_OFFSHIFT;	/* fragment offset */
    here	= hlen;
    error	= 0;

    while (len > 0) 
    {
	/*
	 * Allocate mbuf for fragment IP header
	 */
	mhdr = m_get(M_DONTWAIT, MT_HEADER);
	if (mhdr == NULL)
	{
	    error = ENOBUFS;
	    break;
	}
	/*
	 * get copy of data for fragment
	 */
	if (len < max)
	    fdlen = len;
	else
	    fdlen = max & (~7); /* 7 == 2^IP_OFFSHIFT -1 */
	mhdr->m_next = m_copy(m, here, fdlen);
	if (mhdr->m_next == NULL)
	{
	    m_free(mhdr);
	    error = ENOBUFS;
	    break;
	}
	/*
	 * build the header for this fragment and ship it off.
	 */
	mhdr->m_len = hlen;
	mhdr->m_off = MMAXOFF - hlen;
	fip = mtod(mhdr, struct ip *);
	bcopy((caddr_t)p, (caddr_t)fip, (unsigned)hlen);
	fip->ip_off = off >> IP_OFFSHIFT;
	if (fdlen >= len)
	    /* it's the last fragment */
	    fip->ip_off |= flags;
	else
	    fip->ip_off |= ip_mf;
	fip->ip_off = htons((u_short)fip->ip_off);
	fip->ip_len = htons((u_short)fdlen + hlen);
	fip->ip_sum = 0;
	fip->ip_sum = in_cksum(mhdr, hlen);
	if (error = if_send (ifp, mhdr, rt))
	    break;

	/*
	 * and get ready for next pass through the loop
	 */
	len	-= fdlen;
	off	+= fdlen;
	here	+= fdlen;
    }

    m_freem(m);
    return (error);
}

/*
 * Current connection should use a new path.
 */
struct rtentry *ip_reroute(inp)
register struct inpcb *inp;
{
    register struct route *ro = &inp->inp_route;

    rtfree(ro->ro_rt);
    return(ro->ro_rt = ip_route(&inp->inp_laddr, &inp->inp_faddr));
}

/*
 * A gateway has gone down.  Change route used by all connections currently
 * using it.
 */
ip_gdown(addr)
u_long	addr;
{
    register struct protosw *psw;

    for(psw=inetdomain.dom_protosw; psw < inetdomain.dom_protoswNPROTOSW; psw++)
	if (psw->pr_type != SOCK_RAW)
		if (psw->pr_ctlinput)
			(*(psw->pr_ctlinput)) (PRC_GWDOWN, addr);
}

/*
 * Called from protocol ctlinput routine.  This way, IP/ICMP don't need to know
 * about protocol's head of inpcbs... for all the protocols.
 */
in_gdown (head, addr)
register struct inpcb *head;
u_long addr;
{
    register struct inpcb   *inp;
    register struct rtentry *rt;

    if (head == NULL)
	return;

    for(inp = head->inp_next; inp != head; inp = inp->inp_next)
    {
	if (rt = inp->inp_route.ro_rt)
	{
	    if (rt->rt_flags & RTF_GATEWAY)
	    {
		if (((struct sockaddr_in *) &rt->rt_gateway)->sin_addr.s_addr == addr)
		{
			/*
			 * Don't remove route permanently, since want to catch
			 * the gateway when it reboots:
			 *      -- rtrequest (SIOCDELRT, rt) --
			 *
			 * make sure rtfree() not remove route mbuf
			 * incrementing reference count here, and decrementing
			 * when timeout on reinstatement goes off.  Cannot call
			 * rtfree with zero reference count when have not done
			 * SIOCDELRT.
			 */
			if (rt->rt_flags & RTF_UP)
			{
			    rt->rt_flags &= ~RTF_UP;
			    rt->rt_flags |= RTF_REINSTATE;
			    rt->irt_gdown = RT_REINSTATE;
			    rt->rt_refcnt ++;
			}

			if (!ip_reroute(inp))
			    advise_user(inp->inp_socket, ENETUNREACH);

		}
	    }
	}
    }
}
