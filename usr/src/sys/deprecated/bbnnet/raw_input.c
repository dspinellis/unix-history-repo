#ifdef	RCSIDENT
static char rcsident[] = "$Header: raw_input.c,v 1.17 85/07/31 09:33:16 walsh Exp $";
#endif

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../machine/mtpr.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"
#include "../net/raw_cb.h"

#include "../bbnnet/in.h"
#include "../bbnnet/net.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/fsm.h"
#include "../bbnnet/tcp.h"
#include "../bbnnet/icmp.h"
#include "../bbnnet/udp.h"
#ifdef HMP
#include "../bbnnet/hmp.h"
#endif HMP
#include "../bbnnet/nopcb.h"

/*
 * Sort INET packets for user(s).  To get a packet, socket must match:
 *
 *    raw_ip_proto
 *		domain   (INET)
 *		protocol (TCP/UDP/ICMP)
 *    raw_ip_dst
 *		domain   (INET)
 *		address, but not port, if connected(2)
 *    raw_ip_src
 *		domain   (INET)
 *		address, but not port, if bound(2)
 *
 * Called from ip_input() for packets that were well-formed enough to get
 * passed up to TCP/UDP/ICMP.
 */
struct	sockaddr_in raw_ip_dst = 
{
    AF_INET 
} ;
struct	sockaddr_in raw_ip_src = 
{
    AF_INET 
} ;
struct	sockproto raw_ip_proto = 
{
    PF_INET 
} ;

raw_ip_input(m)
struct mbuf *m;
{
    register struct ip *ip = mtod(m, struct ip *);

    raw_ip_proto.sp_protocol = ip->ip_p;
    raw_ip_dst.sin_addr.s_addr = ip->ip_dst.s_addr;
    raw_ip_src.sin_addr.s_addr = ip->ip_src.s_addr;

    raw_input (m, &raw_ip_proto, (struct sockaddr *)&raw_ip_src,
	(struct sockaddr *)&raw_ip_dst);
}

/*
 * Bad ip packets, which are taken care of via calls to ip_log() and netlog().
 */
struct	sockaddr_in netlog_dst = 
{
    AF_INET 
} ;
struct	sockaddr_in netlog_src = 
{
    AF_INET 
} ;
struct	sockproto netlog_proto = 
{
    PF_INET, NETLOG_PROTO 
} ;

netlog(m)
struct mbuf *m;
{
    raw_input (m, &netlog_proto, (struct sockaddr *)&netlog_src,
	(struct sockaddr *)&netlog_dst);
}

#ifdef AF_TCPDEBUG
	/*
	 * TCP debugging log
	 *
	 * Though the mbuf contains a copy of the tcpcb, and thus a pointer to the
	 * inpcb, kernel can't do address sorting since this may point within a freed
	 * (and perhaps now recycled) mbuf.  Remember that this logging is done after
	 * state changes (closing).
	 */
	struct	sockaddr_in tcpdebug_dst = 
	{
	    AF_TCPDEBUG 
	} ;
	struct	sockaddr_in tcpdebug_src = 
	{
	    AF_TCPDEBUG 
	} ;
	struct	sockproto tcpdebug_proto = 
	{
	    PF_TCPDEBUG, 0 
	} ;

	tcpdebuglog(m)
	struct mbuf *m;
	{
	    raw_input (m, &tcpdebug_proto, (struct sockaddr *)&tcpdebug_src,
		(struct sockaddr *)&tcpdebug_dst);
	}
#else
	tcpdebuglog(m)
	struct mbuf *m;
	{
	    m_freem(m);
	}
#endif

#ifdef AF_RDPDEBUG
	struct sockaddr_in rdpdebug_dst = 
	{
	    AF_RDPDEBUG 
	} ;
	struct sockaddr_in rdpdebug_src = 
	{
	    AF_RDPDEBUG 
	} ;
	struct sockproto rdpdebug_proto = 
	{
	    PF_RDPDEBUG, 0 
	} ;

	rdpdebuglog(m)
	struct mbuf *m;
	{
	    raw_input (m, &rdpdebug_proto, (struct sockaddr *)&rdpdebug_src,
		(struct sockaddr *)&rdpdebug_dst);
	}
#else
	rdpdebuglog(m)
	struct mbuf *m;
	{
	    m_freem(m);
	}
#endif


struct mbuf *m_bpullup(m0, len)
struct mbuf *m0;
int len;
{
    register struct mbuf *m, *n;
    unsigned count;

    n = m0;
    if (len > MLEN)
    {
	m_freem(n);
	return ((struct mbuf *) NULL);
    }
#ifdef MBUF_DEBUG
    m = m_get(M_DONTWAIT, n->m_type);
#else
    MGET(m, M_DONTWAIT, n->m_type);
#endif
    if (m == 0)
    {
	m_freem(n);
	return ((struct mbuf *) NULL);
    }
    m->m_len = 0;
    m->m_off = MMAXOFF - len;	/* -- difference from m_pullup -- */
    do
    {
	count = MIN(len, n->m_len);
	bcopy(mtod(n, caddr_t), mtod(m, caddr_t)+m->m_len, count);
	len -= count;
	m->m_len += count;
	n->m_off += count;
	n->m_len -= count;
	if (n->m_len)
	    break;
	n = m_free(n);
    }
    while (n);

    if (len) 
    {
	(void) m_free(m);
	m_freem(n);
	return ((struct mbuf *) NULL);
    }
    m->m_next = n;
    return (m);
}

/*
 * output function called from net/raw_usrreq
 */

/* ARGSUSED */
raw_ip_output (m0, so)
struct mbuf *m0;
struct socket *so;
{
    register struct mbuf	*m;
    register struct ip 	*ip;
    register int		 len;
    int retval;

    /*
     * verify length of datagram, get IP header at end of mbuf so can
     * prepend local net header.
     */
    len = 0;
    for (m = m0 ; m ; m = m->m_next)
	len += m->m_len;
    if (len < sizeof(struct ip))
    {
	m_freem(m0);
	return(EMSGSIZE);	/* ### */
    }
    if ((m = m_bpullup(m0, sizeof(struct ip))) == NULL)
	return (ENOBUFS);
    ip = mtod(m, struct ip *);
    if ((ntohs((u_short)ip->ip_len) != len) ||
	((ip->ip_hl << IP_HLSHIFT) > len)) 
    {
	m_freem(m);
	return(EMSGSIZE);	/* ### */
    }

#ifdef notdef
    /* have to be super-user anyway to do this.
     * Cronus wants to be able to forward broadcast UDP packets.
     */

    /*
     * verify that addresses are valid
     */
    if (in_broadcast(ip->ip_src) || (in_iawithaddr(ip->ip_src, TRUE) == 0))
#else
    if (in_broadcast(ip->ip_src))
#endif
    {
	m_freem(m);
	return (EADDRNOTAVAIL);
    }

    NOPCB_IPSEND (m, len, TRUE, retval);
    return (retval);
}



/*
 * Send out an icmp packet.  Use the user's ICMP header, and our own IP
 * header.
 */
/* ARGSUSED */
raw_icmp_output (m0, so)
struct mbuf	*m0;
struct socket	*so;
{
    register struct mbuf  *m;
    register struct icmp  *p;
    register struct ip    *ip;
    register struct rawcb *rcb;
    int len;
    int retval;

    rcb = sotorawcb(so);
    if (!(rcb->rcb_flags & RAW_FADDR))
    {
	m_freem(m0);
	return(EDESTADDRREQ);
    }

    /*
     * find length of datagram
     */
    len = 0;
    for (m = m0 ; m ; m = m->m_next)
	len += m->m_len;
    if (len < ICMPSIZE)
    {
	m_freem(m0);
	return (EMSGSIZE);	/* ### */
    }

    /*
     * Pull up user's ICMP header so we can prepend IP header later.
     */
    if ((m = m_bpullup(m0, ICMPSIZE)) == NULL)
	return (ENOBUFS);

    /*
     * "Verify" ICMP header.  Accept user's type and code.
     */
    p = mtod(m, struct icmp *);

    /*
     * Use our own checksum, though.  It'll be at least as fast as the
     * user's and we'll have to use those CPU cycles sometime.
     */
    p->ic_sum = 0;
    p->ic_sum = in_cksum(m, len);

    /*
     * Fill in IP header and send it
     */
    m->m_off -= sizeof(struct ip);
    m->m_len += sizeof(struct ip);
    ip = mtod(m, struct ip *);
    ip->ip_p	= IPPROTO_ICMP;
    ip->ip_tos	= 0;
    ip->ip_dst.s_addr = 
	((struct sockaddr_in *) &rcb->rcb_faddr)->sin_addr.s_addr;

    if (rcb->rcb_flags & RAW_LADDR)
    {
	ip->ip_src.s_addr = 
	    ((struct sockaddr_in *) &rcb->rcb_laddr)->sin_addr.s_addr;
    }
    else 
    {
	/*
	 * We may examine the routing tables twice.
	 * perhaps if this gets used a lot, it can be changed.
	 */
	struct route r;
	struct rtentry *rt;

	bzero ((caddr_t) &r, sizeof(r));
	((struct sockaddr_in *) (&r.ro_dst)) ->sin_family = AF_INET;
	((struct sockaddr_in *) (&r.ro_dst)) ->sin_addr.s_addr =
	    ip->ip_dst.s_addr;
	rtalloc(&r);
	if (rt = r.ro_rt) 
	{
	    ip->ip_src = IA_INADDR(in_iafromif(rt->rt_ifp));
	    rtfree (rt);
	}
	else 
	{
	    m_freem(m);
	    return (ENETUNREACH);
	}
    }

    NOPCB_IPSEND (m, len, FALSE, retval);
    return (retval);
}

#ifdef NSIP
/*
 * Generate IP header and pass packet to ip_output.
 * Tack on options user may have setup with control call.
 */
rip_output(m0, so)
struct mbuf *m0;
struct socket *so;
{
    register struct mbuf *m;

    /*
     * get an mbuf for IP header.
     */
    m = m_get(M_DONTWAIT, MT_HEADER);
    if (m == NULL)
    {
	m_freem(m0);
	return (ENOBUFS);
    }

    /*
     * Fill in IP header as needed.
     */
    m->m_off = MMAXOFF - sizeof(struct ip);
    m->m_len = sizeof(struct ip);
    m->m_next = m0;
    {
    register struct ip *ip;
    register struct rawcb *rcb = sotorawcb(so);

    ip = mtod(m, struct ip *);
    ip->ip_p	= rcb->rcb_proto.sp_protocol;
    ip->ip_tos	= 0;
    ip->ip_dst.s_addr = 
	((struct sockaddr_in *) &rcb->rcb_faddr)->sin_addr.s_addr;
    if (rcb->rcb_flags & RAW_LADDR)
	ip->ip_src.s_addr = 
	    ((struct sockaddr_in *) &rcb->rcb_laddr)->sin_addr.s_addr;
    else 
	ip->ip_src.s_addr = 0;
    }

    {
    register int retval;
    register int len;

    /*
     * Calculate data length
     */
    len = 0;
    while (m0)
    {
	len += m0->m_len;
	m0 = m0->m_next;
    }


    NOPCB_IPSEND (m, len, FALSE, retval);
    return (retval);
    }
}
#endif

/*
 * The UDP header is so small and simple, the user should either:
 *	1.  go all the way and use a raw IP socket
 * or	2.  use send(2) type system calls.
 */
/* ARGSUSED */
raw_udp_output (m0, so)
struct mbuf *m0;
struct socket *so;
{
    m_freem(m0);
    return (EOPNOTSUPP);
}

/*
 * TCP requires a lot of state information.  Sure we could try to verify the
 * user's header and pass it on to ip, but unless debugging a new version
 * in user code with a different protocol number, probably shouldn't ship
 * out tcp packets, since we'll get packets in reply that might screw us up.
 *
 * And how does the sending of a single tcp packet make sense?
 */
/* ARGSUSED */
raw_tcp_output (m0, so)
struct mbuf *m0;
struct socket *so;
{
    m_freem(m0);
    return (EOPNOTSUPP);
}

#ifdef RDP
/*
 * ditto TCP for RDP
 */
/* ARGSUSED */
raw_rdp_output (m0, so)
struct mbuf *m0;
struct socket *so;
{
    m_freem(m0);
    return(EOPNOTSUPP);
}
#endif

/*
 * use the user level stuff to send -- much simpler 
 */

#ifdef HMP
/* ARGSUSED */
raw_hmp_output (m0, so)
struct mbuf *m0;
struct socket *so;
{
    m_freem(m0);
    return(EOPNOTSUPP);
}
#endif
