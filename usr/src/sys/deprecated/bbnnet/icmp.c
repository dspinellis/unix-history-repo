#ifdef	RCSIDENT
static char rcsident[] = "$Header: icmp.c,v 1.17 85/06/18 14:53:43 walsh Exp $";
#endif

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/protosw.h"
#include "../h/syslog.h"

#include "../net/route.h"
#include "../net/if.h"

#include "../bbnnet/in.h"
#include "../bbnnet/net.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/in_var.h"

#include "../bbnnet/ip.h"
#include "../bbnnet/icmp.h"
#include "../bbnnet/nopcb.h"
#ifdef HMPTRAPS
#include "../bbnnet/hmp_traps.h"
#endif

#include "../h/errno.h"
#include "../h/time.h"
#include "../h/kernel.h"

#ifdef	RCSIDENT
static char rcsicmphdr[] = RCSICMPHDR;
#endif

extern int nosum;

#define NICTYPE	17

/* ICMP message formats */
#define ICBAD	0	/* unimplemented */
#define ICERR	1	/* error format (use header) */
#define ICDAT	2	/* data format (use id) */
#define ICINT	3	/* data format (handle internally) */

char icaction[NICTYPE] = 
{
    ICDAT, ICBAD, ICBAD, ICERR, ICERR, ICERR, ICBAD,
	ICBAD, ICINT, ICBAD, ICBAD, ICERR, ICERR, ICINT,
	ICDAT, ICINT, ICDAT 
} ;

#define ICLEN1	(sizeof(struct ip) + ICMPSIZE + sizeof(struct ip) + ICMP_ERRLEN)
#define ICLEN2	(sizeof(struct ip) + ICMPSIZE + 3 * sizeof(long))

int icpullup[NICTYPE] = 
{
    0,	/* echo reply */
	0,
	0,
	ICLEN1,	/* unreachable */
	ICLEN1,	/* source quench */
	ICLEN1,	/* redirect */
	0,
	0,
	0,	/* echo request */
	0,
	0,
	ICLEN1,	/* time exceeded */
	ICLEN1,	/* parameter problem */
	ICLEN2,	/* timestamp */
	ICLEN2,	/* timestamp reply */
	0,	/* information request */
	0	/* information reply */
} ;

char icunrch[ICMP_UNRCH_NUM] = 
{
    PRC_UNREACH_NET, PRC_UNREACH_HOST, PRC_UNREACH_PROTOCOL,
	PRC_UNREACH_PORT, PRC_MSGSIZE, PRC_UNREACH_HOST
} ;

struct icmp_stat icmpstat;


u_long iptime()
{
    int s = spl7();	/* berkeley had spl6() */
    u_long t;

    t = (time.tv_sec % (24*60*60)) * 1000 + time.tv_usec / 1000;
    splx(s);
    return (htonl(t));
}

know_gateway2 (gaddr, list)
u_long	gaddr;
struct mbuf *list;
{
    register struct rtentry *rt;

    while (list)
    {
	rt = mtod(list, struct rtentry *);
	if ((rt->rt_flags & RTF_GATEWAY) &&
	    (rt->rt_dst.sa_family == AF_INET) &&
	    (((struct sockaddr_in *) &rt->rt_gateway)->sin_addr.s_addr == gaddr))
		return (TRUE);
	list = list->m_next;
    }
    return (FALSE);
}

know_gateway (gaddr)
u_long	gaddr;
{
    register int	i;

    for (i=0 ; i<RTHASHSIZ ; i++)
    {
	if (know_gateway2 (gaddr, rthost[i]) ||
	    know_gateway2 (gaddr, rtnet[i]))
	    return (TRUE);
    }
    return (FALSE);
}

#ifdef BBNPING
/*
 * Note that pinging is done on a per-route basis.
 *
 * 1.  If a gateway is used by more than one route, then for routes
 *	with no active (measured by new data xfer) tcp connections,
 *	the gateway will be pinged.
 *	It is possible that every PINGTIME/2 seconds a gateway would
 *	be sent multiple icmp ECHO REQUESTS, but that is unlikely (uncommon)
 *	and we can worry about that if it actually proves to be a problem.
 *
 * 2.  Since the ping count is incremented on a per-route basis, but
 *	ECHO REPLIES are dealt with on a per-address basis, a gateway is
 *	not prematurely pinged out if it is used by more than one active
 *	routing entry.
 */

static check_ping(list)
register struct mbuf *list;
{
    register struct rtentry *rt;
    register struct sockaddr_in *sin;
    register struct mbuf *next;

    while (list)
    {
	rt = mtod(list, struct rtentry *);
	next = list->m_next;	/* in case remove it from list */

	if ((rt->rt_flags & RTF_GATEWAY) &&
	    (rt->rt_dst.sa_family == AF_INET))
	{
	    sin = (struct sockaddr_in *) &rt->rt_gateway;
	    if ((rt->rt_refcnt > 0) && (rt->rt_flags & RTF_UP)) 
	    {
		if (rt->irt_pings >= MAXPING)
		{
		    /*
		     * Too many unanswered pings.  re-route
		     * connections using this gateway.  Usually,
		     * this happens because the gateway is flooded
		     * with traffic.
		     */
		    union { u_long ul; u_char c[4]; } a;

		    a.ul = sin->sin_addr.s_addr;
		    log(LOG_INFO, "gw %d.%d.%d.%d pinged out\n",
			a.c[0], a.c[1], a.c[2], a.c[3]);

		    rt->irt_pings = 0;
		    ip_gdown(sin->sin_addr.s_addr);
		}
		else 
		{
		    /*
		     * Ping him again.
		     * See rcv_ack() for comparison with zero here.
		     */
		    rt->irt_pings ++;
		    if (rt->irt_pings > 0)
		    {
			/*
			 * count ping even if doesn't get to
			 * interface (ENOBUFS) or other error
			 * (EHOSTDOWN if no gateway at that
			 * address on an IMP network).
			 */

			ping (sin->sin_addr);
			icmpstat.ic_pings ++;
		    }
		    else
			icmpstat.ic_svpings ++;
		}
	    }
	    else 
	    {
		if (rt->rt_flags & RTF_REINSTATE)
		{
		    /*
		     * The gateway pinged out or died at some point.
		     * Let's see if it's back up or if our
		     * re-routing of current connections in ip_gdown
		     * has let it breathe again.  Wait a while
		     * before try to use it again.
		     */
		    rt->irt_gdown --;
		    if (rt->irt_gdown <= 0)
		    {
			rt->irt_gdown = 0;
			/*
			 * Wait until we know it's alive
			 * for certain.  Ping it.
			 */
			ping (sin->sin_addr);
		    }
		}
	    }
	}

	list = next;
    }
}

static reset_ping(list, addr)
register struct mbuf *list;
register u_long	addr;
{
    register struct rtentry *rt;

    while (list)
    {
	rt = mtod(list, struct rtentry *);
	if ((rt->rt_flags & RTF_GATEWAY) &&
	    (rt->rt_dst.sa_family == AF_INET))
	{
	    if (((struct sockaddr_in *) &rt->rt_gateway)->sin_addr.s_addr == addr)
	    {
		if (rt->rt_flags & RTF_REINSTATE)
		{
		    if (rt->irt_gdown == 0)
		    {
			/*
			 * Was not a slow echo reply.  If was dead,
			 * use it again.  If was flooded, new connections
			 * can now use it (old shifted away).
			 */
			rt->rt_flags |= RTF_UP;
			rt->rt_flags &= ~RTF_REINSTATE;
			rt->rt_refcnt --; /* see ip_gdown() */
		    }
		}
		else
		    rt->irt_pings = 0;
	    }
	}
	list = list->m_next;
    }
}

/*
 * Would be nice if we could use HOSTHASH/NETHASH/0, but the hashing is done
 * on the destination, not the intermediary gateway.
 */
got_ping(addr)
register u_long	addr;
{
    register int	i;

    for (i=0 ; i<RTHASHSIZ ; i++)
    {
	reset_ping(rthost[i], addr);
	reset_ping(rtnet[i], addr);
    }
}
#endif

/*
 * Process ICMP messages.  Called directly from ip_input processor.
 */
icmp(mp)
register struct mbuf *mp;
{
    register struct ip *ip;
    register struct icmp *icp;
    struct in_ifaddr *ia;
    int ilen;
    int prccode;

    icmpstat.ic_total ++;

    /*
     * see ip_input()
     */
    if ((mp->m_off > MMAXOFF) ||
	(mp->m_len < sizeof(struct ip) + ICMPSIZE))
    {
	if ((mp = m_pullup(mp, sizeof(struct ip) + ICMPSIZE)) == NULL)
	{
	    icmpstat.ic_tooshort ++;
	    return;
	}
    }
    ip = mtod(mp, struct ip *);
    icp = (struct icmp *) (ip+1);

    /*
     * watch for fools sending out broadcast ICMP packets
     * Don't check against inetifp, since is up to ip_input whether to receive
     * on some interface rather than send to self for input on dst interface.
     */
    ia = in_iawithaddr(ip->ip_dst, FALSE);
    if (ia == NULL)
    {
	/* drop it */
	m_freem(mp);
	return;
    }

    /* filter out message types */

    if (icp->ic_type >= NICTYPE || icaction[icp->ic_type] == ICBAD)
    {
	icmpstat.ic_drops++;
	goto badret;
    }

    if (mp->m_len < icpullup[icp->ic_type]) 
    {
	if ((mp = m_pullup(mp, icpullup[icp->ic_type])) == NULL)
	{
	    icmpstat.ic_tooshort ++;
	    return;
	}
	ip = mtod(mp, struct ip *);
	icp = (struct icmp *) (ip+1);
    }
    mp->m_off += sizeof(struct ip);
    mp->m_len -= sizeof(struct ip);

    ilen = ip->ip_len;

    {
    register u_short his_sum, our_sum;

    his_sum = (u_short)icp->ic_sum;
    icp->ic_sum = 0;
    if (his_sum != (our_sum = (u_short)in_cksum(mp, ilen))) 
    {
	icmpstat.ic_badsum++;
	if (! nosum)
	{
	    /* note that the icmp header doesn't overlap IP */
#ifdef HMPTRAPS
	    /* hmp_trap(T_ICMP_CKSUM, (caddr_t),0); */
#endif
	    inet_cksum_err ("icmp", ip, (u_long) his_sum, (u_long) our_sum);
	    netlog(mp);
	    return;
	}
    }
    }

    /*
     * Now do any processing.  Some messages are handled here,
     * others are passed up ctlinput path for further processing.
     */

    switch (icp->ic_type) 
    {

      case ICMP_UNRCH:	/* destination unreachable */

	if (icp->ic_code < ICMP_UNRCH_NUM)
	{
	    register int (*ctlfunc)();

	    prccode = icunrch[icp->ic_code];
passup:
	    ctlfunc = ipsw[icp->ic_iphdr.ip_p].ipsw_user->pr_ctlinput;
	    (*ctlfunc) (prccode, (caddr_t) icp);
	}
	break;

      case ICMP_SRCQ:	/* source quench */

	/*
	 * At the IP level, we could try to reroute the connection and see if we
	 * come up with a less loaded gateway.  Problem with this is that we know
	 * total number of packets sent over a route, not the recent traffic load.
	 */
	icmpstat.ic_quenches++;
	prccode = PRC_QUENCH;
#ifdef HMPTRAPS
	/* hmp_trap(T_ICMP_SRCQ, (caddr_t)0, 0); */
#endif
	goto passup;

      case ICMP_REDIR:	/* redirect */

	icmpstat.ic_redirects ++;

	/*
	 * Sorry, we only trust the connected set of gateways
	 * that includes gateways installed by the system
	 * manager.
	 */
	if (know_gateway(ip->ip_src.s_addr))
	{
	    register struct mbuf **table;

	    if (icp->ic_code == ICMP_REDIR_NET)
	    {
		prccode = PRC_REDIRECT_NET;
		table = rtnet;
	    }
	    else
	    {
		prccode = PRC_REDIRECT_HOST;
		table = rthost;
	    }
	    if (icmp_redirect_route (icp, table))
		goto passup;
	}
	else
	{
	    /*
	     * Who are you?  Why are you talking to us?
	     * And how do we know the ip source isn't a lie?
	     * (Eg., Catches Symbolics redirection of subnet broadcast.)
	     */
	    union { u_long ul; u_char c[4]; } a;

	    a.ul = ip->ip_src.s_addr;
	    log(LOG_INFO, "Ignoring redirect from %d.%d.%d.%d\n",
		a.c[0], a.c[1], a.c[2], a.c[3]);
	}
#ifdef HMPTRAPS
	/* hmp_trap(T_ICMP_REDIR, (caddr_t)0,0); */
#endif
	break;

      case ICMP_ECHO:	/* echo */

	icp->ic_type = ICMP_ECHOR;
	icmpstat.ic_echoes++;
	goto loopback;

      case ICMP_ECHOR:	/* echo reply */

	/* check for gateway ping packets, look for
	 * corresponding gateway entry and set echo count
	 * to zero.
	 */
#ifdef	BBNPING
	if (icp->ic_id == MY_ECHO_ID)
	    got_ping(ip->ip_src.s_addr);
#endif
	break;

      case ICMP_TIMEX:	/* time exceeded */
	/*
	 * IP time to live field should be associated with the route so
	 * that it can be dynamically adjusted for time exceeded in transit.
	 * If did, would only need to "pass time exceeded in reassembly"
	 * up to protocol (TCP) so that it can better try to avoid IP
	 * fragmentation.
	 */
	icmpstat.ic_timex++;
	prccode = (icp->ic_code == ICMP_TIMEX_XMT)
	    ? PRC_TIMXCEED_INTRANS
	    : PRC_TIMXCEED_REASS;
#ifdef HMPTRAPS
	/* hmp_trap(T_ICMP_TIMEX, (caddr_t)0,0); */
#endif
	goto passup;

      case ICMP_TIMES:	/* timestamp */

	if (icp->ic_code == 0) 
	{
	    icp->ic_type = ICMP_TIMESR;
	    /*
	     * Can now do timestamps in UT
	     *
	       icp->ic_trcv = (long)time.tv_sec | 0x80;
	       icp->ic_txmt = (long)time.tv_sec | 0x80;
	     */
	    icp->ic_txmt = icp->ic_trcv = iptime();
	    goto loopback;
	}
	break;

      case ICMP_INFO:	/* info request */
	/*
	 * He knows his host number, but not his network #,
	 * fill in src & dst as he would have, had he known.
	 */
	{
	register struct in_ifaddr *inaddress;
	extern struct ifnet *inetifp;

	icp->ic_type = ICMP_INFOR;
	inaddress = in_iafromif(inetifp);
	ip->ip_src.s_addr |= inaddress->ia_subnet;
	ip->ip_dst = redir_addr(ip);
	}
	goto loopback;

      case ICMP_PARM:	/* parameter problem */
	icmpstat.ic_parm++;
	prccode = PRC_PARAMPROB;
#ifdef HMPTRAPS
	/* hmp_trap(T_ICMP_PARM, (caddr_t)0,0); */
#endif
	goto passup;
    }

badret :
    m_freem(mp);
    return;

loopback :
    {
	struct in_addr temp;
	register int error;

	temp = ip->ip_src;
	ip->ip_src = ip->ip_dst;
	ip->ip_dst = temp;
	/* ip->ip_p = IPPROTO_ICMP; still is from input */
	/* ip->ip_tos = 0; use same tos for reply */

	icp->ic_sum = in_cksum(mp, ilen);
	mp->m_off -= sizeof(struct ip);
	mp->m_len += sizeof(struct ip);
	NOPCB_IPSEND (mp, (int)ip->ip_len, FALSE, error);

#ifdef lint
	error = error;
#endif

    }
}


/*
 * Ping gateways in use to see if they are still alive.
 */
ic_timeo()
{
#ifdef BBNPING
    register int	i;
    register int	level;
    static int ictimer;

    if (--ictimer > 0)
	return;
    ictimer = PINGTIME;

    level = splnet();
    for (i=0 ; i<RTHASHSIZ ; i++)
    {
	check_ping(rthost[i]);
	check_ping(rtnet[i]);
    }
    splx(level);
#endif
}

static struct rtentry *rtfind (dst, via, table)
struct in_addr	 dst;
struct in_addr	 via;
struct mbuf	*table[];
{
    register struct mbuf	*m;

    struct rtentry	*rt;

    if (table == rthost)
	m = rthost[HOSTHASH(dst.s_addr) % RTHASHSIZ];
    else
    {
	if (dst.s_addr) 
	{
	    m = rtnet[NETHASH(dst) % RTHASHSIZ];
	    dst.s_addr = iptonet(dst);
	}
	else
	    m = rtnet[0];
    }

    while (m)
    {
	struct in_addr d, g;

	rt = mtod(m, struct rtentry *);
	d = satoipa(&rt->rt_dst);
	g = satoipa(&rt->rt_gateway);
	if ((d.s_addr == dst.s_addr) &&
	    (g.s_addr == via.s_addr) &&
	    (rt->rt_dst.sa_family == AF_INET))
	{
	    /* then, hash values must be same. */
	    return (rt);
	}

	m = m->m_next;
    }

    return (NULL);
}


icmp_redirect_route (ic, table)
struct icmp	*ic;
struct mbuf	*table[];
{
    struct ip	*ip;
    int flags;
    static struct sockaddr_in red_dst = { AF_INET } ;
    static struct sockaddr_in red_gtw = { AF_INET } ;

    ip = (struct ip *) ic->ic_data;
    /*
     * 1.  Make new routing entry so that new connections will use better
     * route.  But only make entry if have not already done so.
     */
    if (!rtfind(ip->ip_dst, ic->ic_gaddr, table))
    {
	char	*err;

	/* check reasonableness of redirect */

	if (in_iawithnet(ic->ic_gaddr) == NULL)
	{
	    /*
	     * Sorry, can't get there from here.
	     */
	    union { u_long ul; u_char c[4]; } g, f, t, v;

	    err = "No interface for first hop";
perr :

	    g.ul = (((struct ip *) (((char *) ic) - sizeof(struct ip)))->ip_src.s_addr);
	    f.ul = ip->ip_src.s_addr;
	    t.ul = ip->ip_dst.s_addr;
	    v.ul = ic->ic_gaddr.s_addr;
	    log(LOG_INFO,
		"Ignoring ICMP redirect from gw %d.%d.%d.%d? to go from %d.%d.%d.%d to %d.%d.%d.%d via %d.%d.%d.%d : %s\n",
		g.c[0], g.c[1], g.c[2], g.c[3],
		f.c[0], f.c[1], f.c[2], f.c[3],
		t.c[0], t.c[1], t.c[2], t.c[3],
		v.c[0], v.c[1], v.c[2], v.c[3],
		err);

	    return (FALSE);
	}

	if (in_iawithaddr(ic->ic_gaddr, TRUE))
	{
	    /*
	     * redirect to self is stupid, as is redirect to
	     * broadcast address (which if_iawithaddr will match
	     * for interfaces with IFF_BROADCAST set.)
	     */
	    err = "redirected to self";
	    goto perr;
	}

	if (iptonet(ic->ic_gaddr) != iptonet(ip->ip_src))
	{
	    /*
	     * Why is this gateway redirecting us?  It is not
	     * giving us a first hop gateway that is on the
	     * local net that we advertise.
	     */
	    err = "new first hop net <> src net";
	    goto perr;
	}

#ifdef done_in_icmp_c
	if (! know_gateway(icmp source))
	    /*
	     * Sorry, we only trust the connected set of gateways
	     * that includes gateways installed by the system
	     * manager.  Who are you?  Why are you talking to us?
	     */
	    return;
#endif

	/* o.k., I'll believe it */
	flags = RTF_UP;
	if (table == rthost)
	{
	    flags |= RTF_HOST;
	    red_dst.sin_addr.s_addr = ip->ip_dst.s_addr;
	}
	else 
	{
	    flags |= RTF_GATEWAY;
	    red_dst.sin_addr.s_addr = iptonet(ip->ip_dst);
	}
	red_gtw.sin_addr.s_addr = ic->ic_gaddr.s_addr;
	rtinit ((struct sockaddr *) &red_dst,
	    (struct sockaddr *) &red_gtw,
	    flags);
    }
    return (TRUE);
}

icmp_redirect_inp(inp, ic, table)
struct inpcb	*inp;
struct icmp	*ic;
struct mbuf	**table;
{
    struct rtentry *rt;

    /*
     * 2.  Redirect current connection.
     */

#ifdef neverdef
	/*
	 * This would try to balance load across gateways, but
	 * that's something best done by the gateway before it
	 * sends a redirect.  Also, consider 3 gateways of which
	 * two are bad, and possibility of bouncing between the
	 * two bad ones until their use counts got high enough.
	 *
	 * Currently, gateways only take into account # hops, not
	 * load.
	 */
	if (rt = inp->inp_route.ro_rt)
	{
	    short	oflags;

	    /* try to force a different path */
	    oflags = rt->rt_flags;
	    rt->rt_flags &= ~RTF_UP;
	    /* but don't lose current route */
	    rt->rt_refcnt ++;
	    (void) ip_reroute (inp);
	    rt->rt_refcnt --;
	    rt->rt_flags = oflags;
	}
#endif
	if (rt = rtfind (ic->ic_iphdr.ip_dst, ic->ic_gaddr, table))
	{
	    if (rt->rt_flags & RTF_UP)
	    {
		/*
		 * packets go out an interface with our local
		 * IP address.  Know true from checks after
		 * first call to rtfind above.
		 *
		 * Interface has to be at least as up as
		 * for previous route, so don't bother to
		 * check.
		 */
		if (inp->inp_route.ro_rt)
		    rtfree (inp->inp_route.ro_rt);
		inp->inp_route.ro_rt = rt;
		rt->rt_refcnt ++;
	    }
	    else
		log(LOG_INFO, "ICMP Redirect to down route\n");
	}
	else
	    log(LOG_INFO, "ICMP Redirect route not installed?\n");
}
