#ifdef	RCSIDENT
static char rcsident[] = "$Header: in_pcb.c,v 1.12 84/11/29 17:02:13 walsh Exp $";
#endif	RCSIDENT

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/protosw.h"
#include "../h/domain.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../bbnnet/in.h"
#include "../bbnnet/net.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/in_var.h"

extern struct rtentry *ip_route();
extern struct domain  inetdomain;

in_pcballoc(so, head)
struct socket *so;
struct inpcb *head;
{
    register struct mbuf *m;
    register struct inpcb *inp;

    m = m_getclr(M_DONTWAIT, MT_PCB);
    if (m == NULL)
	return (ENOBUFS);

    inp = mtod(m, struct inpcb *);
    inp->inp_socket = so;

    insque(inp,head);

    so->so_pcb = (caddr_t)inp;

    return (0);
}

/*
 * changed from 4.2 to accept a structure which has protocol
 * specific data like how to break down port allocation.
 */

in_pcbbind(inp, nam, advice)
register struct inpcb *inp;
struct mbuf *nam;
struct pr_advice *advice;
{
    register struct socket *so = inp->inp_socket;
    register struct sockaddr_in *sin;
    register u_short lport = 0;

    if (in_ifaddr == NULL)
	return (EADDRNOTAVAIL);
    /*
     * socket must not already be bound
     */

    if (inp->inp_lport || inp->inp_laddr.s_addr != INADDR_ANY)
	return (EINVAL);

    if (nam == 0)
	goto noname;
    sin = mtod(nam, struct sockaddr_in *);
    if (nam->m_len != sizeof (*sin))
	return (EINVAL);
    /*
     * Since Berkeley left this out, some of their programs (ftpd)
     * aren't ready for it
     *
    if (sin->sin_family != AF_INET)
	    return (EAFNOSUPPORT);
     */

    if (sin->sin_addr.s_addr != INADDR_ANY) 
    {
	/* some code says ..withnet() */
	if (in_iawithaddr(sin->sin_addr, FALSE) == NULL)
	    return (EADDRNOTAVAIL);

    }

    /* user gives port to us in net order */
    if (lport = sin->sin_port)
    {
	u_short aport;

	/* if portsize > 2 a major rewrite needed to
	 * accomodate longs.....
	 */

	if (advice->portsize > 1)
	    aport = ntohs(lport);
	else
	{
	    if (lport != (lport & 0xff))
		return(EINVAL);	/* must be 8 bits */

	    aport = lport;	/* a char is a char */
	}

	/*
	 *  really only a worry for byte size ports
	 */

	if (aport > advice->maxport)
	    return(EADDRNOTAVAIL);

	if (aport <= advice->rootport && u.u_uid != 0)
	    return (EACCES);

	/*
	 * Check to see if the local address/port is in use.
	 * but, process may use this pair to communicate with
	 * several destinations (each with its own tcp) if he
	 * sets SO_REUSEADDR
	 */
	if (advice->bind_used &&
	    (*(advice->bind_used))(inp,		/* current binding */
	    lport,			/* desired port */
	    sin->sin_addr.s_addr,	/* desired address */
	    so->so_options & SO_REUSEADDR))
	{
	    return (EADDRINUSE);
	}
    }
    inp->inp_laddr = sin->sin_addr;

noname :
    /* any ports for random allocation by non-root users? */
    if ((advice->maxport <= advice->resvport) && (u.u_uid))
	return(EADDRNOTAVAIL);

    if (lport == 0)
    {
	/*
	 * Allow for reserved ports for non-super users
	 * so that don't interfere with some project's software.
	 */
	u_short possible = advice->nowport;

	do
	{
	    if (advice->portsize > 1)
		lport = htons(possible);
	    else
		lport = possible;

	    /*
	     * catch roll over.....
	     */

	    if (possible >= advice->maxport)
		possible = advice->resvport + 1;
	    else 
		possible++;

	    /*
	     * no free ports??? RDP/HMP problem
	     */

	    if (possible == advice->nowport)
		return(EADDRNOTAVAIL);

	}
	while (advice->bind_used &&
	    (*(advice->bind_used))(inp, lport, inp->inp_laddr.s_addr, 0));

	advice->nowport = possible;
    }
    inp->inp_lport = lport;
    return (0);
}

/*
 * Connect from a socket to a specified address.
 * Both address and port must be specified in argument sin.
 * If don't have a local address for this socket yet,
 * then pick one.
 */
in_pcbconnect(inp, nam, conn_used)
struct inpcb *inp;
struct mbuf *nam;
char *(*conn_used)();
{
    register struct ifnet *ifp = NULL;
    register struct in_ifaddr *ia = NULL;
    register struct sockaddr_in *ifaddr;
    register struct sockaddr_in *sin = mtod(nam, struct sockaddr_in *);
    register struct rtentry *rt;
    struct sockaddr_in *inpsin;

    if (nam->m_len != sizeof (*sin))
	return (EINVAL);
    if (sin->sin_family != AF_INET)
	return (EAFNOSUPPORT);
    if (sin->sin_addr.s_addr == INADDR_ANY || sin->sin_port == 0)
	return (EADDRNOTAVAIL);

    /*
     * Find route for connection.  For a tcp connecting to a server,
     * this route will be used for the duration of the connection
     * (unless redirected...).  For a UDP doing a connect, this route
     * will also be used for the duration.  For a UDP unconnected send,
     * this route will be used for the current packet.
     *
     * rtalloc cannot handle routing with both sides already bound
     */
    rt = (struct rtentry *) NULL;

    /*
     * NOTE: programmers often forget to zero sin_zero[0-1].
     * rtalloc does not want to know the port number for routes to hosts.
     */
    inpsin = (struct sockaddr_in *) &inp->inp_route.ro_dst;
    bcopy((caddr_t)sin, (caddr_t)inpsin, sizeof (*sin));
    inpsin->sin_port = 0;

    if (inp->inp_laddr.s_addr == INADDR_ANY) 
    {
	rtalloc(&inp->inp_route);
	if (rt = inp->inp_route.ro_rt)
	    ifp = rt->rt_ifp;
    }
    else 
    {
	if (rt = ip_route(&inp->inp_laddr, &sin->sin_addr)) 
	{
	    inp->inp_route.ro_rt = rt;
	    ifp = rt->rt_ifp;
	}
    }

    if (ifp == NULL)
	return (ENETUNREACH);

    /*
     * find Internet address structure for this interface.
     */
    ia = in_iafromif(ifp);

    if (ia == NULL)
	/* ??? */
	return (ENETUNREACH);

    ifaddr = (struct sockaddr_in *) &ia->ia_addr;

#ifdef bsd42
    /*
     * 8.7.0.2 (on IMP net) can send to 128.11.0.0 (on Ethernet), but
     * not to 8.0.0.0
     */
    if (in_broadcast(sin->sin_addr) &&
	iptonet(sin->sin_addr) == iptonet(ifaddr->sin_addr) &&
	!(ifp->if_flags & IFF_BROADCAST) )
    {
	if (rt)
	{
	    rtfree(rt);
	    inp->inp_route.ro_rt = NULL;
	}
	return (EADDRNOTAVAIL);
    }
#endif

    if ((*conn_used)(inp,
	inp->inp_lport, 
	(inp->inp_laddr.s_addr ? inp->inp_laddr.s_addr : ifaddr->sin_addr.s_addr),
	sin->sin_port,
	sin->sin_addr.s_addr) != (char *)NULL)
    {

	if (rt)
	{
	    rtfree(rt);
	    inp->inp_route.ro_rt = NULL;
	}
	return (EADDRINUSE);
    }

    if (inp->inp_laddr.s_addr == INADDR_ANY)
	inp->inp_laddr = ifaddr->sin_addr;
    inp->inp_faddr = sin->sin_addr;
    inp->inp_fport = sin->sin_port;
    return (0);
}

in_pcbdisconnect(inp, pcb_free_func)
struct inpcb *inp;
int (*pcb_free_func)();
{
    inp->inp_faddr.s_addr = INADDR_ANY;
    inp->inp_fport = 0;
    /*
     * may attach a route to an inpcb several times.  For example,
     * when UDP does unconnected, but bound, sends.
     */
    if (inp->inp_route.ro_rt)
    {
	rtfree(inp->inp_route.ro_rt);
	inp->inp_route.ro_rt = NULL;
    }

    if (inp->inp_socket->so_state & SS_NOFDREF)
	in_pcbdetach(inp, pcb_free_func);
}

/*
 * Don't need to splnet while altering lists, since called from places
 * where that has already been done.
 */
in_pcbdetach(inp, pcb_free_func)
register struct inpcb *inp;
int (*pcb_free_func)();
{
    register struct socket *so;

    if (so = inp->inp_socket)
    {
	so->so_pcb = (caddr_t) NULL;
	/* inp->inp_socket = (struct socket *) NULL; */
	soisdisconnected(so);
	sofree(so);
    }
    else
	panic("in_pcbdetach");

    if (inp->inp_route.ro_rt)
	rtfree(inp->inp_route.ro_rt);

    if (inp->inp_ppcb)
	(*pcb_free_func)(inp); /* free per-protocol block */

    remque(inp);

    (void) m_free(dtom(inp));
}

in_setsockaddr(inp, nam)
register struct inpcb *inp;
struct mbuf *nam;
{
    register struct sockaddr_in *sin;

    nam->m_len = sizeof (*sin);
    sin = mtod(nam, struct sockaddr_in *);
    bzero((caddr_t)sin, sizeof (*sin));
    sin->sin_family = AF_INET;
    sin->sin_port = inp->inp_lport;
    sin->sin_addr = inp->inp_laddr;
}

in_setpeeraddr(inp, nam)
register struct inpcb *inp;
struct mbuf *nam;
{
    register struct sockaddr_in *sin;

    nam->m_len = sizeof (*sin);
    sin = mtod(nam, struct sockaddr_in *);
    bzero((caddr_t)sin, sizeof (*sin));
    sin->sin_family = AF_INET;
    sin->sin_port = inp->inp_fport;
    sin->sin_addr = inp->inp_faddr;
}

/*
 * somewhat different from the one in 4.2 and (I think) substantially
 * easier to read, though a bit slower.
 *
 * fport == 0 if don't want/need match on remote port # (HMP and UDP)
 */
struct inpcb *
in_pcblookup(head,faddr,fport,laddr,lport,wild)
struct inpcb *head;
u_long faddr, laddr;
u_short fport, lport;
int wild;
{
    register struct inpcb *inp;

    /* try exact match */
    for(inp = head->inp_next; inp != head; inp = inp->inp_next)
    {
	/* ports check */
	if (inp->inp_lport != lport) 
	    continue;

	if (fport && (inp->inp_fport != fport))
	    continue;

	if ((inp->inp_faddr.s_addr != faddr) || (inp->inp_laddr.s_addr != laddr))
	    continue;

	/* keep it! */
	return(inp);
    }

    /* try wildcard ? */
    if (wild)
    {
	for(inp = head->inp_next; inp != head; inp = inp->inp_next)
	{
	    /* ports again */
	    if (inp->inp_lport != lport)
		continue;

	    if (fport && (inp->inp_fport != fport) && inp->inp_fport)
		continue;

	    if ((inp->inp_faddr.s_addr) && (inp->inp_faddr.s_addr != faddr))
		continue;

	    if ((inp->inp_laddr.s_addr) && (inp->inp_laddr.s_addr != laddr))
		continue;

	    return(inp);
	}
    }

    return((struct inpcb *) NULL);
}


/*
 * This only advises process and does not internally close socket,
 * not so much because the user can do much but close when he gets a
 * HOSTDEAD/HOSTUNREACH indication, but because it is possible that
 * the destination host has saved connection state information. (His IMP
 * interface went down for PM, but the machine stayed up...)
 * 
 * Also, this makes addition of new protocols easy, since we don't need to
 * know the name and calling sequence of their close/abort routine.
 *
 * We do not close child sockets of listen(2)ers for connection oriented
 * protocols.  We let the protocol do that by timing out connection
 * establishment.
 */
inpcb_notify(head, laddr, faddr, error)
register struct inpcb *head;
register u_long	laddr;
register u_long	faddr;
{
    register struct inpcb   *inp;

    for(inp = head->inp_next; inp != head; inp = inp->inp_next)
	if (((inp->inp_faddr.s_addr == faddr) || (faddr == 0)) &&
	    ((inp->inp_laddr.s_addr == laddr) || (laddr == 0)))
		advise_user(inp->inp_socket, error);
}

advise_user(so, error)
struct socket *so;
int error;
{
    if (so == 0)
	return;

    so->so_error = error;

    wakeup((caddr_t) &so->so_timeo); /* in connect(2) */
    sowwakeup(so);
    sorwakeup(so);
}
