/*	udp_usrreq.c	4.45	83/02/16	*/
/*
 * UDP protocol implementation.
 * Per RFC 768, August, 1980.
 */

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/errno.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../bbnnet/in.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/net.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/udp.h"
#include "../bbnnet/fsm.h"
#include "../bbnnet/tcp.h"
#include "../bbnnet/icmp.h"

extern udp_binding_used();
extern struct inpcb udp;

struct pr_advice udp_advice = 
{
    UDP_RESERVED,		/* all basically the same as TCP */
    UDP_USERRESERVED,
    UDP_MAXPORT,
    UDP_USERRESERVED+1,
    sizeof(u_short),
    udp_binding_used
} ;

udp_init()
{
    udp.inp_next = udp.inp_prev = &udp;
    ipsw[IPPROTO_UDP].ipsw_hlen = sizeof(struct udp);
}

udp_abort(inp)
struct inpcb *inp;
{
    struct socket *so = inp->inp_socket;

    in_pcbdisconnect(inp, (int(*)())0);
    soisdisconnected(so);
}

/*
 * Is a udp port/address pair already in use?
 */
int udp_binding_used(inp, lport, lsaddr, reuselocal)
struct inpcb   *inp;
u_short	lport;
u_long	lsaddr;
{
    register struct inpcb *i;

    if (reuselocal)
	/*
	 * But since UDP, unlike TCP, is not connection oriented,
	 * this allows for liars to exist.
	 */
	return (0);

    for (i = udp.inp_next; i != &udp; i = i->inp_next) 
    {
	if (i != inp)
	    if (i->inp_lport == lport)
		if ((i->inp_laddr.s_addr == lsaddr) ||
		    (i->inp_laddr.s_addr == INADDR_ANY) ||
		    (lsaddr == INADDR_ANY))
			break;
    }
    return (i != &udp);
}

char *udp_conn_used(inp, lport, lsaddr, fport, fsaddr)
struct inpcb   *inp;
u_short	lport;
u_long	lsaddr;
u_short	fport;
u_long	fsaddr;
{
    register struct inpcb *i;

    for (i = udp.inp_next; i != &udp; i = i->inp_next) 
    {
	/*
	 * Since our inpcb is in this linked list, don't want to know
	 * if we, ourselves, are already using this connetion.
	 */
	if (i != inp)
	    if ((i->inp_lport == lport) && (i->inp_fport == fport) &&
		(i->inp_laddr.s_addr == lsaddr) &&
		(i->inp_faddr.s_addr == fsaddr))
		    return((char *) i);
    }

    return ((char *) NULL);
}


/*ARGSUSED*/
udp_usrreq(so, req, m, nam, rights)
struct socket *so;
register int req;
struct mbuf *m, *nam, *rights;
{
    register int s;
    struct inpcb *inp;
    int error = 0;

    s = splnet();
    inp = sotoinpcb(so);

    if (rights && req != PRU_CONTROL) 
    {
	if (rights->m_len) 
	{
	    error = EINVAL;
	    goto release;
	}
    }

    if (inp == NULL && req != PRU_ATTACH) 
    {
	error = EINVAL;
	goto release;
    }

    switch (req) 
    {

      case PRU_ATTACH:
	if (inp != NULL) 
	{
	    error = EINVAL;
	    break;
	}
	error = soreserve(so, 2048, 2048);
	if (error)
	    break;
	error = in_pcballoc(so, &udp);
	if (error)
	    break;
	break;

      case PRU_DETACH:
	if (inp == NULL) 
	{
	    error = ENOTCONN;
	    break;
	}
	in_pcbdetach(inp, (int (*)())0);
	break;

      case PRU_BIND:
	error = in_pcbbind(inp, nam, &udp_advice);
	break;

      case PRU_LISTEN:
	error = EOPNOTSUPP;
	break;

      case PRU_CONNECT:
	if (inp->inp_faddr.s_addr != INADDR_ANY) 
	{
	    error = EISCONN;
	    break;
	}
	if (inp->inp_lport == 0) 
	{
	    error = in_pcbbind(inp, (struct mbuf *)0, &udp_advice);
	    if (error)
		break;
	}
	error = in_pcbconnect(inp, nam, udp_conn_used);
	if (error == 0)
	    soisconnected(so);
	break;

      case PRU_ACCEPT:
	error = EOPNOTSUPP;
	break;

      case PRU_DISCONNECT:
	if (inp->inp_faddr.s_addr == INADDR_ANY) 
	{
	    error = ENOTCONN;
	    break;
	}
	in_pcbdisconnect(inp, (int(*)())0);
	soisdisconnected(so);
	break;

      case PRU_SHUTDOWN:
	socantsendmore(so);
	break;

      case PRU_SEND: 
	{
	    struct in_addr laddr;

	    if (nam) 
	    {
		laddr = inp->inp_laddr;
		if (inp->inp_faddr.s_addr != INADDR_ANY) 
		{
		    error = EISCONN;
		    break;
		}
		if (inp->inp_lport == 0) 
		{
		    if (error = in_pcbbind(inp, (struct mbuf *)0, &udp_advice))
			break;
		}
		error = in_pcbconnect(inp, nam, udp_conn_used);
		if (error)
		    break;
	    }
	    else 
	    {
		if (inp->inp_faddr.s_addr == INADDR_ANY) 
		{
		    error = ENOTCONN;
		    break;
		}
	    }
	    error = udp_output(inp, m);
	    m = NULL;
	    if (nam) 
	    {
		in_pcbdisconnect(inp, (int(*))0);
		inp->inp_laddr = laddr;
	    }
	}
	break;

      case PRU_ABORT:
	in_pcbdetach(inp, (int (*)())0);
	break;

      case PRU_CONTROL:
	/* not our ioctl, let lower level try ioctl */
	error = ip_ioctl (inp, (int) m, (caddr_t) nam);
	goto dontfree;

      case PRU_SOCKADDR:
	in_setsockaddr(inp, nam);
	break;

      default:
	panic("udp_usrreq");
    }

release :
    if (m != NULL)
	m_freem(m);
dontfree:
    splx(s);
    return (error);
}

udp_ctlinput (prc_code, arg)
caddr_t arg;
{
    int error;

    error = inetctlerrmap[prc_code];

    switch (prc_code)
    {
	case PRC_UNREACH_PROTOCOL:	/* icmp message */
	case PRC_UNREACH_PORT:
	case PRC_MSGSIZE:
	    {
	    register struct udp	*up;
	    struct inpcb *inp;

	    up = (struct udp *) (&((struct icmp *) arg)->ic_iphdr);
	    inp = (struct inpcb *)udp_conn_used ((struct inpcb *) 0,
		up->u_src, up->u_s.s_addr,
		up->u_dst, up->u_d.s_addr);

	    if (inp)
	    {
		inp->inp_socket->so_error = error;
		udp_abort(inp);
	    }
	    }
	    break;

	case PRC_UNREACH_NET:
	case PRC_UNREACH_HOST:
	    {
	    register struct udp	*up;
	    struct inpcb *inp;

	    up = (struct udp *) (&((struct icmp *) arg)->ic_iphdr);
	    inp = (struct inpcb *)udp_conn_used ((struct inpcb *) 0,
		up->u_src, up->u_s.s_addr,
		up->u_dst, up->u_d.s_addr);

	    if (inp)
	    {
		struct socket *so;

		so = inp->inp_socket;
		if ((so->so_state & SS_NOFDREF) == 0)
		    advise_user(so, error);
		else
		{
		    so->so_error = error;
		    udp_abort(inp);
		}
	    }
	    }
	    break;

	case PRC_GWDOWN:
	    in_gdown (&udp, (u_long) arg);
	    break;

	case PRC_REDIRECT_NET:	/* icmp message */
	case PRC_REDIRECT_HOST:
	    {
	    register struct udp	*up;
	    struct inpcb *inp;

	    up = (struct udp *) (&((struct icmp *) arg)->ic_iphdr);
	    inp = (struct inpcb *)udp_conn_used ((struct inpcb *) 0,
		up->u_src, up->u_s.s_addr,
		up->u_dst, up->u_d.s_addr);

	    if (inp)
		icmp_redirect_inp(inp, (struct icmp *) arg,
		 prc_code == PRC_REDIRECT_NET ? rtnet : rthost);
	    }
	    break;

	case PRC_TIMXCEED_INTRANS:	/* icmp message */
	case PRC_TIMXCEED_REASS:
	case PRC_PARAMPROB:
	case PRC_QUENCH:
	    break;

	case PRC_IFDOWN:
	    {
	    u_long addr;

	    addr = ((struct sockaddr_in *)(arg))->sin_addr.s_addr;
	    inpcb_notify(&udp, addr, (u_long) 0, error);
	    inpcb_notify(&udp, (u_long) 0, addr, error);
	    }
	    break;

	case PRC_HOSTDEAD:	/* from imp interface */
	case PRC_HOSTUNREACH:
	    /*
	     * get same message for destination hosts and gateways.
	     */
	    {
	    u_long addr;

	    addr = ((struct sockaddr_in *)arg)->sin_addr.s_addr;
	    in_gdown (&udp, addr);
	    inpcb_notify(&udp, (u_long) 0, addr, error);
	    }
	    break;

	default:
	    panic("udp_ctlinput");
    }
}
