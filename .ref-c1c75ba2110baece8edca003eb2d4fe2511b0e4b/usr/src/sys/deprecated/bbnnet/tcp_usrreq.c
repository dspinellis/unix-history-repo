#ifdef	RCSIDENT
static char rcsident[] = "$Header: tcp_usrreq.c,v 1.30 85/07/31 09:43:43 walsh Exp $";
#endif RCSIDENT

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/protosw.h"
#include "../h/errno.h"
#include "../h/ioctl.h"
#include "../h/time.h"
#include "../h/kernel.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../bbnnet/in.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/net.h"
#include "../bbnnet/fsm.h"
#include "../bbnnet/tcp.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/icmp.h"
#include "../bbnnet/macros.h"
#include "../bbnnet/sws.h"

/*
 * TCP protocol interface to socket abstraction.
 */

#ifdef GPROF
int	tcp_acounts[TCP_NSTATES][PRU_NREQ];
#endif

extern tcp_pcbdisconnect();
extern tcp_binding_used();

struct inpcb tcp;
struct tcp_stat tcpstat;
sequence tcp_iss;		/* tcp initial send seq # */

struct dfilter tcp_dfilter;

struct pr_advice tcp_advice = 
{
    TCP_RESERVED,	/* application reserved */
    TCP_USERRESERVED,	/* user reserved */
    TCP_MAXPORT,	/* max port */
    TCP_USERRESERVED+1,	/* random last used */
    sizeof(u_short),	/* port size */
    tcp_binding_used,	/* confirmation routine */
} ;

dowedebug(inp, so, filter)
register struct inpcb	*inp;
struct socket	*so;
register struct dfilter *filter;
{
    register int		 count;

    count = 0;
    if (inp->inp_faddr.s_addr == filter->foreign_host.s_addr)
	count ++;
    if (inp->inp_fport == filter->foreign_port)
	count ++;
    if (inp->inp_laddr.s_addr == filter->local_host.s_addr)
	count ++;
    if (inp->inp_lport == filter->local_port)
	count ++;

    if (count >= filter->matches)
	so->so_options |= SO_DEBUG;
}

int tcp_noact = 0; /* patchable */

/*
 * Allocate and initialize a new TCB
 * tcp_usrreq calls tcp_attach calls us.  tcp_usrreq splnet()'s
 */
struct tcpcb *tcp_newtcpcb(inp)
register struct inpcb *inp;
{
    register struct tcpcb  *tp;
    register struct mbuf   *m;

    m = m_getclr(M_WAIT, MT_PCB);
    if (m == NULL)
	return(NULL);
    tp = mtod(m, struct tcpcb *);

    /* initialize non-zero tcb fields */

    tp->t_rcv_next	= (struct th *)tp;
    tp->t_rcv_prev	= (struct th *)tp;
    /*
     * Don't start off assuming minimum srtt/rxmitime.  If we do, and
     * TCP_tvRXMIN is small and we decide to communicate over a
     * reliable, but slow, network then we may not find true values for
     * these.  We may assume an ACK was for a retransmission that
     * we're measuring the srtt of, not the original packet.
     *
     * Instead, start high and approach from above in a deterministic
     * fashion.  We should get close to the right values fairly rapidly.
     *
     * 7/85: start from above by special casing first round trip time
     * measurement.  If srtt == 0, do not reset rtt, and do not use
     * weighted averaging.  srtt starts as time to ack(xmit [+ rxmit...])
     * and then gets smoothed with new round trip times.  This compromise
     * for getting to long-term srtt more quickly on LANs should work
     * on the Internet as well.  It will only hurt Internet connections
     * if packet loss is high, and even then would only slow getting
     * to long term srtt.
     * This method can be turned off by initializing srtt with a non-zero
     * value.
     */
    /* tp->t_srtt   = TCP_tvMAXSRTT; */
    tp->t_rxmitime	= TCP_tvMAXSRTT + 1;
    tp->t_rttltimeo	= TCP_tvRTTL;
    tp->t_xmt_val = tp->snd_end = tp->seq_fin = tp->snd_nxt =
	tp->snd_hi = tp->snd_una = tp->iss = tcp_iss;
    tcp_iss += ISSINCR;

    /*
     * Imitate Berkeley code by setting push as a default.  This should
     * increase compatibility at the user code level.
     */
    tp->t_push	 = TRUE;

    /*
     * Berkeley 4.2 code sends a data byte beyond the window's edge to see
     * if the other end is up.  If other end does not respond, connection
     * times out and aborts.  This is dangerous since the byte may make its
     * way into the input stream if the recipient is coded keeping in mind
     * how expensive packets are.
     *
     * We'll provide for an optional method to send a well formed ack that
     * will catch remote failure and generate a tcp reset.  Note that we
     * don't care if the other end ignores the ack; we only hope for a well
     * coded tcp to respond with a reset in the right circumstances.  This
     * sort of handshaking/probing should really be done at the application
     * level, but not all specs (eg., SMTP) provide for such a noop.
     *
     * Optional, since some networks charge for packets and since some might
     * see this as unecessary traffic.
     *
     * also see tcp_ioctl()
     */
    if (tp->t_noact = tcp_noact)
	tp->t_noactprobe = TRUE;

    /* attach the tcpcb to the in_pcb */

    inp->inp_ppcb = (caddr_t)tp;
    tp->t_in_pcb = inp;

    return(tp);
}

/*
 * Is a tcp port/address pair already in use by some socket on this machine?
 * Passed to in_pcbbind() to help it find a port/address binding
 * that is unique for tcp.
 */
int tcp_binding_used(inp, lport, lsaddr, reuselocal)
struct inpcb   *inp;
u_short	lport;
u_long	lsaddr;
{
    register struct inpcb *i;

    for (i = tcp.inp_next; i != &tcp; i = i->inp_next) 
    {
	/*
	 * Since our inpcb is in this linked list, don't want to know
	 * if we, ourselves, are already using this binding.
	 */
	if (i != inp)
	    if (i->inp_lport == lport)
		/*
		 * Our/His address is unbound (INADDR_ANY) iff
		 * not yet connected to foreign host.
		 */
		if ((i->inp_laddr.s_addr == lsaddr) ||
		    (i->inp_laddr.s_addr == INADDR_ANY) ||
		    (lsaddr == INADDR_ANY))
		{
		    if (!reuselocal)
			break;
		    if (i->inp_faddr.s_addr == INADDR_ANY)
			/*
			 * We're both waiting for foreign
			 * connection.  Could only re-use if
			 * he was already connected.
			 */
			break;
		}
    }
    return (i != &tcp);
}

/*
 * returns a (struct tcpcb *) cast to a (char *).  This is
 * so in_pcbconnect() can correctly handle return value. All
 * other uses promptly cast back.
 */

char *tcp_conn_used(inp, lport, lsaddr, fport, fsaddr)
struct inpcb   *inp;
u_short	lport;
u_long	lsaddr;
u_short	fport;
u_long	fsaddr;
{
    register struct inpcb *i;

    for (i = tcp.inp_next; i != &tcp; i = i->inp_next) 
    {
	/*
	 * Since our inpcb is in this linked list, don't want to know
	 * if we, ourselves, are already using this connetion.
	 */
	if (i != inp)
	    if ((i->inp_lport == lport) &&
		(i->inp_fport == fport) &&
		(i->inp_laddr.s_addr == lsaddr) &&
		(i->inp_faddr.s_addr == fsaddr))
		    return((char *)i->inp_ppcb);
    }
    return ((char *) NULL);
}

tcp_ioctl (tp, command, data)
struct tcpcb *tp;
int command;
caddr_t	data;
{
    switch (command)
    {
	/* push */
      case SIOCSPUSH:
	tp->t_push = TRUE;
	break;

      case SIOCCPUSH:
	tp->t_push = FALSE;
	break;

	/* no activity timer */
      case SIOCSNOACT:
	{
	u_long	value;

	value = *((u_long *) data);
	/*
	 * A shutdown socket should still be able to request some sort of
	 * check on the status of the remote end.  Also see tcp_newtcpcb().
	 */
	tp->t_noactprobe = (value & TCP_NOACTPROBE) ? TRUE : FALSE;
	tp->t_noactsig = (value & TCP_NOACTSIG) ? TRUE : FALSE;

	if ((tp->t_state <= ESTAB) || (tp->t_state == CLOSE_WAIT))
	{
	    /* don't interfere with system use of timer */
	    value &= ~(TCP_NOACTPROBE|TCP_NOACTSIG);
	    tp->t_noact = MIN (MAX_TCPTIMERVAL, value);
	    tp->t_timers[TNOACT] = tp->t_noact;
	}
	}
	break;

      case SIOCGNOACT:
	{
	u_long	value;

	value = tp->t_noact;
	if (tp->t_noactprobe)
	    value |= TCP_NOACTPROBE;
	if (tp->t_noactsig)
	    value |= TCP_NOACTSIG;

	*((u_long *) data) = value;
	}
	break;

	/* init timer */
      case SIOCSINIT:
	tp->t_itimeo = MIN (MAX_TCPTIMERVAL, *((unsigned *) data));
	break;

      case SIOCGINIT:
	*((int *) data) = tp->t_itimeo;
	break;

	/* retransmit took too long timer */
      case SIOCSRTTL:
	tp->t_rttltimeo = MIN (MAX_TCPTIMERVAL, *((unsigned *) data));
	break;

      case SIOCGRTTL:
	*((int *) data) = tp->t_rttltimeo;
	break;

      case SIOCABORT:
	{
	    struct socket *so;

	    /* there really should be a generic way for
	     * a user to get to soabort()
	     */

	    tp->usr_abort = TRUE;
	    /*
	     * Just in case asked to abort a LISTENing socket,
	     * Don't leave unattached, unaccepted connections.
	     */
	    so = tp->t_in_pcb->inp_socket;
	    while (so->so_q0 && (so->so_q0 != so))
		(void) soabort(so->so_q0);
	    while (so->so_q  && (so->so_q  != so))
		(void) soabort(so->so_q);

	    w_alloc(IUABORT, 0, tp, tp->t_in_pcb);
	}
	break;

      default:
	/* not our ioctl, let lower level try ioctl */
	return ip_ioctl (tp->t_in_pcb, command, data);
    }

    return (0);
}


/*
 * Process a TCP user request for TCP tb.  If this is a send request
 * then m is the mbuf chain of send data.  If this is a timer expiration
 * (called from the software clock routine), then timertype tells which timer.
 */
/*ARGSUSED*/
tcp_usrreq(so, req, m, nam, rights)
struct socket *so;
int req;
struct mbuf *m, *nam, *rights;
{
    register struct inpcb *inp;
    register struct tcpcb *tp;
    register int s;
    register int act, newstate;
    int error = 0;

    s = splnet();
    inp = sotoinpcb(so);

    /* keep in mind call from ifioctl() */
    if (rights && req != PRU_CONTROL) 
    {
	if (rights->m_len) 
	{
	    splx(s);
	    return (EINVAL);
	}
    }
    /*
     * When a TCP is attached to a socket, then there will be
     * a (struct inpcb) pointed at by the socket, and this
     * structure will point at a subsidary (struct tcpcb).
     */
    if (inp == NULL && req != PRU_ATTACH) 
    {
	splx(s);
	return (EINVAL);	/* XXX */
    }
    if (inp) 
    {
	tp = inptotcpcb(inp);
	/* WHAT IF TP IS 0? */
#ifdef GPROF
	tcp_acounts[tp->t_state][req]++;
#endif
    }

    /*
     * This switch becomes a 'caseb', so put common ones at top.
     */
    switch (req) 
    {

      case PRU_RCVD:
	/*
	 * After a receive, possibly send window update to peer.
	 */
	W_ALLOC(IURECV, 0, tp, NULL, so, act, newstate);
	break;

      case PRU_SEND:
	/*
	 * Do a send by initiating the proper entry to the FSM.
	 * Don't let urgent continue.
	 */
	tp->t_urg = FALSE;
	W_ALLOC(IUSEND, 0, tp, m, so, act, newstate);
	break;

	/*
	 * TCP attaches to socket via PRU_ATTACH, reserving space,
	 * and an internet control block.
	 */
      case PRU_ATTACH:
	if (inp) 
	{
	    error = EISCONN;
	    break;
	}
	error = tcp_attach(so);
	if (error)
	    break;
	if ((so->so_options & SO_LINGER) && so->so_linger == 0)
	    so->so_linger = T_LINGERTIME;
	tp = sototcpcb(so);
	break;

	/*
	 * PRU_DETACH detaches the TCP protocol from the socket.
	 * This is only done after SO_ISCONNECTED has been cleared.
	 */
      case PRU_DETACH:
	tcp_disconnect(tp);
	break;

	/*
	 * Give the socket an address.
	 */
      case PRU_BIND:
	error = in_pcbbind(inp, nam, &tcp_advice);
	break;

	/*
	 * Prepare to accept connections.
	 */
      case PRU_LISTEN:
	if (inp->inp_lport == 0)
	    error = in_pcbbind(inp, (struct mbuf *)0, &tcp_advice);
	if (error == 0)
	    w_alloc(IUOPENA, 0, tp, NULL);
	break;

	/*
	 * Initiate connection to peer.
	 * Bind the local end if not already.
	 * Set the routing.
	 * Crank up the TCP state machine.
	 */
      case PRU_CONNECT:
	{
	    struct in_addr laddr;

	    laddr = inp->inp_laddr;
	    if (inp->inp_lport == 0) 
	    {
		error = in_pcbbind(inp, (struct mbuf *)0, &tcp_advice);
		if (error)
		    break;
	    }
	    error = in_pcbconnect(inp, nam, tcp_conn_used);
	    if (error)
		break;

	    if (in_broadcast(inp->inp_faddr))
	    {
		in_pcbdisconnect (inp, tcp_pcbdisconnect);
		inp->inp_laddr = laddr;
		error = EADDRNOTAVAIL;
		break;
	    }

	    if (! (tp->t_template = tcp_template(tp)))
	    {
		in_pcbdisconnect (inp, tcp_pcbdisconnect);
		inp->inp_laddr = laddr;
		error = ENOBUFS;
		break;
	    }

	    tp->sws_qff = SWS_QFF_DEF;

	    /*
	     * So can debug connection problems without having to change
	     * every program or apply debugging flag to each program every
	     * time run it.
	     */
	    dowedebug(inp, so, &tcp_dfilter);

	    soisconnecting(so);
	    w_alloc(IUOPENR, 0, tp, NULL);
	}
	break;

	/*
	 * Create a TCP connection between two sockets.
	 */
      case PRU_CONNECT2:
	error = EOPNOTSUPP;
	break;

	/*
	 * Initiate disconnect from peer.
	 * If connection never passed embryonic stage, just drop;
	 * else if don't need to let data drain, then can just drop anyways,
	 * else have to begin TCP shutdown process: mark socket disconnecting,
	 * drain unread data, state switch to reflect user close, and
	 * send segment (e.g. FIN) to peer.  Socket will be really disconnected
	 * when peer sends FIN and acks ours.
	 */
      case PRU_DISCONNECT:
	tcp_disconnect(tp);
	break;

	/*
	 * Accept a connection.  Essentially all the work is
	 * done at higher levels; just return the address
	 * of the peer, storing through addr.
	 *
	 * BBN-NOTE: upper levels do all the waiting;  this stays the same.
	 */
      case PRU_ACCEPT: 
	{
	    struct sockaddr_in *sin = mtod(nam, struct sockaddr_in *);

	    nam->m_len = sizeof (struct sockaddr_in);
	    sin->sin_family = AF_INET;
	    sin->sin_port = inp->inp_fport;
	    sin->sin_addr = inp->inp_faddr;
	    break;
	}

	/*
	 * Mark the connection as being incapable of further output.
	 */
      case PRU_SHUTDOWN:
	socantsendmore(so);
	if (! tp->usr_closed)
	    w_alloc(IUCLOSE, 0, tp, inp);
	break;

	/*
	 * Abort the TCP.
	 */
      case PRU_ABORT:
	w_alloc(IUABORT, 0, tp, inp);
	break;

      case PRU_CONTROL:
	error = tcp_ioctl(tp, (int) m, (caddr_t) nam);
	break;


/* SOME AS YET UNIMPLEMENTED HOOKS */
      case PRU_SENSE:
	error = EOPNOTSUPP;
	break;
/* END UNIMPLEMENTED HOOKS */

      case PRU_RCVOOB:

	{
	    int	desired;

	    if (so->so_oobmark == 0 && (so->so_state & SS_RCVATMARK) == 0) 
	    {
		error = EINVAL;
		break;
	    }
	    if (tp->oob_data == NULL) 
	    {
		error = EWOULDBLOCK;
		break;
	    }
	    desired = *(mtod(m, int *));

	    while ((desired > 0) && (tp->oob_data))
	    {
		char	*p;
		unsigned count;

		p = mtod(m, caddr_t);
		count = MIN(desired, tp->oob_data->m_len);
		count = MIN(count, MLEN);
		bcopy(mtod(tp->oob_data, caddr_t), p, count);
		m->m_len = count;
		desired -= count;

		tp->oob_data->m_len -= count;
		tp->oob_data->m_off += count;
		if (tp->oob_data->m_len <= 0)
		    tp->oob_data = m_free(tp->oob_data);

		if ((desired > 0) && (tp->oob_data))
		{
		    m->m_next = m_get(M_WAIT, MT_DATA);
		    m = m->m_next;
		}
	    }

	}
	break;

      case PRU_SENDOOB:
	/*
	 * allows up to MAX_TCPOOB bytes of out of band data
	 * even if user has used up all his allocated space.
	 */
	if (sbspace(&so->so_snd) < (- MAX_TCPOOB)) 
	{
	    m_freem(m);
	    error = ENOBUFS;
	    break;

	}
	tp->t_urg = TRUE;
	w_alloc(IUSEND, 0, tp, m);
	break;

	/*
	 * Return the address of this socket (local-side binding)
	 */
      case PRU_SOCKADDR:
	in_setsockaddr(inp, nam);
	break;

      case PRU_PEERADDR:
	in_setpeeraddr(inp, nam);
	break;

	/*
	 * TCP slow timer went off; run down all those timers.
	 */
      case PRU_SLOWTIMO:
	tcp_timeo();
	break;

      default:
	panic("tcp_usrreq");
    }
    splx(s);
    return (error);
}

/*
 * getsockopt() / setsockopt()
 */
tcp_ctloutput (req,so,level,optname,optval)
int req;
struct socket *so;
int level, optname;
struct mbuf **optval;
{
    int s = splnet(); /* like PRU/packet/timer entry into net code */
    int error;
    struct inpcb *inp;

    /*
     * possibly for us?
     * Follow Berkeley methods: level is protocol number if meant for the
     * protocol layer.  (Why not say if=0, arp=1, ip=2, udp/tcp/rdp=3....?)
     *
     * Problem: tcp needs to know about IP options in order to use right
     * maxseg.  This doesn't quite work with the layering.
     *
     * Why not combine ioctl/setsockopt/getsockopt paths, since ioctl can be
     * seen as fixed size sockopt- tried at BBN; removed for 4.3
     */

    /* should be "mature" socket so pointers all valid... */
    inp = sotoinpcb(so);

    switch(req)
    {
	case PRCO_GETOPT:
	    error = tcp_getopt (inp, optname, optval);
	    break;

	case PRCO_SETOPT:
	    error = tcp_setopt (inp, optname, optval);
	    break;
	 
	default:
	    panic("tcp_ctloutput");
    }

    splx(s);
    return (error);
}

tcp_getopt (inp, command, data)
struct inpcb	*inp;
struct mbuf	**data;
{
    /*
     * no TCP specific options accessed by getsockopt() as yet.
     * let lower level at cmd
     */
    return ip_getopt (inp, command, data);
}

tcp_setopt (inp, command, data)
struct inpcb	*inp;
struct mbuf	**data;
{
    int error;
    struct tcpcb *tp;

    /* no TCP specific options accessed by setsockopt() as yet */
    tp = inptotcpcb(inp);

    if (command == SO_IPROUTE)
	tp->t_maxseg += inp->inp_optlen;

    error =  ip_setopt(inp, command, data);

    if (command == SO_IPROUTE)
	tp->t_maxseg -= inp->inp_optlen;

    return (error);
}

/*
 * These numbers come from measurements described in the paper
 *	"Converting the BBN TCP/IP to 4.2BSD"  (S.L.C. USENIX)
 * If your network handles packets larger than an ethernet frame, you
 * could change tcp_init back to determine the largest net's packet size,
 * multiply that by some number, and round up to a multiple of a CLSIZE.
 */
int	tcp_recvspace = 4096;
int	tcp_sendspace = 4096;

/*
 * Attach TCP protocol to socket, allocating
 * internet protocol control block, tcp control block, buffer space.
 */
tcp_attach(so)
struct socket *so;
{
    register struct tcpcb *tp;
    struct inpcb *inp;
    int error;

    if (! (error = soreserve(so, tcp_sendspace, tcp_recvspace))) 
    {
	if (! (error = in_pcballoc(so, &tcp))) 
	{
	    inp = sotoinpcb(so);
	    if (tp = tcp_newtcpcb(inp))
	    {
		/*
		 * Should change state tables to have an UNOPENED state like
		 * the butterfly's which is different from SAME.
		 */
		tp->t_state = 0;
		return (0);
	    }
	    error = ENOBUFS;
	    in_pcbdetach(inp, (int (*)())0);
	}
    }
    return (error);
}

/*
 * Initiate (or continue) disconnect.
 * If embryonic state, just send reset (once).
 * If not in ``let data drain'' option, just drop.
 * Otherwise (hard), mark socket disconnecting and drop
 * current input data; switch states based on user close, and
 * send segment to peer (with FIN).
 */

tcp_disconnect(tp)
register struct tcpcb *tp;
{
    struct socket *so = tp->t_in_pcb->inp_socket;

    soisdisconnecting(so);
    sbflush(&so->so_rcv);
    tp->usr_abort = TRUE;
    if (!tp->usr_closed)
	w_alloc(IUCLOSE, 0, tp, tp->t_in_pcb);
}

tcp_init()
{
    /*
     * Leave these checks in!  It's a pain in the ass to find out
     * problems caused by too small mbufs if someone changes the
     * size of an mbuf.
     */
    if (sizeof(struct inpcb) > MLEN)
	panic("inpcb too big");

    if (sizeof(struct socket) > MLEN)
	panic("socket too big");

    if (sizeof(struct th) > MLEN)
	panic("th too big");

    if (sizeof(struct tcpcb) > MLEN)
	panic("tcpcb too big");

    if (sizeof(struct t_debug) > MLEN)
	panic("t_debug too big");

    /* init queue */
    tcp.inp_next = tcp.inp_prev = &tcp;

    /* are only 4 things to match. turn off for now */
    tcp_dfilter.matches = 5;

    tcp_iss = time.tv_sec;

    ipsw[IPPROTO_TCP].ipsw_hlen = sizeof(struct th);
}

tcp_ctlinput (prc_code, arg)
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
	    register struct th	*tp;
	    struct tcpcb	*t;

	    tp = (struct th *) (&((struct icmp *) arg)->ic_iphdr);
	    t = (struct tcpcb *)tcp_conn_used ((struct inpcb *) 0,
		tp->t_src, tp->t_s.s_addr,
		tp->t_dst, tp->t_d.s_addr);
	    if (t)
		t_close(t, error);
	    }
	    break;

	case PRC_UNREACH_NET:
	case PRC_UNREACH_HOST:
	    {
	    register struct th	*tp;
	    struct tcpcb	*t;

	    tp = (struct th *) (&((struct icmp *) arg)->ic_iphdr);
	    t = (struct tcpcb *)tcp_conn_used ((struct inpcb *) 0,
		tp->t_src, tp->t_s.s_addr,
		tp->t_dst, tp->t_d.s_addr);
	    if (t)
	    {
		struct socket *so;

		so = t->t_in_pcb->inp_socket;
		if ((so->so_state & SS_NOFDREF) == 0)
		    advise_user(so, error);
		else
		    t_close(t, error);
	    }
	    }
	    break;

	case PRC_GWDOWN:
	    in_gdown (&tcp, (u_long) arg);
	    break;

	case PRC_REDIRECT_NET:	/* icmp message */
	case PRC_REDIRECT_HOST:
	    {
	    struct tcpcb	*t;
	    register struct th	*tp;

	    tp = (struct th *) (&((struct icmp *) arg)->ic_iphdr);
	    t = (struct tcpcb *)tcp_conn_used ((struct inpcb *) 0,
		tp->t_src, tp->t_s.s_addr,
		tp->t_dst, tp->t_d.s_addr);
	    if (t)
		icmp_redirect_inp(t->t_in_pcb, (struct icmp *) arg,
		    prc_code == PRC_REDIRECT_NET ? rtnet : rthost);
	    }
	    break;

	case PRC_TIMXCEED_INTRANS:	/* icmp message */
	case PRC_TIMXCEED_REASS:
	case PRC_PARAMPROB:
	    break;

	case PRC_QUENCH:	/* icmp message */
	    /*
	     * See RFC 896.  The idea is, when we get a source quench message on
	     * a connection we should send fewer packets.  This ties in with the
	     * silly window syndrome whose solution is to send fewer, larger packets.
	     * Deal with quenches by altering threshold used by silly window
	     * syndrome.  This is similar to acting as if the window is smaller
	     * than it actually is for deciding when to send, except that when we
	     * do, we use as much as there really is.
	     */
	    {
	    register struct th	*tp;
	    struct tcpcb	*t;

	    tp = (struct th *) (&((struct icmp *) arg)->ic_iphdr);
	    t = (struct tcpcb *)tcp_conn_used ((struct inpcb *) 0,
		tp->t_src, tp->t_s.s_addr,
		tp->t_dst, tp->t_d.s_addr);
	    if (t)
	    {
		t->sws_qff -= SWS_QFF_DEC;
		if (t->sws_qff < SWS_QFF_MIN)
		    t->sws_qff = SWS_QFF_MIN;
	    }
	    }
	    break;

	case PRC_IFDOWN:
	    {
	    u_long addr;

	    addr = ((struct sockaddr_in *)(arg))->sin_addr.s_addr;
	    inpcb_notify(&tcp, addr, (u_long) 0, error);
	    inpcb_notify(&tcp, (u_long) 0, addr, error);
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
	    in_gdown (&tcp, addr);
	    inpcb_notify(&tcp, (u_long) 0, addr, error);
	    }
	    break;

	default:
	    panic("tcp_ctlinput");
    }
}
