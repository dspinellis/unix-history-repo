/*
 $Log:	rdp_usrreq.c,v $
 * Revision 2.8  85/05/30  11:54:12  walsh
 * initialize r_srtt.
 * 
 * Revision 2.7  85/02/26  08:27:02  walsh
 * First pass at using IP source routing information to establish connections
 * (possibly with hosts not known by the Internet gateways.)  The hooks with
 * TCP could be better done - particularly dealing with IP addresses in the
 * header for checksums and tcpdb lookups.
 * 
 * Revision 2.6  84/11/29  12:51:00  walsh
 * changed references to currentrtt into references to srtt, a better
 * and less misleading mnemonic.
 * 
 * Revision 2.5  84/11/08  16:13:17  walsh
 * Added code to gather statistics on RDP traffic.  This makes the RDPCB
 * too big unles you make mbufs 512 bytes large.  RDP_CS should be turned off
 * unless you do.
 * 
 * Revision 2.4  84/11/05  12:41:29  walsh
 * Set things up so can debug RDP connections just like can debug TCP
 * connections.
 * 
 * Revision 2.3  84/11/05  11:05:42  walsh
 * comment and adjust number for rdp_iss in a mathematically correct way
 * as a result of benchmarks (cf. operationally correct).
 * 
 * Revision 2.2  84/11/05  10:49:01  walsh
 * More changes to go with NULL messages getting their own sequence
 * number.
 * 
 * Revision 2.1  84/11/02  10:16:02  walsh
 * Fixed to include RCS comments in checked out source.
 * 
 *
 * description:
 * The user system call interface to RDP.
 * 
 * revision 1.11        
 * date: 84/07/20 12:35:26;  author: root;  state: Exp;  lines added/del: 2/2
 * fix syntax error.
 * 
 * revision 1.10        
 * date: 84/07/20 11:25:53;  author: walsh;  state: Exp;  lines added/del: 3/2
 * Don't let user use unreasonable (too small) retranmit took too long timers.
 * 
 * revision 1.9        
 * date: 84/07/19 10:22:59;  author: walsh;  state: Exp;  lines added/del: 2/1
 * Organized macros and classified their definitions in rdp_macros.h.
 * 
 * revision 1.8        
 * date: 84/07/18 18:51:41;  author: walsh;  state: Exp;  lines added/del: 29/1
 * Added provision for sending of NULL messages.  These are sent on an idle
 * connection to determine that the other side still exists.
 * 
 * revision 1.7        
 * date: 84/07/18 13:49:19;  author: walsh;  state: Exp;  lines added/del: 19/1
 * RTTL timer is now user alterable.
 * 
 * revision 1.6        
 * date: 84/07/17 22:42:08;  author: walsh;  state: Exp;  lines added/del: 7/3
 * Can't connect to port numbers greater than RDP_pMAX.
 *
 * revision 1.5        
 * date: 84/07/12 13:48:38;  author: walsh;  state: Exp;  lines added/del: 2/1
 * Rather than in-line stuffing of IP/RDP headers, at least half of which are
 * constant, copy headers in from a template of what the headers are like.  The
 * bcopy() call is turned into a movc3 instruction on the VAX by a sed script
 * run over the assembler output of the C compiler.  Marginal speed-up.
 * 
 * revision 1.4        
 * date: 84/07/10 10:45:38;  author: walsh;  state: Exp;  lines added/del: 20/19
 * neatened up some formatting.
 * 
 * revision 1.3        
 * date: 84/07/06 14:41:15;  author: wjacobso;  state: Exp;  lines added/del: 11/3
 * use RSP_ACTION macro instead of rdp_action
 * 
 * revision 1.2        
 * date: 84/07/06 09:51:35;  author: root;  state: Exp;  lines added/del: 56/17
 * This version seems to run bug-free.
 * 
 * revision 1.1        
 * date: 84/06/26 14:18:47;  author: walsh;  state: Exp;  
 * Initial revision
 */



#ifdef RDP
#ifdef	RCSIDENT
static char rcsident[] = "$Header: rdp_usrreq.c,v 2.8 85/05/30 11:54:12 walsh Exp $";
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
#include "../bbnnet/net.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/icmp.h"
#include "../bbnnet/rdp.h"
#include "../bbnnet/rdp_macros.h"

/*
 * RDP protocol interface to socket abstraction.
 */

/*
 * misc data structures
 */

struct inpcb rdp;
struct rdp_stat rdpstat;

struct dfilter	rdp_dfilter;
rdpsequence rdp_iss;

/*
 * RDP port allocation information
 */

extern rdp_binding_used();

struct pr_advice rdp_advice = 
{
    RDP_RESERVED,
    RDP_USERRESERVED,
    RDP_MAXPORT,
    RDP_USERRESERVED+1,
    sizeof(u_char),
    rdp_binding_used
} ;


/*
 * Allocate and initialize a new RDPCB
 * rdp_usrreq calls rdp_attach calls us.  rdp_usrreq splnet()'s
 *
 * Default allocation for kernel receive buffering is
 * (rdp_ournbuf * rdp_ourmaxlen) bytes.
 */
int rdp_ournbuf = 8;
int rdp_ourmaxlen = IPMAX - HDRSLOP;

rdp_newrdpcb(inp)
register INPCB	*inp;
{
    register RDPCB	*rdpcb;
    register MBUF	*m;
    MBUF	*mrq, *msq;

    m	= m_getclr(M_WAIT, MT_PCB);
    mrq	= m_getclr(M_WAIT, MT_PCB);
    msq	= m_getclr(M_WAIT, MT_PCB);
    if ((m == NULL) || (mrq == NULL) || (msq == NULL)) 
    {
	if (m)
	    m_free(m);
	if (mrq)
	    m_free(mrq);
	if (msq)
	    m_free(msq);
	return(ENOBUFS);
    }

    rdpcb = mtod(m, RDPCB *);

    /* initialize non-zero tcb fields */

    rdpcb->r_sendq.rq_msgs	= mtod(msq, MBUF **);
    rdpcb->r_rcvq.rq_msgs	= mtod(mrq, MBUF **);
    rdpcb->r_state		= RDP_sUNOPENED;
#ifdef RDP_CS
    rdpcb->r_entered[RDP_sUNOPENED] = iptime();
#endif
    rdpcb->r_ournbuf	= MAX(1, MIN(RDP_MAXDGRAMS, rdp_ournbuf));
    rdpcb->r_hisnbuf	= 1;
/*  rdpcb->r_synrcvd	= FALSE;	*/
/*  rdpcb->r_synacked	= FALSE;	*/
/*  rdpcb->r_usrclosed	= FALSE;	*/
/*  rdpcb->r_rttiming	= FALSE;	*/
    rdpcb->r_sequential	= TRUE;
    rdpcb->r_closewait	= RDP_tvCLOSEWAIT;
    rdpcb->r_rttl	= RDP_tvRTTL;
    rdpcb->r_tvnull	= RDP_tvNULL;
    rdpcb->r_srtt	= RDP_tvRXMIN; /*###*/
    rdpcb->r_rxmitime	= rdpcb->r_srtt + 1;
    rdpcb->r_rttlindex	= (-1);
    rdpcb->r_iss = rdp_iss;
    rdpcb->r_sndnxt = rdpcb->r_snduna = rdpcb->r_iss +1;
    rdp_iss += RDP_ISSINCR;

    /* attach the protocol specific pcb to the generic internet pcb */
    inp->inp_ppcb = (caddr_t)rdpcb;
    rdpcb->r_inpcb = inp;

    /*
     * User has until listen(2) or connect(2) to increase max dgram
     * size we will accept.  He does this by adjusting his socket's
     * amount of receive buffering.
     */
    sbreserve (&rdpcb->r_inpcb->inp_socket->so_rcv, rdp_ourmaxlen);
    pick_ourmaxlen(rdpcb);

    return(0);
}

rdp_pcbdisconnect(inp)
INPCB	*inp;
{
    register RDPCB	*rdpcb;
    register MBUF	*m;
    register int	 i;

    if (rdpcb = (RDPCB *) inp->inp_ppcb)
    {
	inp->inp_ppcb = (caddr_t) NULL;

	/*
	 * free all data on receive and send qs
	 * Remember, due to EACKS, send q may contain non-NULL
	 * RDP_DELIVERED pointers.
	 * If we close while we're retransmitting a NULL message,
	 * may have one of those on our send queue.
	 */
	for (i=0; i<RDP_MAXDGRAMS; i++)
	{
	    if (m = rdpcb->r_sendq.rq_msgs[i])
		if ((m != RDP_DELIVERED) && (m != RDP_NULLMSG))
		    m_freem(m);
	    if (m = rdpcb->r_rcvq.rq_msgs[i])
		if (m != RDP_DELIVERED)  /* just in case */
		    m_freem(m);
	}
	m_free(dtom(rdpcb->r_sendq.rq_msgs));
	m_free(dtom(rdpcb->r_rcvq.rq_msgs));

	m_free(dtom(rdpcb));
    }
}

/*
 * Is a rdp port/address pair already in use by some socket on this machine?
 * Passed to in_pcbbind() to help it find a port/address binding
 * that is unique for rdp.
 */
int rdp_binding_used(inp, lport, laddr, reuselocal)
INPCB	*inp;
rdpportnum lport;
u_long laddr;
{
    register INPCB		*i;

    for(i = rdp.inp_next; i != &rdp; i = i->inp_next)
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
		if ((i->inp_laddr.s_addr == laddr) ||
		    (i->inp_laddr.s_addr == INADDR_ANY) ||
		    (laddr == INADDR_ANY))
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
    return (i != &rdp);
}

char *rdp_conn_used(inp, lport, laddr, fport, faddr)
INPCB	*inp;
rdpportnum lport;
u_long laddr;
rdpportnum fport;
u_long faddr;
{
    register INPCB		*i;

    for(i = rdp.inp_next; i != &rdp; i = i->inp_next)
    {
	/*
	 * Since our inpcb is in this linked list, don't want to know
	 * if we, ourselves, are already using this connetion.
	 */
	if (i != inp)
	    if ((i->inp_lport == lport) &&
	    (i->inp_fport == fport) &&
	    (i->inp_laddr.s_addr == laddr) &&
	    (i->inp_faddr.s_addr == faddr))

		return((char *)(i->inp_ppcb));
    }
    return ((char *) NULL);
}

rdp_ioctl (rdpcb, command, data)
RDPCB *rdpcb;
int command;
caddr_t data;
{
    switch (command)
    {
      case SIOCGNDGRAMS:
	*((int *) data) = rdpcb->r_ournbuf;
	break;

      case SIOCSNDGRAMS:
	if ((rdpcb->r_state == RDP_sUNOPENED) && (*((int *) data) > 0))
	    rdpcb->r_ournbuf = MIN (*((int *) data), RDP_MAXDGRAMS);
	else
	    return EINVAL;
	break;


      case SIOCGSEQ:
	*((int *) data) = rdpcb->r_sequential;
	break;

      case SIOCSSEQ:
	if (rdpcb->r_state == RDP_sUNOPENED)
	    rdpcb->r_sequential = *((int *) data) ? TRUE : FALSE;
	else
	    return EINVAL;
	break;


      case SIOCGRTTL:
	*((int *) data) = rdpcb->r_rttl;
	break;

      case SIOCSRTTL:
	{
	    /*
	     * Allow user to set r_rttl to 0 to disable.
	     */
	    unsigned int	newvalue;

	    newvalue = *((unsigned int *) data);
	    if ((newvalue > RDP_MAXTIMERVAL) ||
		(newvalue && (newvalue < MIN(rdpcb->r_srtt, rdpcb->r_rxmitime))))
		return EINVAL;
	    else
		rdpcb->r_rttl = newvalue;
	}
	break;

	/*
	 * Problem with socket level KEEPALIVES is that timer
	 * would be constant for all connections.
	 */
      case SIOCGNULL:
	*((int *) data) = rdpcb->r_tvnull;
	break;

      case SIOCSNULL:
	{
	    /*
	     * Allow user to set to 0 to disable.
	     */
	    unsigned int	newvalue;

	    newvalue = *((unsigned int *) data);
	    if ((newvalue > RDP_MAXTIMERVAL) ||
		(newvalue && (newvalue < rdpcb->r_rttl)))
		return EINVAL;
	    else
		rdpcb->r_tvnull = newvalue;
	}
	break;

      default:
	/* not our ioctl, let lower level try ioctl */
	return ip_ioctl (rdpcb->r_inpcb, command, data);
    }

    return (0);
}

/*
 * Process a RDP user request (system call).
 */
/*ARGSUSED*/
rdp_usrreq(so, req, m, nam, rights)
struct socket *so;
int req;
struct mbuf *m, *nam, *rights;
{
    register RDPCB	*rdpcb;
    register struct inpcb *inp;
    register int s;
    int error = 0;

    s = splnet();
    inp = sotoinpcb(so);

    if (rights && rights->m_len) 
    {
	splx(s);
	return (EINVAL);
    }
    /*
     * When an RDPCB is attached to a socket, then there will be
     * an INPCB pointed at by the socket, and this
     * structure will point at a subsidary RDPCB.
     */
    if (inp == 0 && req != PRU_ATTACH) 
    {
	splx(s);
	return (EINVAL);	/* XXX */
    }
    if (inp)
	rdpcb = (RDPCB *) inp->inp_ppcb;

    /*
     * This switch becomes a 'caseb', so put common ones at top.
     */
    switch (req) 
    {

      case PRU_RCVD:
	/*
	 * After user has received message, ack the message. read(2)
	 */
	{
	    register rdpstate newstate;

	    RDP_ACTION(RDP_iRCV, rdpcb, 0, newstate);
	}
	break;

      case PRU_SEND:
	/*
	 * Send this message. write(2)
	 */
	{
	    register rdpstate newstate;

	    RDP_ACTION(RDP_iSEND, rdpcb, ((int) m), newstate);
	}
	break;

      case PRU_ATTACH:
	/*
	 * set up protocol control blocks.  socket(2)
	 */
	if (inp) 
	{
	    error = EISCONN;
	    break;
	}
	if (error = rdp_attach(so))
	    break;

	/*
	 * so_linger doesn't affect anything I know of in the socket level
	 * -- see soclose().  Maybe this is one of those someday things.
	 */
	if ((so->so_options & SO_LINGER) && so->so_linger == 0)
		so->so_linger = 120;

	rdpcb = (RDPCB *) ((INPCB *) so->so_pcb)->inp_ppcb;
	break;

      case PRU_DETACH:
	/*
	 * close(2) the connection
	 */
	rdp_close(rdpcb);
	break;

      case PRU_BIND:
	/*
	 * Give the socket an address.  bind(2)
	 */
	error = in_pcbbind(inp, nam, &rdp_advice);
	break;

      case PRU_LISTEN:
	/*
	 * Prepare to accept connections.  Passive open.  listen(2)
	 */
	if (inp->inp_lport == 0)
	    if (error = in_pcbbind(inp, (MBUF *)0, &rdp_advice))
		break;

	pick_ourmaxlen(rdpcb);
	rdp_action(RDP_iLISTEN, rdpcb, 0);
	break;

      case PRU_CONNECT:
	/*
	 * Active open.  connect(2).  Initiate connection to peer.
	 * Bind the local end if not already.  Set the routing.
	 * Crank up the state machine.
	 */
	{
	    struct sockaddr_in *sin;

	    /*
	     * ensure foreign address might be valid.
	     * Can't connect to broadcast address...
	     */
	    sin = mtod(nam, struct sockaddr_in *);
	    if ((in_broadcast(sin->sin_addr)) ||
		(sin->sin_port > RDP_MAXPORT)) 
	    {
		error = EADDRNOTAVAIL;
		break;
	    }

	    if (inp->inp_lport == 0) 
		if (error = in_pcbbind(inp, (MBUF *)0, &rdp_advice))
		    break;
	    if (error = in_pcbconnect(inp, nam, rdp_conn_used))
		break;

	    /*
	     * So can debug connection problems without having to change
	     * every program or apply debugging flag to each program every
	     * time run it.
	     */
	    dowedebug(inp, so, &rdp_dfilter);

	    soisconnecting(so);
	    pick_ourmaxlen(rdpcb);
	    rdp_template(rdpcb);
	    rdp_action(RDP_iCONNECT, rdpcb, 0);
	}
	break;

	/*
	 * Create a TCP connection between two sockets.
	 */
      case PRU_CONNECT2:
	error = EOPNOTSUPP;
	break;

      case PRU_DISCONNECT:
	/*
	 * close(2)
	 */
	rdp_close(rdpcb);
	break;

      case PRU_ACCEPT:
	/*
	 * accept(2).  Socket code has waited until a new connection
	 * is available for the listener/server.  Now that there is
	 * one, we just tell them who it is.
	 */
	{
	    struct sockaddr_in *sin = mtod(nam, struct sockaddr_in *);

	    nam->m_len = sizeof (struct sockaddr_in);
	    sin->sin_family = AF_INET;
	    sin->sin_port = inp->inp_fport;
	    sin->sin_addr = inp->inp_faddr;
	}
	break;

      case PRU_SHUTDOWN:
	/*
	 * user has shutdown(2) for writing.  This is a friendly close;
	 * the user may still want to read.
	 */
	socantsendmore(so);
	break;

      case PRU_ABORT:
	/*
	 * abort un-accept(2)ed connections when close listening socket
	 * act as if it was accepted and closed.  Remove socket from
	 * parent socket's qs so that not hang in soclose()
	 */

	/* accept */
	if (! soqremque(so, 0)) /* SYNSENT, LSYNRCVD */
	    if (! soqremque(so, 1)) /* ESTAB */
		panic("rdp ABORT");

	/* close */
	rdp_close(rdpcb);
	break;

      case PRU_CONTROL:
	error = rdp_ioctl(rdpcb, (int) m, (caddr_t) nam);
	break;

/* SOME AS YET UNIMPLEMENTED HOOKS */
      case PRU_SENSE:
	error = EOPNOTSUPP;
	break;
/* END UNIMPLEMENTED HOOKS */

      case PRU_RCVOOB:
      case PRU_SENDOOB:
	error = EOPNOTSUPP;
	break;

      case PRU_SOCKADDR:
	/*
	 * Tell user his (local) address/binding
	 */
	in_setsockaddr(inp, nam);
	break;

      case PRU_PEERADDR:
	in_setpeeraddr(inp, nam);
	break;

#ifdef neverdef
      case PRU_SLOWTIMO:
	rdp_timeo();
	break;
#endif

      default:
	panic("rdp_usrreq");
    }
    splx(s);
    return (error);
}

/*
 * get/setsockopt() handler
 */

rdp_ctloutput(req,so,level,optname,optval)
int req;
struct socket *so;
int level, optname;
struct mbuf **optval;
{
    int s = splnet(); /* like PRU/packet/timer entry into net code */
    int error;
    struct inpcb *inp;

    /*
     * See comments by tcp_ctloutput()
     */
    if (level == IPPROTO_RDP)
    {
	inp = sotoinpcb(so);

	switch(optname)
	{
	  case PRCO_GETOPT:
	    error = rdp_getopt(inp, optname, optval);
	    break;

	  case PRCO_SETOPT:
	    error = rdp_setopt(inp, optname, optval);
	    break;

	  default:
	    panic("rdp_ctloutput");
	}
    } else
        error = ip_ctloutput(req,so,level,optname,optval);

    splx(s);
    return (error);
}

rdp_setopt (inp, command, data)
struct inpcb	*inp;
struct mbuf	**data;
{
    /* no RDP specific options accessed by setsockopt() as yet */
    return EOPNOTSUPP;
}

rdp_getopt (inp, command, data)
struct inpcb	*inp;
struct mbuf	**data;
{
    /* no RDP specific options accessed by getsockopt() as yet */
    return EOPNOTSUPP;
}

/*
 * attach rdp protocol to socket
 */
rdp_attach(so)
struct socket *so;
{
    struct inpcb *inp;
    int error;

    if ((error = in_pcballoc(so,&rdp)) == 0)
    {
	inp = sotoinpcb(so);
	if (error = rdp_newrdpcb(inp))
	    in_pcbdetach(inp,(int(*)())0);
    }
    return(error);
}



/*
 * Initiate (or continue) disconnect.  close(2).
 */
rdp_close(rdpcb)
register RDPCB *rdpcb;
{
    struct socket *so;

    if (! rdpcb->r_usrclosed)
    {
	rdpcb->r_usrclosed = TRUE;
	so = rdpcb->r_inpcb->inp_socket;
	soisdisconnecting(so);
	sbflush(&so->so_rcv);
	rdp_action(RDP_iUCLOSE, rdpcb, 0);
    }
}

rdp_init()
{
    /*
     * Leave these checks in!  It's a pain in the ass to find out
     * problems caused by too small mbufs if someone changes the
     * size of an mbuf.
     */
    if (sizeof(RDPCB) > MLEN)
	panic("rdpcb too big");

    if (sizeof(R_DEBUG) > MLEN)
	panic("r_debug too big");

    if (sizeof(RDPHDR) + sizeof(struct ip) > MLEN)
	panic("rdphdr too big");

    /*
     * When send a packet, we allocate an mbuf for options and assume
     * they'll always fit.
     */
    if (sizeof(EACKOPTIONS) * RDP_MAXDGRAMS > MLEN)
	panic("RDP_MAXDGRAMS too big");

    /*
     * rq_msgs is assumed to fit within a single mbuf.
     */
    if (sizeof(MBUF *) * RDP_MAXDGRAMS > MLEN)
	panic("RDP_MAXDGRAMS too big 2");

    /*
     * When receive a packet, IP hdr + RDP hdr + RDP options pulled up
     * into a single mbuf and later assumed to be contiguous.  We'd like
     * to avoid deadlock on a connection leading to a timeout failure of
     * the connection.  Also, later just before we pass the packet to the
     * user, we trim off the headers assuming they're in one mbuf.
     *
     * This superceeds a few of the above, but if we change things, the
     * separate listing will make things easier.
     */
    if (sizeof(struct ip)+sizeof(RDPHDR)+(sizeof(EACKOPTIONS)*RDP_MAXDGRAMS)
	> MLEN)
	panic("RDP_MAXDGRAMS too big 3");

    rdp_iss = time.tv_sec;

    rdp.inp_next = rdp.inp_prev = &rdp;

    /* are only 4 things to match. turn off for now */
    rdp_dfilter.matches = 5;

    ipsw[IPPROTO_RDP].ipsw_hlen = sizeof(struct ip) + RDPHDRSZ;
}

rdp_ctlinput (prc_code, arg)
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
	    RDPHDR *pkt;
	    RDPCB *rdpcb;
	    struct ip *ip;

	    ip = (struct ip *) (&((struct icmp *) arg)->ic_iphdr);
	    pkt = (RDPHDR *) (ip+1);
	    rdpcb = (RDPCB *) rdp_conn_used((struct inpcb *) NULL,
		pkt->rh_sport, ip->ip_src.s_addr,
		pkt->rh_dport, ip->ip_dst.s_addr);

	    if (rdpcb)
	    {
		rdpcb->r_inpcb->inp_socket->so_error = error;
		rdp_close(rdpcb);
	    }
	    }
	    break;

	case PRC_UNREACH_NET:
	case PRC_UNREACH_HOST:
	    {
	    RDPHDR *pkt;
	    RDPCB *rdpcb;
	    struct ip *ip;

	    ip = (struct ip *) (&((struct icmp *) arg)->ic_iphdr);
	    pkt = (RDPHDR *) (ip+1);
	    rdpcb = (RDPCB *) rdp_conn_used((struct inpcb *) NULL,
		pkt->rh_sport, ip->ip_src.s_addr,
		pkt->rh_dport, ip->ip_dst.s_addr);

	    if (rdpcb)
	    {
		struct socket *so;

		so = rdpcb->r_inpcb->inp_socket;
		if ((so->so_state & SS_NOFDREF) == 0)
		    advise_user(so, error);
		else
		{
		    so->so_error = error;
		    rdp_close(rdpcb);
		}
	    }
	    }
	    break;

	case PRC_GWDOWN:
	    in_gdown (&rdp, (u_long) arg);
	    break;

	case PRC_REDIRECT_NET:	/* icmp message */
	case PRC_REDIRECT_HOST:
	    {
	    RDPHDR *pkt;
	    RDPCB *rdpcb;
	    struct ip *ip;

	    ip = (struct ip *) (&((struct icmp *) arg)->ic_iphdr);
	    pkt = (RDPHDR *) (ip+1);
	    rdpcb = (RDPCB *) rdp_conn_used((struct inpcb *) NULL,
		pkt->rh_sport, ip->ip_src.s_addr,
		pkt->rh_dport, ip->ip_dst.s_addr);

	    if (rdpcb)
		icmp_redirect_inp(rdpcb->r_inpcb, (struct icmp *) arg,
		     prc_code == PRC_REDIRECT_NET ? rtnet : rthost);
	    }
	    break;

	case PRC_TIMXCEED_INTRANS:	/* icmp message */
	case PRC_TIMXCEED_REASS:
	case PRC_PARAMPROB:
	    break;

	case PRC_QUENCH:	/* icmp message */
	    /*
	     * Reduce the traffic on this connection, so the gateway is happy.
	     * Since can't change message size, must change frequency.  If continue
	     * to send it straight out in response to write(2), can only tweak
	     * retransmission period.
	     *
	     * This will allow the gateway to relax until things flow again and we
	     * calculate another round trip time.
	     */
	    {
	    RDPHDR *pkt;
	    RDPCB *rdpcb;
	    struct ip *ip;

	    ip = (struct ip *) (&((struct icmp *) arg)->ic_iphdr);
	    pkt = (RDPHDR *) (ip+1);
	    rdpcb = (RDPCB *) rdp_conn_used((struct inpcb *) NULL,
		pkt->rh_sport, ip->ip_src.s_addr,
		pkt->rh_dport, ip->ip_dst.s_addr);
	    if (rdpcb)
		rdpcb->r_rxmitime = MIN(rdpcb->r_rxmitime +1, RDP_tvRXMAX);
	    }
	    break;

	case PRC_IFDOWN:
	    {
	    u_long addr;

	    addr = ((struct sockaddr_in *)(arg))->sin_addr.s_addr;
	    inpcb_notify(&rdp, addr, (u_long) 0, error);
	    inpcb_notify(&rdp, (u_long) 0, addr, error);
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
	    in_gdown (&rdp, addr);
	    inpcb_notify(&rdp, (u_long) 0, addr, error);
	    }
	    break;

	default:
	    panic("rdp_ctlinput");
    }
}
#endif
