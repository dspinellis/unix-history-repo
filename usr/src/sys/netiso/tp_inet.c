/***********************************************************
		Copyright IBM Corporation 1987

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of IBM not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * ARGO Project, Computer Sciences Dept., University of Wisconsin - Madison
 */
/* 
 * ARGO TP
 * $Header: tp_inet.c,v 5.3 88/11/18 17:27:29 nhall Exp $ 
 * $Source: /usr/argo/sys/netiso/RCS/tp_inet.c,v $
 *
 * Here is where you find the inet-dependent code.  We've tried
 * keep all net-level and (primarily) address-family-dependent stuff
 * out of the tp source, and everthing here is reached indirectly
 * through a switch table (struct nl_protosw *) tpcb->tp_nlproto 
 * (see tp_pcb.c). 
 * The routines here are:
 * 		in_getsufx: gets transport suffix out of an inpcb structure.
 * 		in_putsufx: put transport suffix into an inpcb structure.
 *		in_putnetaddr: put a whole net addr into an inpcb.
 *		in_getnetaddr: get a whole net addr from an inpcb.
 *		in_recycle_suffix: clear suffix for reuse in inpcb
 *		tpip_mtu: figure out what size tpdu to use
 *		tpip_input: take a pkt from ip, strip off its ip header, give to tp
 *		tpip_output_dg: package a pkt for ip given 2 addresses & some data
 *		tpip_output: package a pkt for ip given an inpcb & some data
 */

#ifndef lint
static char *rcsid = "$Header: tp_inet.c,v 5.3 88/11/18 17:27:29 nhall Exp $";
#endif lint

#ifdef INET

#include "types.h"
#include "socket.h"
#include "socketvar.h"
#include "mbuf.h"
#include "errno.h"
#include "time.h"
#include "../net/if.h"
#include "../netiso/tp_param.h"
#include "../netiso/argo_debug.h"
#include "../netiso/tp_stat.h"
#include "../netiso/tp_ip.h"
#include "../netiso/tp_pcb.h"
#include "../netiso/tp_trace.h"
#include "../netiso/tp_stat.h"
#include "../netiso/tp_tpdu.h"
#include "../netinet/in_var.h"


/*
 * NAME:			in_getsufx()

 * CALLED FROM: 	pr_usrreq() on PRU_BIND, 
 *					PRU_CONNECT, PRU_ACCEPT, and PRU_PEERADDR
 *
 * FUNCTION, ARGUMENTS, and RETURN VALUE:
 * 	Get a transport suffix from an inpcb structure (inp).
 * 	The argument (which) takes the value TP_LOCAL or TP_FOREIGN.
 *
 * RETURNS:		internet port / transport suffix
 *  			(CAST TO AN INT)
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */

int
in_getsufx(inp,  which)
	struct inpcb *inp;
	int which;
{
	switch (which) {
	case TP_LOCAL:
		return (int) inp->inp_lport;

	case TP_FOREIGN:
		return (int) inp->inp_fport;
	}
}

/*
 * NAME:		in_putsufx()
 *
 * CALLED FROM: tp_newsocket(); i.e., when a connection 
 *		is being established by an incoming CR_TPDU.
 *
 * FUNCTION, ARGUMENTS:
 * 	Put a transport suffix (found in name) into an inpcb structure (inp).
 * 	The argument (which) takes the value TP_LOCAL or TP_FOREIGN.
 *
 * RETURNS:		Nada
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
void
in_putsufx(inp, name, which)
	struct inpcb *inp;
	struct sockaddr_in *name;
	int which;
{
	switch (which) {
	case TP_LOCAL:
		inp->inp_lport = name->sin_port;
		break;
	case TP_FOREIGN:
		inp->inp_fport = name->sin_port;
		break;
	}
}

/*
 * NAME:	in_recycle_tsuffix()	
 *
 * CALLED FROM:	tp.trans whenever we go into REFWAIT state.
 *
 * FUNCTION and ARGUMENT:
 *	 Called when a ref is frozen, to allow the suffix to be reused. 
 * 	(inp) is the net level pcb.  
 *
 * RETURNS:			Nada
 *
 * SIDE EFFECTS:	
 *
 * NOTES:	This really shouldn't have to be done in a NET level pcb 
 *	but... for the internet world that just the way it is done in BSD...
 * 	The alternative is to have the port unusable until the reference
 * 	timer goes off.
 */
void
in_recycle_tsuffix(inp)
	struct inpcb	*inp;
{
	inp->inp_fport = inp->inp_lport = 0;
}

/*
 * NAME:	in_putnetaddr()
 *
 * CALLED FROM:
 * 	tp_newsocket(); i.e., when a connection is being established by an
 * 	incoming CR_TPDU.
 *
 * FUNCTION and ARGUMENTS:
 * 	Copy a whole net addr from a struct sockaddr (name).
 * 	into an inpcb (inp).
 * 	The argument (which) takes values TP_LOCAL or TP_FOREIGN
 *
 * RETURNS:		Nada
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */ 
void
in_putnetaddr(inp, name, which)
	register struct inpcb	*inp;
	struct sockaddr_in	*name;
	int which;
{
	switch (which) {
	case TP_LOCAL:
		bcopy((caddr_t)&name->sin_addr, 
			(caddr_t)&inp->inp_laddr, sizeof(struct in_addr));
			/* won't work if the dst address (name) is INADDR_ANY */

		break;
	case TP_FOREIGN:
		if( name != (struct sockaddr_in *)0 ) {
			bcopy((caddr_t)&name->sin_addr, 
				(caddr_t)&inp->inp_faddr, sizeof(struct in_addr));
		}
	}
}

/*
 * NAME:	in_getnetaddr()
 *
 * CALLED FROM:
 *  pr_usrreq() PRU_SOCKADDR, PRU_ACCEPT, PRU_PEERADDR
 * FUNCTION and ARGUMENTS:
 * 	Copy a whole net addr from an inpcb (inp) into
 * 	a struct sockaddr (name).
 * 	The argument (which) takes values TP_LOCAL or TP_FOREIGN.
 *
 * RETURNS:		Nada
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */ 

void
in_getnetaddr( inp, name, which)
	struct inpcb *inp;
	struct sockaddr_in *name;
	int which;
{
	switch (which) {
	case TP_LOCAL:
		bcopy( (caddr_t)&inp->inp_laddr, (caddr_t)&name->sin_addr, 
				sizeof(struct in_addr));
			/* won't work if the dst address (name) is INADDR_ANY */
		break;

	case TP_FOREIGN:
		bcopy( (caddr_t)&inp->inp_faddr, (caddr_t)&name->sin_addr, 
				sizeof(struct in_addr));
			/* won't work if the dst address (name) is INADDR_ANY */
		break;
	}
}

/*
 * NAME: 	tpip_mtu()
 *
 * CALLED FROM:
 *  tp_input() on incoming CR, CC, and pr_usrreq() for PRU_CONNECT
 *
 * FUNCTION, ARGUMENTS, and RETURN VALUE:
 *
 * Determine the proper maximum transmission unit, i.e., MTU, to use, given
 * a) the header size for the network protocol and the max transmission
 *	  unit on the subnet interface, determined from the information in (inp),
 * b) the max size negotiated so far (negot)
 * c) the window size used by the tp connection (found in so),
 *
 * The result is put in the integer *size in its integer form and in
 * *negot in its logarithmic form.  
 * 
 * The rules are:
 * a) can only negotiate down from the value found in *negot.
 * b) the MTU must be < the windowsize,
 * c) If src and dest are on the same net,
 * 	  we will negotiate the closest size larger than  MTU but really USE 
 *    the actual device mtu - ll hdr sizes.
 *   otherwise we negotiate the closest size smaller than MTU - ll hdr sizes.
 * 
 * SIDE EFFECTS:
 *	changes the values addressed by the arguments (size) and (negot)
 *  and
 *  when the peer is not on one of our directly connected subnets, it
 *  looks up a route, leaving the route in the inpcb addressed by (inp)
 *
 * NOTES:
 */

void
tpip_mtu(so, inp, size, negot)
	struct socket *so;
	struct inpcb *inp;
	int *size;
	u_char *negot;
{
	register struct ifnet	*ifp;
	struct ifnet			*tpip_route();
	struct in_ifaddr		*ia;
	register int			i;
	int						windowsize = so->so_rcv.sb_hiwat;

	IFDEBUG(D_CONN)
		printf("tpip_mtu(0x%x,0x%x,0x%x,0x%x)\n",
			so, inp, size, negot);
		printf("tpip_mtu routing to addr 0x%x\n", inp->inp_faddr);
	ENDDEBUG
	IFTRACE(D_CONN)
		tptrace(TPPTmisc, "ENTER GET MTU: size negot \n",*size, *negot, 0, 0);
	ENDTRACE

	*size = 1 << *negot;

	if( *size > windowsize ) {
		*size = windowsize;
	}

	ia = in_iaonnetof(in_netof(inp->inp_faddr));
	if ( ia == (struct in_ifaddr *)0 ) {
		ifp = tpip_route(&inp->inp_faddr);
		if( ifp == (struct ifnet *)0 )
			return ;
	} else
		ifp = ia->ia_ifp;


	/****************************************************************
	 * TODO - make this indirect off the socket structure to the
	 * network layer to get headersize
	 * After all, who knows what lies below the IP layer?
	 * Who knows how big the NL header will be?
	 ***************************************************************/

	if( *size > ifp->if_mtu - sizeof(struct ip)) {
		*size = ifp->if_mtu - sizeof(struct ip);
	}
	for(i=TP_MIN_TPDUSIZE; (i<TP_MAX_TPDUSIZE && ((1<<i)<*size)) ; i++)
		;
	i--;

	if (in_netof(inp->inp_laddr) != in_netof(inp->inp_faddr)) {
		i++;
	} else {
		*size = 1<<i;
	}
	*negot = i;

	IFDEBUG(D_CONN)
		printf("GET MTU RETURNS: ifp %s size 0x%x negot 0x%x\n",
		ifp->if_name,	*size, *negot);
	ENDDEBUG
	IFTRACE(D_CONN)
		tptrace(TPPTmisc, "EXIT GET MTU: tpcb size negot \n",
		*size, *negot, 0, 0);
	ENDTRACE

}

/*
 * NAME:	tpip_output()
 *
 * CALLED FROM:  tp_emit()
 *
 * FUNCTION and ARGUMENTS:
 *  Take a packet(m0) from tp and package it so that ip will accept it.
 *  This means prepending space for the ip header and filling in a few
 *  of the fields.
 *  inp is the inpcb structure; datalen is the length of the data in the
 *  mbuf string m0.
 * RETURNS:			
 *  whatever (E*) is returned form the net layer output routine.
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */

int
tpip_output(inp, m0, datalen, nochksum)
	struct inpcb		*inp;
	struct mbuf 		*m0;
	int 				datalen;
	int					nochksum;
{
	return tpip_output_dg( &inp->inp_laddr, &inp->inp_faddr, m0, datalen,
		&inp->inp_route, nochksum);
}

/*
 * NAME:	tpip_output_dg()
 *
 * CALLED FROM:  tp_error_emit()
 *
 * FUNCTION and ARGUMENTS:
 *  This is a copy of tpip_output that takes the addresses
 *  instead of a pcb.  It's used by the tp_error_emit, when we
 *  don't have an in_pcb with which to call the normal output rtn.
 *
 * RETURNS:	 ENOBUFS or  whatever (E*) is 
 *	returned form the net layer output routine.
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */

int
tpip_output_dg(laddr, faddr, m0, datalen, ro, nochksum)
	struct in_addr		*laddr, *faddr;
	struct mbuf 		*m0;
	int 				datalen;
	struct route 		*ro;
	int					nochksum;
{
	register struct mbuf 	*m;
	register struct ip *ip;
	int 					error;

	IFDEBUG(D_EMIT)
		printf("tpip_output_dg  datalen 0x%x m0 0x%x\n", datalen, m0);
	ENDDEBUG


	MGET(m, M_DONTWAIT, TPMT_IPHDR);
	if (m == 0) {
		error = ENOBUFS;
		goto bad;
	}
	bzero(mtod(m, caddr_t), MLEN);
	m->m_next = m0;
	m->m_off = MMAXOFF - sizeof(struct ip);
	m->m_len = sizeof(struct ip);
	m->m_act = MNULL;

	ip = mtod(m, struct ip *);

	ip->ip_p = IPPROTO_TP;
	ip->ip_len = sizeof(struct ip) + datalen;
	ip->ip_ttl = MAXTTL;	
		/* don't know why you need to set ttl;
		 * overlay doesn't even make this available
		 */

	ip->ip_src = *laddr;
	ip->ip_dst = *faddr;

	IncStat(ts_tpdu_sent);
	IFDEBUG(D_EMIT)
		dump_mbuf(m, "tpip_output_dg before ip_output\n");
	ENDDEBUG

	error = ip_output(m, (struct mbuf *)0, ro, IP_ALLOWBROADCAST);

	IFDEBUG(D_EMIT)
		printf("tpip_output_dg after ip_output\n");
	ENDDEBUG

	return error;

bad:
	m_freem(m);
	IncStat(ts_send_drop);
	return error;
}

/*
 * NAME:  tpip_input()
 *
 * CALLED FROM:
 * 	ip's input routine, indirectly through the protosw.
 *
 * FUNCTION and ARGUMENTS:
 * Take a packet (m) from ip, strip off the ip header and give it to tp
 *
 * RETURNS:  No return value.  
 * 
 * SIDE EFFECTS:
 *
 * NOTES:
 */
ProtoHook
tpip_input(m)
	struct mbuf *m;
{
	typedef struct {
		struct ip	 tpip_i;
		struct tpdu  tpip_d;
	} tpiphdr;
	register struct tpdu 	*hdr = mtod(m, struct tpdu *);
	struct sockaddr_in 	src, dst;
	register struct ip 		*ip;
	int						s = splnet();

	IncStat(ts_pkt_rcvd);

	/* IP layer has already pulled up the IP header */

	while( m->m_len < 1 ) {
		struct mbuf *n;
		n = m_free(m);
		if( n == MNULL ) {
			splx(s);
			return 0;
		}
	}
	CHANGE_MTYPE(m, TPMT_DATA);
	
	/*
	 * now pull up the whole tp header : we stripped all leading mbufs
	 * w/o at least one byte, so we know we can read the tpdu_li field.
	 */
	hdr = &(mtod(m, tpiphdr *))->tpip_d;

	if( m->m_len < hdr->tpdu_li + 1 + sizeof(struct ip) ) {
		if((m = m_pullup(m, sizeof(struct ip) + (int)(hdr->tpdu_li)+1))==MNULL){
			IFDEBUG(D_TPINPUT)
				printf("tp_input, pullup 2!\n");
			ENDDEBUG
			goto discard;
		}
	}
	/* 
	 * cannot use tp_inputprep() here 'cause you don't 
	 * have quite the same situation
	 */

	IFDEBUG(D_TPINPUT)
		dump_mbuf(m, "after tpip_input both pullups");
	ENDDEBUG
	/* 
	 * m_pullup may have returned a different mbuf
	 */
	ip = &(mtod(m, tpiphdr *))->tpip_i;

	/*
	 * drop the ip header from the front of the mbuf
	 * this is necessary for the tp checksum
	 */
	m->m_len -= sizeof(struct ip);
	m->m_off += sizeof(struct ip);

	src.sin_addr = *(struct in_addr *)&(ip->ip_src);
	src.sin_family  = AF_INET;
	dst.sin_addr = *(struct in_addr *)&(ip->ip_dst);
	dst.sin_family  = AF_INET; 

	(void) tp_input(m, &src, &dst, 0, tpip_output_dg);
	splx(s);
	return 0;

discard:
	IFDEBUG(D_TPINPUT)
		printf("tpip_input DISCARD\n");
	ENDDEBUG
	IFTRACE(D_TPINPUT)
		tptrace(TPPTmisc, "tpip_input DISCARD m",  m,0,0,0);
	ENDTRACE
	m_freem(m);
	IncStat(ts_recv_drop);

	return 0;
}


#include "../h/protosw.h"
#include "../netinet/ip_icmp.h"

/*
 * NAME:	tpin_quench()
 *
 * CALLED FROM: tpip_ctlinput()
 *
 * FUNCTION and ARGUMENTS:  find the tpcb pointer and pass it to tp_quench
 *
 * RETURNS:	Nada
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */

void
tpin_quench(inp)
	struct inpcb *inp;
{
	tp_quench( inp->inp_socket->so_tpcb );
}

/*
 * NAME:	tpip_ctlinput()
 *
 * CALLED FROM:
 *  The network layer through the protosw table.
 *
 * FUNCTION and ARGUMENTS:
 *	When clnp gets an ICMP msg this gets called.
 *	It either returns an error status to the user or
 *	causes all connections on this address to be aborted
 *	by calling the appropriate xx_notify() routine.
 *	(cmd) is the type of ICMP error.   
 * 	(sa) the address of the sender
 *
 * RETURNS:	 Nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
ProtoHook
tpip_ctlinput(cmd, sin)
	int cmd;
	struct sockaddr_in *sin;
{
	extern u_char inetctlerrmap[];
	extern ProtoHook tpin_abort();
	extern ProtoHook in_rtchange();

	if (sin->sin_family != AF_INET && sin->sin_family != AF_IMPLINK)
		return 0;
	if (sin->sin_addr.s_addr == INADDR_ANY)
		return 0;
	if (cmd < 0 || cmd > PRC_NCMDS)
		return 0;
	switch (cmd) {

		case	PRC_QUENCH:
			in_pcbnotify(&tp_inpcb, &sin->sin_addr, 0, tp_quench);
			break;

		case	PRC_ROUTEDEAD:
		case	PRC_HOSTUNREACH:
		case	PRC_UNREACH_NET:
		case	PRC_IFDOWN:
		case	PRC_HOSTDEAD:
			in_pcbnotify(&tp_inpcb, &sin->sin_addr, 
					(int)inetctlerrmap[cmd], in_rtchange);
			break;

		default:
		/*
		case	PRC_MSGSIZE:
		case	PRC_UNREACH_HOST:
		case	PRC_UNREACH_PROTOCOL:
		case	PRC_UNREACH_PORT:
		case	PRC_UNREACH_NEEDFRAG:
		case	PRC_UNREACH_SRCFAIL:
		case	PRC_REDIRECT_NET:
		case	PRC_REDIRECT_HOST:
		case	PRC_REDIRECT_TOSNET:
		case	PRC_REDIRECT_TOSHOST:
		case	PRC_TIMXCEED_INTRANS:
		case	PRC_TIMXCEED_REASS:
		case	PRC_PARAMPROB:
		*/
		in_pcbnotify(&tp_inpcb, sin, (int)inetctlerrmap[cmd], tpin_abort);
	}
	return 0;
}

/*
 * NAME:	tpin_abort()
 *
 * CALLED FROM:
 *	xxx_notify() from tp_ctlinput() when
 *  net level gets some ICMP-equiv. type event.
 *
 * FUNCTION and ARGUMENTS:
 *  Cause the connection to be aborted with some sort of error
 *  reason indicating that the network layer caused the abort.
 *  Fakes an ER TPDU so we can go through the driver.
 *
 * RETURNS:	 Nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */

ProtoHook
tpin_abort(inp)
	struct inpcb *inp;
{
	struct tp_event e;

	e.ev_number = ER_TPDU;
	e.ATTR(ER_TPDU).e_reason = ENETRESET;
	(void) tp_driver(inp->inp_ppcb, &e);
	return 0;
}

#ifdef ARGO_DEBUG
dump_inaddr(addr)
	register struct sockaddr_in *addr;
{
	printf("INET: port 0x%x; addr 0x%x\n", addr->sin_port, addr->sin_addr);
}
#endif ARGO_DEBUG

/*
 * NAME:	tpip_route()
 *
 * CALLED FROM: tpip_mtu()
 *
 * FUNCTION and ARGUMENTS:	given a destination addresss,
 *	find the interface that would be used to send something to this address.
 *
 * RETURNS:	 pointer to an ifnet structure
 *
 * SIDE EFFECTS:
 *
 * NOTES:			
 */
struct ifnet *
tpip_route(dst)
	struct in_addr *dst;
{
	struct	ifnet 		*ifp = (struct ifnet *)0;
	struct	sockaddr_in	*dst_in;
	struct route		iproute;
	struct route		*ro = (struct route *)0;
	struct in_ifaddr	*ia;

	IFDEBUG(D_CONN)
		printf("tpip_route: dst is x%x\n", *dst);
	ENDDEBUG

	ro = &iproute;
	bzero((caddr_t)ro, sizeof (*ro));
	dst_in = (struct sockaddr_in *)&ro->ro_dst;
	dst_in->sin_family = AF_INET;
	dst_in->sin_addr = *dst;

	ia = (struct in_ifaddr *)ifa_ifwithdstaddr(dst_in);
	if (ia == 0)
		ia = in_iaonnetof(in_netof(dst_in));
	if (ia != 0) {
		ifp = ia->ia_ifp;
		IFDEBUG(D_CONN)
			printf("tpip_route: ifp from ia:0x%x\n", ia);
		ENDDEBUG
	} else {
		rtalloc(ro);
		if (ro->ro_rt != 0) {
			ifp = ro->ro_rt->rt_ifp;
			IFDEBUG(D_CONN)
				printf("tpip_route: ifp from route:0x%x ro_rt 0x%x\n", ro,
					ro->ro_rt);
			ENDDEBUG
			rtfree(ro->ro_rt);
		}
	}
	IFDEBUG(D_CONN)
		printf("tpip_route: returning 0x%x\n", ifp);
		if (ifp)
			printf("tpip_route: if name %s unit 0x%x, mtu 0x%x\n", 
				ifp->if_name, ifp->if_unit, ifp->if_mtu);
	ENDDEBUG
	return ifp;
}

#endif INET
