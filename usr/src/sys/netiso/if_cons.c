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
 * $Header: if_cons.c,v 4.7 88/08/11 15:52:55 nhall Exp $
 * $Source: /usr/argo/sys/netiso/RCS/if_cons.c,v $
 *
 * cons.c - Connection Oriented Network Service:
 * including support for a) user transport-level service, 
 *	b) COSNS below CLNP, and c) CONS below TP.
 */

#ifndef lint
static char *rcsid = "$Header: if_cons.c,v 4.7 88/08/11 15:52:55 nhall Exp $";
#endif lint

#ifdef ARGO_DEBUG
#define Static  
unsigned LAST_CALL_PCB;
#else ARGO_DEBUG
#define Static static
#endif ARGO_DEBUG

#include "ecn.h"
#include "argoxtwentyfive.h"

#if NARGOXTWENTYFIVE > 0

#ifdef KERNEL

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "errno.h"
#include "ioctl.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#include "../netiso/iso_errno.h"
#include "../netiso/argo_debug.h"
#include "../netiso/tp_trace.h"
#include "../netiso/iso.h"
#include "../netiso/cons.h"
#include "../netiso/iso_pcb.h"
#include "../netiso/cons_pcb.h"
#include "../caif/eicon.h"

#ifdef ARGO_DEBUG
#define MT_XCONN	0x50
#define MT_XCLOSE	0x51
#define MT_XCONFIRM	0x52
#define MT_XDATA	0x53
#define MT_XHEADER	0x54
#else
#define MT_XCONN	MT_DATA
#define MT_XCLOSE	MT_DATA
#define MT_XCONFIRM	MT_DATA
#define MT_XDATA	MT_DATA
#define MT_XHEADER	MT_HEADER
#endif ARGO_DEBUG

#define DONTCLEAR	 -1

/*********************************************************************	
 * cons.c - CONS interface to the eicon adapter
 * Includes connection manager - for (TP, CLNP)/x.25
 *
 * TODO: figure out what resources we might run out of besides mbufs.
 *  If we run out of any of them (including mbufs) close and recycle
 *  lru x% of the connections, for some parameter x.
 *
 * There are 4 interfaces from above:
 * 0) from CLNP: 
 *    cons is an interface driver - CLNP calls
 *    cosns_output(ifp, m, dst), a device-type interface output routine
 *    that does some connection management stuff and queues a
 *    request on the eicon driver queue by calling ifp->if_output.
 *    The eicon's ifp structure contains cosns_output as its output routine
 *    rather than ifp_>if_output! Kludge, but we don't have much choice...
 *    X25 connections created in this manner may always be multiplexed
 *    but only with their own kind (not with connections servicing TP
 *    directly.)
 *	  	co_flags & CONSF_DGM 
 * 1) from TP0: 
 *    cons CO network service
 *    TP associates a transport connection with a network connection.
 * 	  cons_output( isop, m, len, isdgm==0 ) 
 *        co_flags == 0
 * 2) from TP 4:
 *	  It's a datagram service, like clnp is. - even though it calls
 *			cons_output( isop, m, len, isdgm==1 ) 
 *	  it eventually goes through
 *			cosns_output(ifp, m, dst).
 *    TP4 permits multiplexing (reuse, possibly simultaneously) of the 
 *	  network connections.
 *    This means that many sockets (many tpcbs) may be associated with
 *    this cons_pcb, hence cannot have a back ptr from cons_pcb to a tpcb.
 *        co_flags & CONSF_DGM 
 *    co_socket is null since there may be many sockets that use this copcb.
 * 3) from user: cons_usrreq(), cons_ctloutput() 
 *    cons is a standard transport service interface.
 *    There is a 1-1 correspondence between net connections and sockets.
 *	  co_socket points to a socket.
 *
NOTE:
	streams would really be nice. sigh.
NOTE:
	eicon <--> cons interface: the first mbuf (the ecn_request structure)
	had better NOT be a cluster.
NOTE:
	PVCs could be handled by config-ing a cons with an address and with the
	IFF_POINTTOPOINT flag on.  This code would then have to skip the
	connection setup stuff for pt-to-pt links.  
NOTE:
	We keep track of the ifp for each connection.  Right now this is
	unnecessary, but just in case someone comes up with some kind
	of a kludge to allow > 1 eicon to be attached at a time,
	(i.e., some meaningful netof( a type 37 address ) ),
	we do keep track of this.


 *********************************************************************/

#define touch(copcb) copcb->co_ttl = copcb->co_init_ttl

#define CONS_IFQMAXLEN 5

#define SET_CHANMASK( isop, chan )\
	if( (u_int)(chan) < 32 ) \
		(isop)->isop_chanmask = (1<<((chan)-1));\
	else \
		(isop)->isop_negchanmask = (1<<((256-(chan))-1))

#define ADD_CHANMASK( isop, chan )\
	if( (u_int)(chan) < 32 ) \
		(isop)->isop_chanmask |= (1<<((chan)-1));\
	else \
		(isop)->isop_negchanmask |= (1<<((256-(chan))-1))

struct ifnet 			*consif; /* TO BE REMOVED */
Static int				consinit(), consioctl(), consattach();

/* protosw pointers for getting to higher layer */
Static 	struct protosw	*CLNP_proto;
Static 	struct protosw	*TP_proto;
Static 	struct protosw	*X25_proto;
Static 	int				issue_clear_req();

#ifndef	PHASEONE
extern	struct ifaddr	*ifa_ifwithnet();
#endif	PHASEONE

extern	struct ifaddr	*ifa_ifwithaddr();

Static  struct socket	dummysocket; /* for use by cosns */

extern struct	isopcb	tp_isopcb; /* chain of all TP pcbs */
struct	isopcb			cons_isopcb; /* chain of all cons pcbs */
struct	isopcb			tp_incoming_pending;  /* incoming connections
										for TP, pending */

struct isopcb 	*Xpcblist[] =  {
	&cons_isopcb, 
	&tp_incoming_pending,
	&tp_isopcb,
	(struct isopcb *)0
};

Static 	int parse_facil(), NSAPtoDTE(), make_partial_x25_packet();
Static	int FACILtoNSAP(), DTEtoNSAP();
Static	struct cons_pcb *cons_chan_to_pcb();

#define HIGH_NIBBLE 1
#define LOW_NIBBLE 0

/*
 * NAME:	nibble_copy()
 * FUNCTION and ARGUMENTS:
 * 	copies (len) nibbles from (src_octet), high or low nibble
 *  to (dst_octet), high or low nibble,
 * src_nibble & dst_nibble should be:
 * 	HIGH_NIBBLE (1) if leftmost 4 bits/ most significant nibble
 * 	LOW_NIBBLE (0) if rightmost 4 bits/ least significant nibble
 * RETURNS: VOID
 */
void
nibble_copy( src_octet, src_nibble, dst_octet, dst_nibble, len)
	register char  	*src_octet;
	register char  	*dst_octet;
	register unsigned		src_nibble;
	register unsigned 		dst_nibble;
	int		len;
{

	register 	i;
	register 	unsigned dshift, sshift;

	IFDEBUG(D_CADDR)
		printf("nibble_copy ( 0x%x, 0x%x, 0x%x, 0x%x 0x%x)\n", 
		 src_octet, src_nibble, dst_octet, dst_nibble, len);
	ENDDEBUG
#define SHIFT 0x4

	dshift = dst_nibble << 2;
	sshift = src_nibble << 2;

	for (i=0; i<len; i++) {
		/* clear dst_nibble  */
		*dst_octet 	&= ~(0xf<< dshift);

		/* set dst nibble */
		*dst_octet 	|= ( 0xf & (*src_octet >> sshift))<< dshift;

		dshift		^= SHIFT;
		sshift		^= SHIFT;
		src_nibble 	= 1-src_nibble;
		dst_nibble 	= 1-dst_nibble;
		src_octet	+= src_nibble;
		dst_octet 	+= dst_nibble;
	}
	IFDEBUG(D_CADDR)
		printf("nibble_copy DONE\n");
	ENDDEBUG
}

/*
 * NAME:	nibble_match()
 * FUNCTION and ARGUMENTS:
 * 	compares src_octet/src_nibble and dst_octet/dst_nibble  for len nibbles.
 * RETURNS: 0 if they differ, 1 if they are the same.
 */
int
nibble_match( src_octet, src_nibble, dst_octet, dst_nibble, len)
	register char  	*src_octet;
	register char  	*dst_octet;
	register unsigned		src_nibble;
	register unsigned 		dst_nibble;
	int		len;
{

	register 	i;
	register 	unsigned dshift, sshift;
	u_char		nibble_a, nibble_b;

	IFDEBUG(D_CADDR)
		printf("nibble_match ( 0x%x, 0x%x, 0x%x, 0x%x 0x%x)\n", 
		 src_octet, src_nibble, dst_octet, dst_nibble, len);
	ENDDEBUG
#define SHIFT 0x4

	dshift = dst_nibble << 2;
	sshift = src_nibble << 2;

	for (i=0; i<len; i++) {
		nibble_b = ((*dst_octet)>>dshift) & 0xf;
		nibble_a = ( 0xf & (*src_octet >> sshift));
		if( nibble_b != nibble_a )
			return 0;

		dshift		^= SHIFT;
		sshift		^= SHIFT;
		src_nibble 	= 1-src_nibble;
		dst_nibble 	= 1-dst_nibble;
		src_octet	+= src_nibble;
		dst_octet 	+= dst_nibble;
	}
	IFDEBUG(D_CADDR)
		printf("nibble_match DONE\n");
	ENDDEBUG
	return 1;
}

#ifdef ARGO_DEBUG

Static
dump_copcb(copcb, str)
	char * str;
	register struct cons_pcb *copcb;
{
	printf("XPCB DUMP %s\n", str);
	if (copcb) {
		printf("\t copcb 0x%x next 0x%x head 0x%x socket 0x%x ifp 0x%x\n",
			copcb, copcb->co_next, copcb->co_head, copcb->co_socket, copcb->co_ifp);
		printf("\t channel 0x%x state 0x%x flags 0x%x proto 0x%x\n",
			copcb->co_channel, copcb->co_state, copcb->co_flags, copcb->co_proto);
		printf("\t laddr :\n");
		dump_isoaddr(&copcb->co_laddr);
		printf("\t faddr :\n");
		dump_isoaddr(&copcb->co_faddr);
		printf("\tttl 0x%x init_ttl 0x%x pending: %d\n",
			copcb->co_ttl, copcb->co_init_ttl, copcb->co_pending.ifq_len);
	}
	printf("END DUMP\n");
}
#endif ARGO_DEBUG

/*
 * FUNCTION : choose_output - chooses between the eicon and loopback.
 * This MUST be here because the ifp->if_output routine is cosns_output
 * -- due to our need to look like a device driver for CLNP. sigh.
 * ARGUMENTS & PURPOSE:  (copcb) ptr to a protocol control block for
 *			x.25, (m) is an mbuf ptr. *m is a request destined either
 * 			for the eicon driver or for the loopback driver.
 * RETURNS : whatever error value the 2I or loopback returns.
 */
Static int 
choose_output( ifp, m, loop)
	struct ifnet 	*ifp;
	struct mbuf 	*m;
	int				loop;
{
	int error;

	if( !m )
		return 0;
	ASSERT(m->m_len != 0);
	if( loop != 0)
		error = lpboutput( ifp, m );
	else
		error = ecnoutput( ifp,  m );

	if (error == 0)
		ifp->if_opackets ++;
	else {
		ifp->if_oerrors ++;
		IFTRACE(D_CDATA)
			tptrace( TPPTmisc, 
			"choose_output: ifp  m error loop\n", 
				ifp, m, error, loop);
		ENDTRACE
	}
	IFDEBUG(D_CCONS)
		printf("choose_output returns 0x%x\n", error );
	ENDDEBUG
	return error;
}

/*
 **************************** NET PROTOCOL cons ***************************
 */

/*
 * NAME:	cons_init()
 * CALLED FROM:
 *	autoconf
 * FUNCTION:
 *	initialize the protocol
 */
cons_init()
{
	init_lpb();
	consattach(); 

	/* protocol init stuff */

	consintrq.ifq_maxlen = IFQ_MAXLEN;
	consintrq.ifq_head = consintrq.ifq_tail =  (struct mbuf *)0;

	CLNP_proto = pffindproto(AF_ISO, ISOPROTO_CLNP, SOCK_DGRAM); 
	X25_proto = pffindproto(AF_ISO, ISOPROTO_X25, SOCK_STREAM);
	TP_proto = pffindproto(AF_ISO, ISOPROTO_TP0, SOCK_SEQPACKET);
	IFDEBUG(D_CCONS)
		printf("cons_init end : cnlp_proto 0x%x cons proto 0x%x tp proto 0x%x\n",
			CLNP_proto, X25_proto, TP_proto);
	ENDDEBUG

	cons_isopcb.isop_next = cons_isopcb.isop_prev = &cons_isopcb;
	tp_incoming_pending.isop_next = tp_incoming_pending.isop_prev =
			&tp_incoming_pending;
}

#ifdef notdef

/*
 * NAME:	cons_free_lru()
 * some day CALLED FROM: 
 * 	wherever we run out of mbufs (not used right yet)
 * FUNCTION:
 *	get rid of the num least recently used connections and
 *  recycle their mbufs.
 * NOTE: GROTESQUELY INEFFICIENT needs to be written nicely
 */

Static
cons_free_lru(qty)
	int qty;
{
	register struct cons_pcb **copcblist = (struct cons_pcb **)Xpcblist;
	register struct cons_pcb *copcb;
	struct cons_pcb 			Lru; 
	struct cons_pcb 			*lru; 

	IFDEBUG(D_CCONS)
		printf("cons_free_lru( 0x%x )\n", qty);
	ENDDEBUG

	Lru.co_ttl = X25_TTL;
	lru = &Lru;

	while (qty > 1) { /* GROT */
		cons_free_lru( 1 );
		qty -- ;
	}

	for( copcb = *copcblist; copcb; copcb = *(++copcblist) ) {
		copcb = (struct cons_pcb *)copcb->co_next;
		while (copcb !=  *copcblist) {
			if( copcb->co_ttl < lru->co_ttl ) 
				lru = copcb;
			copcb = (struct cons_pcb *)copcb->co_next;
		}
	}

	if(lru->co_socket) {
		soisdisconnected(lru->co_socket);
		sohasoutofband(lru->co_socket); /* signal */
	} 

	cons_clear_and_detach( lru, E_CO_HLI_RESYNC, PRC_TIMXCEED_REASS);
}
#endif notdef

/*
 * NAME:	cons_slowtimo()
 * CALLED FROM: 
 * 	the clock
 * FUNCTION:
 *	get rid of any timed-out cons connections
 *  cons connections get "touched" with every use, meaning the
 *  time-to-live gets reset to its max value w/ every use.
 *  The slowtimo() rtn decrements the time-to-live for each
 *  cons connection.  If one of them hits zero ---> zap the connection.
 *  This really only applies to those used for CLNP and TP4.
 *  TP4 keeps the connections open with keepalive.
 * TODO:
 *  Have this happen ONLY for international connections since
 *  there's no connect time charge for domestic calls.
 *  Make default 5 min; make a user option to change it.
 * TODO:
 *  Maybe if the ttl gets lower than a certain threshold, move this 
 *  copcb to the END of its queue so it doesn't slow down the others.
 */

cons_slowtimo()
{
	register struct cons_pcb **copcblist = (struct cons_pcb **)Xpcblist;
	register struct cons_pcb *copcb;
	int s = splnet();
	int	qlen = 0;
	int qdrops = 0;
	int	nvisited = 0;

#ifdef ARGO_DEBUG
	Static int count;

	count = 0;
#endif ARGO_DEBUG

	IncStat(co_slowtimo);
	for( copcb = *copcblist; copcb; copcb = *(++copcblist) ) {
#ifdef ARGO_DEBUG
		if( copcb == (struct cons_pcb *)0 ) {
			ASSERT( 0 );
			panic("TURNING OFF cons_slowtimo()!!! \n");
		}
#endif ARGO_DEBUG
		copcb = (struct cons_pcb *)copcb->co_next;
		while (copcb !=  *copcblist) {
#ifdef ARGO_DEBUG
			if(++count >50 ) {
				printf("cons PANIC: slowtimo LOOP\n");
				splx(s);
				return;
			}
#endif ARGO_DEBUG
#ifdef notdef
			if( copcb->co_init_ttl == 0 ) {
	ASSERT( (struct isopcb *)(*copcblist)==(struct isopcb *)&tp_isopcb );
				copcb = (struct cons_pcb *)copcb->co_next; 
				continue;
			}
#endif notdef
			nvisited ++;
			ASSERT( copcb != (struct cons_pcb *)0 );
			qlen += copcb->co_pending.ifq_len;
			qdrops += copcb->co_pending.ifq_drops;

			if( copcb->co_socket) {
				/* don't want XTS, TP0 connections to be subject to time out */
				copcb = (struct cons_pcb *)copcb->co_next; 
				continue;
			}

			if( -- (copcb->co_ttl) > 0 )  {
				copcb = (struct cons_pcb *)copcb->co_next; 
				continue;
			}

			IncStat(co_timedout);

			IFDEBUG(D_CCONN)
				printf("TIMING OUT chan 0x%x copcb 0x%x flags 0x%x\n", 
					copcb->co_channel, copcb, copcb->co_flags );
			ENDDEBUG

			{ 
				register struct cons_pcb * next = 
					(struct cons_pcb *)copcb->co_next; 
				cons_clear_and_detach(copcb, 
						E_CO_HLI_RESYNC, PRC_TIMXCEED_REASS);
				copcb = next;
			}
		}
	}
	if(nvisited) {
		cons_stat.co_avg_qlen = qlen / nvisited;
		cons_stat.co_avg_qdrop = qdrops / nvisited;
		cons_stat.co_active = nvisited;
	}
done:
	splx(s);
}

DUMP_PCBLIST()
{
	register int i=0;
	register struct cons_pcb *copcb;
	register struct cons_pcb **copcblist = (struct cons_pcb **)Xpcblist;

	for( copcb = *copcblist; copcb; copcb = *(++copcblist) ) {
		printf("FOR %d: 0x%x ", ++i, copcb);
		copcb = (struct cons_pcb *)copcb->co_next;
		printf(" next 0x%x, *copcblist 0x%x\n",  copcb, *copcblist);
		while (copcb !=  *copcblist) {
			ASSERT( copcb != (struct cons_pcb *)0 );
			printf("\tCOPCB 0x%x\n", copcb);
			if( copcb )
				dump_buf(copcb, sizeof( *copcb));
			else
				break;
			copcb = (struct cons_pcb *)copcb->co_next; 
		}
	}
}

/*
 * NAME:	cons_pcballoc()
 * CALLED FROM:
 *	cons_usrreq() when doing PRU_ATTACH, 
 *  cons_incoming() when opening a new connection.  
 * FUNCTION and ARGUMENTS:
 *	Allocates a new pcb.
 *  The flags and proto arguments are stashed into the new pcb.
 * RETURN VALUE:
 *  E* if error, 0 if ok
 */
Static int
cons_pcballoc(so, head, flags, proto, dest)
	struct socket	*so;
	struct	isopcb	*head;
	u_short 		flags;
	struct protosw	*proto;
	struct	cons_pcb **dest;
{
	int 					error;
	register struct cons_pcb *copcb;

	IFDEBUG(D_CCONN)
		printf("cons_pcballoc (0x%x, 0x%x, 0x%x, 0x%x, 0x%x\n", 
			so, head, flags, proto, dest);
	ENDDEBUG
	if(proto == (struct protosw *)0)
		return EPROTONOSUPPORT;

	if( ( error = iso_pcballoc(so, head) ) == EOK )  {
		/* Have allocated a cleared mbuf */

		copcb = (struct cons_pcb *)so->so_pcb;
		copcb->co_ttl = copcb->co_init_ttl = X25_TTL;
		copcb->co_flags = flags; 
		copcb->co_proto = proto;
		copcb->co_pending.ifq_maxlen = CONS_IFQMAXLEN;
		copcb->co_myself = copcb;

		if (so == &dummysocket) 
			copcb->co_socket = (struct socket *)0;

		*dest = copcb;
	}
done:
	IFDEBUG(D_CCONN)
		printf("cons_pcballoc returns 0x%x: DUMP\n", copcb);
		dump_buf( copcb, sizeof(*copcb));
	ENDDEBUG
	if( (flags & CONSF_ICRE) == 0) {
		struct dte_addr *dtea = &(*dest)->co_peer_dte;
		int len;

		error = iso_8208snparesolve(&(*dest)->co_faddr, dtea, &len);
		ASSERT(error == 0);
		ASSERT(len == sizeof(struct dte_addr));
	}

	return error;
}

/*
 * NAME:	cons_connect()
 * CALLED FROM:
 *	cons_usrreq() when opening a new connection.  
 * FUNCTION anD ARGUMENTS:
 *  Figures out which device to use, finding a route if one doesn't
 *  already exist.
 * 	Builds an eicon connection request and gives it to the device.
 * RETURN VALUE:
 *  returns E*
 */
Static int 
cons_connect( copcb )
	register struct cons_pcb *copcb;
{
	register struct eicon_request *ecnrq;
	register struct mbuf 	*m;
	int 					error = 0;
	struct ifaddr 			*ifa;

	IFDEBUG(D_CCONN)
		printf("cons_connect( 0x%x ) : ifp 0x%x\npeer: ", copcb, copcb->co_ifp);
		dump_isoaddr(&copcb->co_faddr);
		printf("\nmyaddr: ");
		dump_isoaddr(&copcb->co_laddr);
		printf("\n" );
	ENDDEBUG

	/* PHASE 2: this call is OK */
	if( ifa = ifa_ifwithaddr(&copcb->co_faddr ) ) {
		/* foreign address is me */
		copcb->co_ifp = ifa->ifa_ifp; 
		IFDEBUG(D_CCONN)
			printf("cons_connect: after if_withaddr copcb->co_ifp 0x%x\n",
				copcb->co_ifp);
		ENDDEBUG

		if( (ifa->ifa_ifp->if_flags&(IFF_LOOPBACK|IFF_UP)) ==
												(IFF_LOOPBACK|IFF_UP)) {
			copcb->co_flags |= CONSF_LOOPBACK;
		}
		bcopy((caddr_t)&ifa->ifa_addr, (caddr_t)&copcb->co_laddr, 
			sizeof(struct sockaddr));
	} 
	IFDEBUG(D_CCONN)
		printf("cons_connect: co_flags 0x%x\n", copcb->co_flags);
		if( ifa ) {
			printf(" cons_connect withaddr returns %s\n", 
				copcb->co_ifp->if_name);
		}
	ENDDEBUG
	else if ( copcb->co_ifp == (struct ifnet *)0 ) {
#ifdef	PHASEONE
		/*
		 *	We need to get the local nsap address.
		 *	First, route to the destination. This will provide us with
		 *	an ifp. Second, determine which local address linked on
		 *	that ifp is appropriate
		 */
		struct sockaddr_iso	*first_hop;		/* filled by clnp_route */
		struct iso_addr	*localaddr, *clnp_srcaddr();

		if (error = clnp_route(&copcb->co_faddr, 
			&((struct isopcb *)copcb)->isop_route, /* flags */0,
			&first_hop, &copcb->co_ifp))
			goto bad;

		/* determine local address based upon ifp */
		if ((localaddr = clnp_srcaddr(copcb->co_ifp, 
				&first_hop->siso_addr)) == NULL) {
			error = ENETUNREACH;
			goto bad;
		}
		copcb->co_laddr.siso_family = AF_ISO;
		copcb->co_laddr.siso_addr = *localaddr;
#else
		/* Foreign addr isn't me (lpb). If still don't have an ifp or have
		 * an ifp but don't know its address, look for a route 
		 */
		if( ifa = ifa_ifwithnet(&copcb->co_faddr) ) {
			copcb->co_ifp =  ifa->ifa_ifp;
			IFDEBUG(D_CCONN)
				printf(" cons_connect withnet returns %s\n",
										copcb->co_ifp->if_name);
			ENDDEBUG
		} else {
			printf("cons PANIC: connect: can't find SNPA \n");
			error = ENETUNREACH;
			goto bad;
		}
#endif	PHASEONE
	}
#ifndef	PHASEONE
	if( ifa == (struct ifaddr *)0 ) {
		struct ifaddr * iso_ifwithidi();

		if( ifa = iso_ifwithidi(&copcb->co_faddr) ) {
			copcb->co_ifp =  ifa->ifa_ifp;
			IFDEBUG(D_CCONN)
				printf(" cons_connect withnet returns %s\n",
										copcb->co_ifp->if_name);
			ENDDEBUG
		} else {
			printf("cons PANIC: connect: can't find SNPA \n");
			error = ENETUNREACH;
			goto bad;
		}
	}
	bcopy((caddr_t)&ifa->ifa_addr, (caddr_t)&copcb->co_laddr, 
		sizeof(struct sockaddr));
#endif	PHASEONE

	copcb->co_state = CONNECTING;

	ASSERT( copcb->co_ifp != (struct ifnet *) 0);
	if ( copcb->co_ifp == (struct ifnet *)0 ) {
		error = ENETUNREACH;
		goto bad;
	}

	m = m_getclr(M_DONTWAIT, MT_XCONN);
	if( !m ) {
		copcb->co_ifp->if_oerrors ++;
		error = ENOBUFS;
		goto bad; 
	}
	m->m_len = sizeof(struct eicon_request);

	ecnrq = mtod(m, struct eicon_request *);

	copcb->co_myself = copcb;
	ecnrq->e_pcb = (caddr_t)copcb;
#ifdef ARGO_DEBUG
	LAST_CALL_PCB = (unsigned) ecnrq->e_pcb;
#endif ARGO_DEBUG
	ecnrq->e_cmd = ECN_CALL;
	ecnrq->e_vc = 0; /* mbz ? */
	ecnrq->e_info = 0; /* mbz */

	/* get data buffer */
	{ 	struct mbuf *n;

		MGET(n, M_DONTWAIT, MT_XCONN);
		if( n==MNULL ) {
			copcb->co_ifp->if_oerrors ++;
			error = ENOBUFS;
			goto bad; 
		}
		e_data(ecnrq) = n; /* e_data is really dtom(ecnrq)->m_next */
	}

	IFDEBUG(D_CCONN)
		printf(
		"calling make_partial_x25_packet( 0x%x, 0x%x, 0x%x, 0x%x, 0x%x)\n",
			&copcb->co_laddr, &copcb->co_faddr, 
			copcb->co_proto->pr_protocol, 
			e_data(ecnrq),
			copcb->co_flags & CONSF_XTS);
	ENDDEBUG
	if( error = make_partial_x25_packet( copcb, e_data(ecnrq)) ) {
		copcb->co_ifp->if_oerrors ++;
		m_freem(m);
		goto bad;
	}
		
	IncStat(co_call);

	IFDEBUG(D_CDUMP_REQ)
		printf("cons_connect ecnrq:\n");
		dump_buf(ecnrq, sizeof(*ecnrq));
	ENDDEBUG

	ASSERT( copcb->co_channel == 0);
	if( copcb->co_channel != 0) {
		printf("cons_connect PANIC: channel is 0x%x\n", copcb->co_channel);
	}

	error = choose_output(copcb->co_ifp, m, copcb->co_flags & CONSF_LOOPBACK);

	switch( error ) {
		case 0: /* ok */
			break;
		default: /* problem */
			printf("cons: PANIC: if_output returns 0x%x\n", error);
			cons_clear_and_detach( copcb, DONTCLEAR, PRC_ROUTEDEAD);
	}

bad:
	IFTRACE(D_CDATA)
		tptrace( TPPTmisc, 
		"cons_connect: choose (copcb m) returned  error\n", 
			copcb, m, error, 0);
	ENDTRACE
	return error;
}

/*
 * NAME:	cons_find()
 * CALLED FROM:
 *	cosns_output1() thus:
 *		cons_find( CONSF_DGM, dst, proto, 0, 0) where
 *		proto is one of { TP_proto, CLNP_proto }
 * FUNCTION and ARGUMENTS:
 *  Looks through list of connections for the destination,
 *  for one marked for the use indicated by flags.
 *  If none found, opens up a new connection.
 *   These connections will be eliminated by :
 *     a) slowtimo timer, or 
 *     b) the need for a new connection, when we've run out of resources.
 *  The argument flags describes the type of pcb we want - may
 *  specify multiplexing-ok, datagram use, etc.
 *  The argument proto points the the higher layer protocol that
 *  will be using this connection.
 * RETURN VALUE:
 *  returns a ptr to a pcb whose characteristics match those
 *  described by (flags, proto)
 */

Static struct cons_pcb *
cons_find(flags, dst, proto, addl_criteria, mask)
	u_int flags, mask;
	struct sockaddr_iso *dst;
	struct protosw *proto;
	int	(*addl_criteria)();
{
	register struct cons_pcb *copcb;
	register struct cons_pcb **copcblist = (struct cons_pcb **)Xpcblist;
	int s = splnet(); /* or whatever, for the device! */
	struct dte_addr dest_dte;
	int	 dummy;

	struct	copcb_descriptor {
		int	xd_qlen;
		struct cons_pcb *xd_pcb;
	} next_best = {
		0, (struct cons_pcb *)0
	};

	IFDEBUG(D_CFIND)
		printf("cons_find( flags 0x%x proto 0x%x) ", flags, proto);
	ENDDEBUG

	if ( iso_8208snparesolve(dst, &dest_dte, &dummy)) {
		ASSERT(0);
		return (struct cons_pcb *)0; /* error */
	}
	ASSERT(dummy == sizeof(struct dte_addr));

	for( copcb = *copcblist; copcb; copcb = *(++copcblist) ) {
		copcb = (struct cons_pcb *)copcb->co_next;
		while (copcb !=  *copcblist) {
			IFDEBUG(D_CFIND)
				printf(
				"cons_find: chan 0x%x flags 0x%x proto 0x%x state 0x%x \n", 
					copcb->co_channel, copcb->co_flags, copcb->co_proto, 
					copcb->co_state);
			ENDDEBUG
			/*
			 * if flags is a subset of the bits in co_flags, it will suffice
			 */
			if( ((copcb->co_flags & flags) == flags ) &&
				/* PHASE2: where do we get the mask if we use nsaps ????
				 * If dte addresses are used, then use
				 * nibble compare otherwise...???
				 */
#ifdef notdef
				iso_addrmatch1(&(copcb->co_faddr.siso_addr), &(dst->siso_addr)) 
#else
				dest_dte.dtea_niblen == copcb->co_peer_dte.dtea_niblen &&
				nibble_match( (char *)&(copcb->co_peer_dte.dtea_addr), 
					HIGH_NIBBLE, (char *)dest_dte.dtea_addr, 
					HIGH_NIBBLE, dest_dte.dtea_niblen)
#endif notdef
				&&
				(copcb->co_proto == proto)  &&
				(copcb->co_state >= MIN_USABLE_STATE)) {
					IFDEBUG(D_CFIND)
						printf(
						"cons_find: add'l criteria...\n" );
					ENDDEBUG
					if((copcb->co_state != OPEN) &&
						(next_best.xd_qlen > copcb->co_pending.ifq_len)) {
						next_best.xd_pcb = copcb;
						next_best.xd_qlen = copcb->co_pending.ifq_len;
					}
					if( !addl_criteria || (*addl_criteria)(copcb, mask) ) {
						goto found; /* have to break out of 2 loops */
					}
				}
			copcb = (struct cons_pcb *)copcb->co_next ;
		}
	}
#ifdef notdef
	/* TODO: 
	 * have a limit of the number of calls per desitination.
	 * If we didn't find one already open AND our limit for this
	 * destination hasn't been reached, return 0 'cause
	 * then the caller will open a new one.
	 * Otherwise return next_best.
	 * To do this we need some sort of per-destination info.
	 * Could go into the directory service. Oh, grotesque.
	 */
#endif notdef
	if( copcb == (struct cons_pcb *)0 ) {
		copcb = next_best.xd_pcb; /* may be zero too */
		IFDEBUG(D_CFIND)
			printf("NEXT_BEST! \n");
			dump_copcb(copcb, "find: next_best");
		ENDDEBUG
	}
found:

	splx(s);
		
	IFDEBUG(D_CFIND)
		printf("returns 0x%x \n", copcb);
	ENDDEBUG
	return copcb;
}


/*
 * NAME:	issue_clear_req()
 * CALLED FROM:
 *	cons_clear() and wherever we get an error from x.25 that makes us
 *  	want to close the vc on which it came, but don't have
 *		a copcb assoc. with that vc.
 * FUNCTION and ARGUMENTS:
 *  Creates an eicon_request for a clear request, returns it in an mbuf.
 *  (chan) is the channel on which to do the clear, (reason) is the 
 *  clear reason(diagnostic).
 * RETURN VALUE:
 *  returns E*
 */
Static int
issue_clear_req(chan, reason, ifp, loop)
	u_char 			chan, reason;
	struct	ifnet 	*ifp;
	int				loop;
{
	register struct mbuf 			*m;
	register struct mbuf 			*cdm;
	register struct eicon_request 	*ecnrq;
	struct e_clear_data 			*ecd; 

	IFDEBUG(D_CCONN)
		printf("issue_clear_req(0x%x, 0x%x, 0x%x, 0x%x)\n", 
			chan, reason, ifp, loop);
	ENDDEBUG
	m = m_getclr(M_DONTWAIT, MT_XCLOSE);
	if( !m ) {
		return ENOBUFS;
	}
	m->m_len = sizeof(struct eicon_request);
	ecnrq = mtod(m, struct eicon_request *);
	ecnrq->e_cmd = ECN_CLEAR;
	ecnrq->e_vc = chan & 0xff; 
	/* 
	 *  see p. 149 of 8208 for reasons (diagnostic codes)
	 */
	MGET(cdm, M_DONTWAIT, MT_XCLOSE);
	if( !cdm ) {
		m_freem(m);
		return ENOBUFS;
	}
	cdm->m_len = sizeof(struct e_clear_data); /* cause, diagnostic */
	e_data(ecnrq) = cdm;

	ecd = mtod(cdm, struct e_clear_data *);
	ecd->ecd_cause = 0x0; /* DTE initiated, diagnostic tells more */
	ecd->ecd_diagnostic = (u_char)reason;

	IncStat(co_clear_out);
	return choose_output(ifp, m, loop);
}


/*
 * NAME:	cons_clear()
 * CALLED FROM:
 *  cons_usrreq(), PRU_DISCONNECT,
 *  cons_slowtimo(), cons_free_lru()
 * FUNCTION and ARGUMENTS:
 *	Builds a clear request for the connection represented by copcb,
 *  gives it to the device.
 * ECN_CLEAR(request) takes e_vc only, returns adr_status.
 * RETURN VALUE:
 */

Static int 
cons_clear( copcb, reason) 
	register struct cons_pcb *copcb;
	u_char					reason;
{
	register struct mbuf			*m;
	int								error;

	IFDEBUG(D_CCONN)
		printf("cons_clear(0x%x, 0x%x)\n", copcb, reason);
	ENDDEBUG
	if( !copcb) {
		printf("cons PANIC: clear: No copcb\n");
		return 0;
	}
	while( copcb->co_pending.ifq_len > 0 ) {
		register int s = splimp();

		IF_DEQUEUE( &copcb->co_pending, m );
		splx(s);
		m_freem(m);
	}
	if( (copcb->co_state == CLOSED) || (copcb->co_state == CLOSING) )
		return 0;

#ifdef ARGO_DEBUG
	if( copcb->co_state == CONNECTING) {
		IFDEBUG(D_CCONN)
			dump_copcb(copcb, "clear");
		ENDDEBUG
	} else if( (copcb->co_channel == 0) || (copcb->co_channel == X_NOCHANNEL) ) {
		IFDEBUG(D_CCONN)
			dump_copcb(copcb, "clear");
		ENDDEBUG
	}
#endif ARGO_DEBUG

	copcb->co_state = CLOSING;

	IFDEBUG(D_CCONN)
		printf("cons_clear: channel 0x%x copcb 0x%x dst: ", 
			copcb->co_channel,  copcb);
		dump_isoaddr(&copcb->co_faddr);
		dump_copcb(copcb, "clear");
	ENDDEBUG

	error = issue_clear_req(copcb->co_channel, reason, copcb->co_ifp,
		copcb->co_flags & CONSF_LOOPBACK);
	copcb->co_channel = X_NOCHANNEL; 
	copcb->co_state = CLOSED;
	return error;
}


/* 
 * NAME:	cons_senddata()
 * CALLED FROM:
 *  cons_output(), consoutput(), consintr()
 * FUNCTION and ARGUMENTS:
 *	issued a data (write) command - if the device isn't ready,
 *  it enqueues the command on a per-connection queue.
 * RETURN VALUE:
 *	ENOBUFS
 *  Is responsible for freeing m0!
 *
 * ECN_SEND (write) 
 */

Static int 
cons_senddata(copcb, m0)
	register struct cons_pcb *copcb;
	struct mbuf *m0;
{
	register struct mbuf *m;
	register struct eicon_request *ecnrq;
	int s;

	IFDEBUG(D_CDATA)
		printf("cons_senddata( 0x%x, m 0x%x ) chan 0x%x", 
			copcb, m0, copcb->co_channel );
		printf(" co_lport 0x%x\n", copcb->co_lport);
	ENDDEBUG
	if( m0 == MNULL ) 
		return;
	ASSERT( m0->m_len > 0);
	if( m0->m_len <= 0) {
		printf("cons_senddata : BAD MLEN? 0x%x", m0->m_len);
	}

	touch(copcb);

	if( (copcb->co_state == CONNECTING) || (copcb->co_state == ACKWAIT) ) {
		IFDEBUG(D_CDATA)
			printf("senddata PUTTING ON PENDING Q copcb 0x%x state 0x%x\n", 
				copcb, copcb->co_state);
		ENDDEBUG
		s = splimp();
		if (IF_QFULL(&copcb->co_pending)) {
			IFDEBUG(D_CDATA)
				printf("senddata DROPPING m0 0x%x\n",  m0);
			ENDDEBUG
			IF_DROP(&copcb->co_pending);
			if(copcb->co_ifp) {
				copcb->co_ifp->if_snd.ifq_drops ++;
			}
			IncStat(co_Xdrops);
			copcb->co_ifp->if_oerrors ++;
			splx(s);
			m_freem (m0);

			if( copcb->co_proto  && copcb->co_proto->pr_ctlinput ) {
				(*copcb->co_proto->pr_ctlinput)(PRC_QUENCH, 
				(struct sockaddr_iso *)&copcb->co_faddr, 
				(caddr_t)copcb);
				
				return 0;
			} else
				return E_CO_QFULL;
		}
		IFDEBUG(D_CDATA)
			printf("Putting 0x%x on 0x%x->pending Q\n", m0, copcb);
		ENDDEBUG
		IF_ENQUEUE( &copcb->co_pending, m0 );
		splx(s);
		return 0;
	}
	if(copcb->co_channel == 0 ) {
		return E_CO_CHAN;
	}
	ASSERT( copcb->co_state == OPEN);

	m = m_getclr(M_DONTWAIT, MT_XDATA);
	if( !m ) {
		copcb->co_ifp->if_oerrors ++;
		m_freem (m0);
		return ENOBUFS;
	}
	m->m_len = sizeof(struct eicon_request);
	ecnrq = mtod(m, struct eicon_request *);
	ecnrq->e_pcb = (caddr_t)copcb;
	if( copcb->co_myself != copcb ) {
		struct mbuf *mm;
		/* TODO: REMOVE THIS DEBUGGING HACK */
		ASSERT(0);
		printf("BAD e_pcb from HL (0x%x,0x%x)\n", copcb, copcb->co_myself);
		mm = dtom( copcb );
		if(mm->m_type == MT_FREE)
			printf("FREED MBUF!\n");
		return ENETDOWN;
	}
	ASSERT( copcb->co_channel != 0);
	ASSERT( copcb->co_channel != X_NOCHANNEL);
	ecnrq->e_vc = (copcb->co_channel & 0xff);
	ecnrq->e_cmd = ECN_SEND;
	e_data(ecnrq) = m0;
	{
		/* TODO: REMOVE THIS DEBUGGING HACK */
		struct mbuf *thedata = e_data(ecnrq);
		u_int *firstint = mtod( thedata, u_int *);
		
		if( (*firstint & 0xff000000) != 0x81000000 ) {
			/* not clnp */
			switch( ((*firstint) & 0x00ff0000) >> 20 ) {
			case 0x1:
			case 0x2:
			case 0x3:
			case 0x6:
			case 0x7:
			case 0x8:
			case 0xc:
			case 0xd:
			case 0xe:
			case 0xf:
				break;
			default:
				printf(" ECN_SEND! BAD DATA\n" );
				dump_buf( thedata, 20 + 12 );
				m_freem( m0 );
				return ENETDOWN;
			}
		}
	}

	ecnrq->e_info = 0;

	IFDEBUG(D_CDUMP_REQ)
		printf("senddata ecnrq\n");
	ENDDEBUG
	IncStat(co_send);

	ASSERT( copcb->co_state == OPEN );
	copcb->co_state = ACKWAIT;

	if( copcb->co_myself != copcb ) {
		struct mbuf *mm;
		/* TODO: REMOVE this and all mention of co_myself */
		ASSERT(0);
		printf("BAD e_pcb TO THE BOARD ecn (0x%x) cmd 0x%x\n", 
			ecnrq->e_pcb, ecnrq->e_cmd);
		mm = dtom( copcb );
		if(mm->m_type == MT_FREE)
			printf("FREED MBUF!\n");
		dump_buf (ecnrq, sizeof (*ecnrq));
		return ENETDOWN;
	}

	return 
	  choose_output(copcb->co_ifp, dtom(ecnrq), copcb->co_flags&CONSF_LOOPBACK);
}

/*
 * NAME:	cons_send_on_vc()
 * CALLED FROM:
 *  tp_error_emit()
 * FUNCTION and ARGUMENTS:
 *  Take a packet(m0), of length (datalen) from tp and 
 * send it on the channel (chan).
 *	
 * RETURN VALUE:
 *  whatever (E*) is returned form the net layer output routine.
 */
int
cons_send_on_vc(chan, m, datalen)
	int				chan;
	struct mbuf 	*m;
	int				datalen;
{
	struct cons_pcb	*copcb = (struct cons_pcb *)0;

	if(m == MNULL)
		return;

	if((copcb = 
#ifdef ARGO_DEBUG
		cons_chan_to_pcb( chan, __LINE__ )
#else ARGO_DEBUG
		cons_chan_to_pcb( chan )
#endif ARGO_DEBUG
			) == (struct cons_pcb *)0 )
		return E_CO_CHAN; 
	IFDEBUG(D_CCONS)
		printf("cons_send_on_vc m 0x%x m_len 0x%x\n", m, m->m_len);
	ENDDEBUG
	return cons_senddata( copcb, m);
}

/*
 * NAME:	cons_output()
 * CALLED FROM:
 *  tpiso_output(), can have whatever interface we want it to...
 *  tpiso_output() decides whether to give a packet to CLNP or to 
 *  cons; if the latter, it calls this routine.
 * FUNCTION and ARGUMENTS:
 *  tp has alloc-ed a pcb - but it may not be open.
 *  some classes of tp may allow multiplexing, in which
 *  case, you may choose to send the data on ANOTHER cons connection.
 *  This decides which net connection to use, opens one if necessary.
 *  Then it sends the data.
 */

cons_output(isop, m, len, isdgm)
	struct isopcb 	*isop;
	struct mbuf 	*m;
	int 			len;
	int				isdgm;
{
	struct cons_pcb	*copcb = (struct cons_pcb *)0;
	int				error;
	int 			s = splnet();

	IFDEBUG(D_CCONS)
		printf("cons_output( isop 0x%x, m 0x%x, len 0x%x, dgm 0x%x )\n", 
			isop,m,len, isdgm);
	ENDDEBUG

	if( m == MNULL )
		return 0;
	ASSERT(m->m_len > 0);
	if( isdgm ) {
		error = cosns_output1(0, m, &isop->isop_faddr, TP_proto, isop);
		IFDEBUG(D_CDATA)
			if(error)
			printf("cosns_output1 RETURNS ERROR 0x%x\n", error);
		ENDDEBUG
		return error;
	}

	if( isop->isop_chanmask  || isop->isop_negchanmask) {
		register int	mask = isop->isop_chanmask;
		register int	chan = 1;

		if( mask == 0)
			mask = isop->isop_negchanmask;

		for ( chan=1; (mask & 1)==0; chan++,mask>>=1 ) ;

		if( isop->isop_chanmask == 0 )
			chan = -chan;

		IFDEBUG(D_CCONS)
			printf(
			"cons_output: isop 0x%x cmask 0x%x negmask 0x%x, chan 0x%x\n",
			isop, isop->isop_chanmask, isop->isop_negchanmask, chan);
		ENDDEBUG
		ASSERT( chan != 0);
#ifdef ARGO_DEBUG
		copcb = cons_chan_to_pcb( chan, __LINE__ );
#else ARGO_DEBUG
		copcb = cons_chan_to_pcb( chan );
#endif ARGO_DEBUG
	}
	if( copcb == (struct cons_pcb *)0 ) {
		/* get a new one */

		if(( error = cons_pcballoc(&dummysocket, &cons_isopcb, CONSF_OCRE, 
												TP_proto, &copcb)) != EOK ) {
			IFDEBUG(D_CCONS)
				printf("cosns_output: no copcb; returns 0x%x\n", error);
			ENDDEBUG
			(void) m_freem (m);
			splx(s);
			return error ;
		}

		/* abbreviated form of iso_pcbconnect(): */
		bcopy((caddr_t)&isop->isop_faddr, (caddr_t)&copcb->co_faddr, 
									sizeof(struct sockaddr_iso));

		if ( error = cons_connect( copcb ) ) { /* if it doesn't work */
			/* oh, dear, throw packet away */
			remque((struct isopcb *)copcb);
			(void) m_free(dtom(copcb));
			(void) m_freem( m );
			splx(s);
			return error;
		}

		if( copcb->co_socket ) {
			while( (copcb->co_state != OPEN) && 
				!(error = copcb->co_socket->so_error) ) {
				IFDEBUG(D_CCONS)
					printf(
	"SLEEP1 copcb 0x%x isop 0x%x state 0x%x chan 0x%x mask 0x%x neg 0x%x\n",
						copcb, isop, copcb->co_state, copcb->co_channel, 
						((struct isopcb *)isop)->isop_chanmask,
						((struct isopcb *)isop)->isop_negchanmask
					);
				ENDDEBUG
				sleep( (caddr_t)&copcb->co_state, PZERO+1 );
				IFDEBUG(D_CCONS)
					printf("AFTER SLEEP 1 chan 0x%x chanmask 0x%x negchanmask 0x%x\n",
						copcb->co_channel, isop->isop_chanmask, 
						isop->isop_negchanmask);
				ENDDEBUG
			}
			if( !error )
				SET_CHANMASK( isop, copcb->co_channel);
		}

	} 

	IFDEBUG(D_CDATA)
		printf("cons_output calling senddata(0x%x 0x%x)\n", copcb, m);
		ASSERT(m != MNULL);
		ASSERT(m->m_len != 0);
	ENDDEBUG

	if( !error )
		error =  cons_senddata( copcb, m);
	splx(s);
	return error;
}

/*
 * NAME:	cons_openvc()
 * CALLED FROM:
 *  TP when it decides to open a VC for TP 0
 * FUNCTION:
 *  opens a connection and stashes the pcb info in the socket
 *  substitute for iso_pcbconnect/ in_pcbconnect for the class 0 case
 *  only.
 */
int
cons_openvc(copcb, faddr, so)
	struct cons_pcb 			*copcb;
	struct	sockaddr_iso	*faddr;
	struct	socket			*so;
{
	int 					error = 0;
	int 					s = splnet();
	struct cons_pcb 		*cons_chan_to_pcb();


	ASSERT( copcb->co_socket == so );
	IFTRACE(D_CCONN)
		tptrace(TPPTmisc, "cons_openvc( copcb so )\n", copcb, so, 0, 0);
	ENDTRACE
	IFDEBUG(D_CCONN)
		printf("cons_openvc( copcb 0x%x, so 0x%x )\n", copcb,so);
	ENDDEBUG
	/*
	 * initialize the copcb part of the isopcb
	 */
	copcb->co_ttl = copcb->co_init_ttl = X25_TTL;
	copcb->co_flags = CONSF_OCRE;
	copcb->co_proto = TP_proto;
	copcb->co_pending.ifq_maxlen = CONS_IFQMAXLEN;

	/* abbreviated form of iso_pcbconnect(): */
	bcopy((caddr_t)faddr, (caddr_t)&copcb->co_faddr, 
								sizeof(struct sockaddr_iso));

	ASSERT( copcb->co_socket == so );
	if( error = cons_connect( copcb ) )
		goto done;
	while( (copcb->co_state != OPEN) && !(error = so->so_error) ) {
		IFDEBUG(D_CCONS)
			printf(
		"SLEEP2 copcb 0x%x state 0x%x chan 0x%x mask 0x%x neg 0x%x\n",
				copcb, copcb->co_state, copcb->co_channel, 
				copcb->co_chanmask,
				copcb->co_negchanmask
			);
		ENDDEBUG
		sleep( (caddr_t)&copcb->co_state, PZERO+2 );
		IFDEBUG(D_CCONS)
			printf("AFTER SLEEP2 chan 0x%x chanmask 0x%x negchanmask 0x%x\n",
				copcb->co_channel, copcb->co_chanmask, 
				copcb->co_negchanmask);
		ENDDEBUG
	}
	if( !error )
		SET_CHANMASK( (struct isopcb *)copcb, copcb->co_channel); 
done:
	ASSERT( copcb->co_socket == so );
	splx(s);

	IFDEBUG(D_CCONN)
		printf("cons_openvc: copcb 0x%x error 0x%x\n", copcb, error );
	ENDDEBUG
	return error;
}

/*
 * NAME:	cons_netcmd()
 * CALLED FROM:
 *  tp_route_to() when it decides to accept or reject an incoming
 *  connection it calls this.
 * FUNCTION:
 *  either closes the cons connection named by (channel)
 *  or associates the copcb with the channel #.
 *	and removes the old copcb from the tp_incoming_pending list.
 */
int
cons_netcmd(cmd, isop, channel, isdgm)
	int 					cmd;
	struct isopcb 			*isop; 
	int						channel;
{
	int 					s = splnet();
	int 					error = 0;
	struct cons_pcb 		*copcb = (struct cons_pcb *)0;
	struct cons_pcb 		*cons_chan_to_pcb();

	IFTRACE(D_CCONN)
		tptrace(TPPTmisc, "cons_netcmd( cmd isopcb channel isdgm)\n", 
			cmd,isop,channel, isdgm);
	ENDTRACE
	IFDEBUG(D_CCONN)
		printf("cons_netcmd( cmd 0x%x, isop 0x%x, channel 0x%x, isdgm 0x%x)\n", 
			cmd,isop,channel, isdgm);
		if( isop )
			printf("cons_netcmd: isop->socket 0x%x\n", 
			isop->isop_socket);
	ENDDEBUG
	ASSERT(cmd != CONN_OPEN);

	/* Can we find a cons-level pcb based on channel? */
	if(channel) {
		if((copcb = 
#ifdef ARGO_DEBUG
			cons_chan_to_pcb( channel, __LINE__ )
#else ARGO_DEBUG
			cons_chan_to_pcb( channel)
#endif ARGO_DEBUG
				) == (struct cons_pcb *)0) {
			error = ECONNABORTED;
			splx(s);
			return error;
		}
		if( copcb == (struct cons_pcb *) isop ) {
			copcb = (struct cons_pcb *)0;
			/* avoid operating on a pcb twice */
		} else {
			/* if isop is null (close/refuse):
			 * this would remove from the TP list, which is NOT what we want 
			 * so only remove if there is an isop (gag)
			 */
			if( isop ) {
				remque((struct cons_pcb *)copcb); /* take it off pending list */
			} else {
				ASSERT( (cmd == CONN_CLOSE) || (cmd == CONN_REFUSE) );
			}
		}
	}
	/* now we have one of these cases:
	 * 1) isop is non-null and copcb is null 
	 * 2) isop is non-null and copcb is non-null and they are different
	 * 3) isop is null and copcb is non-null 
	 */
	ASSERT( (isop != (struct isopcb *)0) || (copcb != (struct cons_pcb *)0));

	switch(cmd) {

		case CONN_CONFIRM:
			if( isdgm ) {
				/* we want two separate pcbs */
				/* if we don't have a copcb, get one */

				if( copcb == (struct cons_pcb *)0 ) {
					if(( error = cons_pcballoc(&dummysocket, &cons_isopcb, 
						((struct cons_pcb *)isop)->co_flags,
						TP_proto, &copcb)) != EOK )
							return error;
					/* copy missing info from isop */
					copcb->co_laddr = isop->isop_laddr;
					copcb->co_faddr = isop->isop_faddr;
					/* don't care about tsuffices */
					((struct cons_pcb *)isop)->co_channel  = 0; 
												/* no longer used */

					copcb->co_ifp = ((struct cons_pcb *)isop)->co_ifp ;
					ASSERT( copcb->co_pending.ifq_len == 0 );

				} else {
					insque((struct isopcb *)copcb, 
						(struct isopcb *)&cons_isopcb); 
				}
				copcb->co_state = OPEN;
				copcb->co_flags |= CONSF_DGM;
				copcb->co_channel = channel;
				ASSERT(copcb->co_channel != 0);

				IFDEBUG(D_CCONN)
					printf("cons_netcmd: put 0x%x on regular list \n", copcb);
				ENDDEBUG
			} else { 
				/* must be TP 0, since this is never called from XTS code */
				/* we want ONE pcb, namely isop.
				 * If this TPE were the active side,
				 * there ought not to be a copcb, since TP should
				 * know that you can't send a CR with dgm and negot down
				 * to non-dgm.
				 * If this TPE were the passive side, we want to copy from
				 * the copcb that was on the pending list, and delete the
				 * pending copcb.
				 */
				if( copcb ) {
					IFDEBUG(D_CCONN)
						printf("cons_netcmd: copied info from 0x%x to 0x%x\n", 
							copcb, isop);
					ENDDEBUG
					isop->isop_laddr = copcb->co_laddr;
					isop->isop_faddr = copcb->co_faddr;
					/* tsuffices, socket should be there already */
					((struct cons_pcb *)isop)->co_flags = 
									copcb->co_flags & ~CONSF_DGM;
					((struct cons_pcb *)isop)->co_init_ttl = copcb->co_init_ttl;
					touch(((struct cons_pcb *)isop));
					((struct cons_pcb *)isop)->co_channel = channel;
					((struct cons_pcb *)isop)->co_ifp = copcb->co_ifp;
					((struct cons_pcb *)isop)->co_proto = copcb->co_proto;
					((struct cons_pcb *)isop)->co_myself = 
						(struct cons_pcb *)isop;
					SET_CHANMASK( isop, ((struct cons_pcb *)isop)->co_channel );
					ASSERT( copcb->co_pending.ifq_len == 0 );

					/* get rid of the copcb that was on the pending list */
					(void) m_free(dtom(copcb));
				} 
				((struct cons_pcb *)isop)->co_state = OPEN;
			}
			break;

		case CONN_CLOSE:
		case CONN_REFUSE:
			/* if dgm then ignore; the connections will 
			 * be re-used or will time out
			 */
			if( isdgm ) 
				break;

			/* we should never come in here with both isop and copcb
			 * unless is dgm, hence the following assertion:
			 */
			ASSERT( (copcb == (struct cons_pcb *)0) || 
				(isop == (struct isopcb *)0) );

			/* close whichever pcb we have */
			if( copcb )
				error = cons_clear(copcb, (cmd == CONN_CLOSE)?
					E_CO_HLI_DISCN:E_CO_HLI_REJT);
			if( isop )
				error = cons_clear((struct cons_pcb *)isop, (cmd == CONN_CLOSE)?
					E_CO_HLI_DISCN:E_CO_HLI_REJT);

			if(copcb &&  (copcb->co_socket == (struct socket *)0) ) {
				ASSERT( copcb->co_flags & (CONSF_DGM | CONSF_ICRE) );
				(void) m_free(dtom(copcb)); /* detached */
			}
			/* isop will always be detached by the higher layer */
			break;
		default:
			error = EOPNOTSUPP;
			break;
	}
	splx(s);

	IFDEBUG(D_CCONN)
		printf("cons_netcmd returns 0x%x: isop 0x%x\n", isop, error );
	ENDDEBUG
	return error;
}


/*
 * NAME:	addr_proto_consistency_check()
 * CALLED FROM: cons_incoming()
 * FUNCTION and ARGUMENTS:
 *  Enforces a set of rules regarding what addresses will serve
 *  what protocol stack.  This is a kludge forced upon us by the
 *  fact that there's no way to tell which NET layer you want to
 *  run when opening a socket.  Besides, no doubt, OSI directory
 *  services won't advertise any kind of a protocol stack with the
 *  NSAPs.  sigh.
 * RETURNS
 * 	EAFNOSUPPORT or EOK.
 */
Static int
addr_proto_consistency_check(proto, addr) 
	int						proto;
	struct 	sockaddr_iso	*addr;
{
	switch( proto ) {
		case ISOPROTO_CLNP:
			break;

		case ISOPROTO_INACT_NL:
		case ISOPROTO_CLTP:
			return E_CO_HLI_PROTOID;

		case ISOPROTO_TP:
		case ISOPROTO_X25:
			/* hl is TP or X.25 */
			if (addr->siso_addr.isoa_afi != AFI_37)
				return E_CO_AIWP;
				/* kludge - necessary because this is the only type of
				 * NSAP we build for an incoming NC
				 */
			break;
		default: /* unsupported */
			return E_CO_HLI_PROTOID;
	}
	return EOK;
}
/* 
 * NAME:	cons_incoming()
 * CALLED FROM:
 *  consintr() for incoming OPEN
 * FUNCTION and ARGUMENTS:
 *  Determines which higher layer gets this call, and 
 *  thus whether to immediately accept, reject, or to let the
 *	higher layer determine this question. 
 */
Static 
cons_incoming(ifp, ecnrq)
	struct ifnet 					*ifp;
	register struct eicon_request 	*ecnrq;
{
	struct sockaddr_iso 	me;
	struct sockaddr_iso 	peer;
	struct cons_pcb			*copcb;
	int 					loop = 0;
	int						proto =0;
	int						error = 0;
	struct	dte_addr		peer_dte;

	IFDEBUG(D_INCOMING)
		printf("consincoming enter: ifp 0x%x ecnrq 0x%x\n", ifp, ecnrq);
	ENDDEBUG
	bzero( &me, sizeof(me));
	error = parse_facil( mtod(e_data(ecnrq), caddr_t), 
						(e_data(ecnrq))->m_len, &me, &peer, &proto,
						&peer_dte);
	loop = is_me( &peer ); /* <-- THIS may be a problem :
							* peer may be nonsense.
							* We can only expect that WE will do it right
							* and never will we get an error return from
							* parse_facil on a facil that WE generated,
							* so if garbage comes in, peer will be garbage,
							* and loop will be false.
							*/
	if( error != EOK ) {
		(void) issue_clear_req(ecnrq->e_vc, error, ifp, loop); 
		IncStat(co_parse_facil_err);
		IncStat(co_Rdrops);
		return;
	}

	if( (error = addr_proto_consistency_check(proto, &me)) != EOK ) {
		/* problem with consistency */
		(void) issue_clear_req(ecnrq->e_vc, error, ifp, loop);
		IncStat(co_addr_proto_consist_err);
		IncStat(co_Rdrops);
		return;
	} else {
		switch( proto ) {
			case ISOPROTO_X25:
				copcb = (struct cons_pcb *)
						((struct cons_pcb *)(&cons_isopcb))->co_next;

				while (copcb != (struct cons_pcb *)&cons_isopcb) {
					if( copcb->co_lport == me.siso_tsuffix ) {
						/* for cons "transport service",
						 * multiplexing is not allowed 
						 */
						if( !copcb->co_socket ) {
							printf(
							"PANIC cons_incoming NOT TP but no sock\n");
							copcb = (struct cons_pcb *)0;
							break;
						}
						if( copcb->co_socket->so_options & SO_ACCEPTCONN ) {
							struct cons_pcb *newx;

							newx = (struct cons_pcb *)
									sonewconn(copcb->co_socket)->so_pcb;
							newx->co_laddr = copcb->co_laddr;
							newx->co_peer_dte = peer_dte;
							newx->co_proto = copcb->co_proto;
							newx->co_myself = newx;
							touch(copcb);
							copcb = newx;
							soisconnected(copcb->co_socket); 
							break;
						} /* else keep looking */ 
					}
					copcb = (struct cons_pcb *)copcb->co_next;
				}
				if (copcb == (struct cons_pcb *)&cons_isopcb)
					copcb = (struct cons_pcb *) 0;
				break;

			case ISOPROTO_TP:
				ASSERT( me.siso_tsuffix == 0 );
				/*
				 * We treat this rather like we do for CLNP.  
				 * TP can't tell which socket
				 * wants this until the TP header comes in, so there's no way
				 * to associate this channel with a tpcb/isopcb.
				 * We assume data will arrive (a CR TPDU) and be given to TP along with
				 * the channel number.  We can then expect TP to call us with
				 * the channel number and pcb ptr, telling us to keep this connection
				 * or clear it.
				 * Now, tp will have created an isopcb in the tp_isopcb list.
				 * We will have to keep another copcb though, because there is no
				 * 1-1 correspondence between socket and copcb when multiplexing
				 * is allowed. 
				 * But we want to save the peer address, ifp, and state, proto.
				 * If the channel should clear before TP responds, we need
				 * to know that also, so we create a tp-pending list...
				 */
				if( cons_pcballoc(&dummysocket, &tp_incoming_pending, 
								CONSF_ICRE, TP_proto, &copcb) != EOK )  {
					copcb = (struct cons_pcb *)0;
				} else {
					copcb->co_peer_dte = peer_dte;
				}
				break;


			case ISOPROTO_CLNP:
				if( cons_pcballoc(&dummysocket, &cons_isopcb, 
						CONSF_ICRE | CONSF_DGM, CLNP_proto, &copcb ) != EOK ) {
					/* choke */
					copcb = (struct cons_pcb *)0;
				} else {
					copcb->co_peer_dte = peer_dte;
				}
				break;

		default:
			panic("cons_incoming");
		} /* end switch */

		if(copcb) {
			touch(copcb);
			copcb->co_channel = (int)ecnrq->e_vc;
			ASSERT( copcb->co_channel != 0);
			copcb->co_state = OPEN;
			copcb->co_ifp = ifp;
			copcb->co_laddr = me;
			copcb->co_faddr = peer;
			if(loop)
				copcb->co_flags |= CONSF_LOOPBACK;
			IFDEBUG(D_CADDR)
				printf("cons_incoming found XPCB 0x%x, loop 0x%x\n", 
						copcb, loop);
				printf("\nco_laddr: ");
				dump_buf(&copcb->co_laddr, sizeof(copcb->co_laddr));
				printf("\nco_faddr: ");
				dump_buf(&copcb->co_faddr, sizeof(copcb->co_faddr));
				printf("\n");
			ENDDEBUG
		} else {
			ifp->if_ierrors ++;
			(void) issue_clear_req(ecnrq->e_vc, E_CO_OSI_UNSAP, ifp, loop);
			IncStat(co_no_copcb);
			IncStat(co_Rdrops);
		}
	} 
	/* caller frees the mbuf so we don't have to do any such thing */
}

/*
 **************************** DEVICE cons ***************************
 */

/*
 * NAME:	cosns_output()
 * CALLED FROM:
 *  clnp - this routine is given as the device-output routine
 *  for the adcom driver.
 * FUNCTION and ARGUMENTS:
 *  (ifp) is the cons/adcom, found by routing function.
 *  (m0) is the clnp datagram.
 *  (dst) is the destination address
 * This routine finds an x.25 connection for datagram use and
 * sends the packet.
 */
int
cosns_output(ifp, m0, dst)
{
	return cosns_output1(ifp, m0, dst, CLNP_proto, NULL);
}

/* DEBUGGING ONLY? */
int	total_cosns_len = 0;
int	total_cosns_cnt = 0;
int	total_pkts_to_clnp = 0;

/*
 *		The isop is passed here so that if we have set x25crud in the
 *		pcb, it can be passed down to cons_connect. It could be null
 *		however, in the case of tp4/x25/clnp
 */
Static int
cosns_output1(ifp, m0, dst, proto, isop)
	struct ifnet *ifp;
	register struct mbuf *m0;
	struct sockaddr_iso *dst;
	struct protosw *proto;
	struct isopcb	*isop;		/* NULL if coming from clnp */
{
	register struct cons_pcb *copcb;
	int 					s = splnet();
	int						error = 0;

	{ 	register struct mbuf *n=m0;
		register int len = 0;

		for(;;) {
			len += n->m_len;
			if (n->m_next == MNULL ) {
				break;
			}
			n = n->m_next;
		}
		total_cosns_len += len;
		total_cosns_cnt ++;

	}

	IFDEBUG(D_CCONS)
		printf("cosns_output1( ifp 0x%x, m 0x%x, dst 0x%x )\n", ifp, m0, dst );
	ENDDEBUG
	if ( ! (copcb = cons_find( CONSF_DGM, dst, proto, 0, 0) )) {
		struct cons_pcb *newcopcb; /* so we can pass addr of this to pcballoc */

		if( (error = cons_pcballoc(&dummysocket, &cons_isopcb, 
				CONSF_DGM | CONSF_OCRE, proto, &newcopcb) )  != EOK ) {
			IFDEBUG(D_CCONS)
				printf("cosns_output: no copcb; returns \n");
			ENDDEBUG
			(void) m_freem(m0);
			goto done;
		}
		copcb = newcopcb;

		/* abbreviated form of iso_pcbconnect(): */
		bcopy((caddr_t)dst, (caddr_t)&copcb->co_faddr, 
									sizeof(struct sockaddr_iso));
		
		/* copy x25crud into copcb if necessary */
		if ((isop != NULL) && (isop->isop_x25crud_len > 0)) {
			bcopy(isop->isop_x25crud, copcb->co_x25crud, 
				isop->isop_x25crud_len);
			copcb->co_x25crud_len = isop->isop_x25crud_len;
		}

		copcb->co_ifp = ifp; /* NULL IF COMING FROM TP4! */

		if ( error = cons_connect( copcb ) ) { /* if it doesn't work */
			/* oh, dear, throw packet away */
			remque((struct isopcb *)copcb);
			(void) m_free(dtom(copcb));
			(void) m_freem(m0);
			goto done;
		}
	}
	IFDEBUG(D_CDATA)
		printf("cosns_output1 @ senddata: state 0x%x flags 0x%x channel 0x%x\n",
			copcb->co_state, copcb->co_flags, copcb->co_channel);
	ENDDEBUG
	ASSERT(copcb->co_channel != X_NOCHANNEL);
	error = cons_senddata(copcb, m0); 
done:
	splx(s);
	return error;
}


/*
 **************************** TRANSPORT cons ***************************
 */


/*
 * NAME:	cons_detach()
 * CALLED FROM:
 *  cons_usrreq() on PRU_DETACH
 *  cons_netcmd() when TP releases a net connection
 *	cons_slowtimo()  when timeout releases a net connection
 * FUNCTION and ARGUMENT:
 *  removes the copcb from the list of copcbs in use, and frees the mbufs.
 *  detaches the pcb from the socket, where a socket exists.
 * RETURN VALUE:
 *  ENOTCONN if it couldn't find the copcb in the list of connections.
 */

Static int 
cons_detach( copcb )
	register struct cons_pcb *copcb;
{
	struct socket *so = copcb->co_socket;

	IFDEBUG(D_CCONN)
		printf("cons_detach( copcb 0x%x )\n", copcb);
	ENDDEBUG
	if(so) {
		if (so->so_head) {
			if (!soqremque(so, 0) && !soqremque(so, 1))
				panic("sofree dq");
			so->so_head = 0;
		}
		((struct isopcb *)copcb)->isop_options = 0; /* kludge */
		iso_pcbdetach(copcb); /* detaches from so */
	} else {
		remque((struct isopcb *)copcb);
		(void) m_free(dtom(copcb));
	}
}

Static int
cons_clear_and_detach(copcb, clearreason, ctlcmd)
	register struct cons_pcb *copcb;
	int					clearreason;
	int						ctlcmd;
{
	IFDEBUG(D_CCONN)
		printf("Clear and Detach (0x%x, 0x%x, 0x%x)\n", 
						copcb, clearreason, ctlcmd);
	ENDDEBUG
	if( clearreason != DONTCLEAR ) {
		(void) cons_clear( copcb ,  clearreason );
	}
	if( copcb->co_proto  && copcb->co_proto->pr_ctlinput )
		(*copcb->co_proto->pr_ctlinput)(ctlcmd, 
			(struct sockaddr_iso *)&copcb->co_faddr, (caddr_t)copcb);

	if( copcb->co_socket == (struct socket *)0 ) {
		/* tp4, clnp users only */
		(void) cons_detach( copcb );
	} /* else detach will be called by the socket's closing */
		else {
			ASSERT( copcb->co_socket != &dummysocket );
			ASSERT( (copcb->co_flags & CONSF_DGM) == 0 );
	}
	IFDEBUG(D_CCONN)
		printf("END OF Clear and Detach (0x%x, 0x%x, 0x%x)\n", 
						copcb, clearreason, ctlcmd);
	ENDDEBUG
}

Static int
cons_pcbbind( copcb, nam )
	register struct cons_pcb *copcb;
	struct mbuf *nam;
{
	int error;

	if( error = iso_pcbbind( copcb, nam) )
		return error;

	/* iso_pcbbind already ensured that if port < 1024 it's superuser */
	/* Now we check: must be in range 0 .. 23 or in range 1024 .. 99 */

	if( (copcb->co_lport < X25_PORT_RESERVED)  || 
			 ((copcb->co_lport >= ISO_PORT_RESERVED) && 
			  (copcb->co_lport <= X25_PORT_USERMAX))) {
		munge( copcb->co_lport, (&copcb->co_laddr)->siso_addr.t37_idi +
			ADDR37_IDI_LEN, 1 /* nibble */);
		munge( copcb->co_fport, (&copcb->co_faddr)->siso_addr.t37_idi +
			ADDR37_IDI_LEN, 1 /* nibble */);
		 return 0;
	} else 
		return EADDRNOTAVAIL;
}
/*
 * NAME:	cons_usrreq()
 * CALLED FROM:
 *  user level via proto switch
 * FUNCTION and ARGUMENTS:
 *  so : socket
 *  req: which PRU* request
 *  m : data or mbuf ptr into which to stash data
 *	nam: mbuf ptr which is really a sockaddr_iso
 *  ifq: in PRU_CONTROL case, an ifnet structure 
 * RETURN VALUE:
 *  ENOTCONN if trying to do something which requires a connection
 * 	 and it's not yet connected
 *  EISCONN if trying to do something which cannot be done to a connection
 * 	 but it's connected
 * 	ENOBUFS if ran out of mbufs
 * 	EWOULDBLOCK if in nonblocking mode & can't send right away
 * 	EOPNOSUPP if req isn't supported
 * 	E* other passed up from lower layers or from other routines
 */

cons_usrreq(so, req, m, nam, ifp)
	struct socket *so;
	u_int req;
	struct mbuf *m, *nam;
	int *ifp; 
{	
	struct cons_pcb *copcb =  (struct cons_pcb *)so->so_pcb;
	int 					s = splnet();
	int 					error = 0;

	IFDEBUG(D_CCONS)
		printf("cons_usrreq 0x%x so 0x%x copcb 0x%x\n", req, so, copcb);
	ENDDEBUG
	if (req == PRU_CONTROL) {
		error =  iso_control(so, (int)m, (caddr_t)nam, (struct ifnet *)ifp);
		splx(s);
		return error;
	}
	if (copcb == (struct cons_pcb *)0  &&  req != PRU_ATTACH) {
		splx(s);
		return ENOTCONN;
	}

	switch (req) {

	case PRU_ATTACH:
		if (copcb) {
			error = EISCONN;
			break;
		}
		soreserve(so, X25_SBSIZE, X25_SBSIZE); /* CONS size */
		error = cons_pcballoc(so, &cons_isopcb, CONSF_XTS, X25_proto, &copcb );
		break;

	case PRU_ABORT: 	/* called from close() */
		/* called for each incoming connect queued on the parent (accepting) 
		 * socket (SO_ACCEPTCONN); 
		 */
		 error = cons_detach ( copcb );
		 break;

	case PRU_DETACH: 	/* called from close() */
		/* called after disconnect was called iff was connected at the time
		 * of the close, or directly if socket never got connected */
		error = cons_detach ( copcb );
		break;

	case PRU_SHUTDOWN:
		/* recv end may have been released; local credit might be zero  */
	case PRU_DISCONNECT:
		soisdisconnected(so);
			error = cons_clear(copcb, E_CO_HLI_DISCN); 
		break;

	case PRU_BIND:
		error = cons_pcbbind( copcb, nam);
		break;

	case PRU_LISTEN:
		if (copcb->co_lport == 0) 
			error = cons_pcbbind( copcb, 0 );
		break;


	case PRU_SOCKADDR: {
			struct sockaddr_iso *siso = mtod(nam, struct sockaddr_iso *);

			nam->m_len = sizeof (struct sockaddr_iso);
			if(copcb->co_ifp) 
				bcopy( (caddr_t)&copcb->co_laddr,
						(caddr_t)siso, sizeof(struct sockaddr_iso) );

			((struct sockaddr_iso *)siso)->siso_tsuffix = copcb->co_lport;
		}
		break;

	case PRU_PEERADDR:
		if( (so->so_state & SS_ISCONNECTED) && 
			(so->so_state & SS_ISDISCONNECTING) == 0) {
				struct sockaddr_iso *siso = mtod(nam, struct sockaddr_iso *);

			nam->m_len = sizeof (struct sockaddr_iso);
			bcopy( (caddr_t)&copcb->co_faddr, (caddr_t)siso, 
									sizeof(struct sockaddr_iso) );
		} else 
			error = ENOTCONN;
		break;

	case PRU_CONNECT:
		/* TODO: We need to bind to the RIGHT interface. 
		 * The only way to have the right interface is to have
		 * the right route.
		 */
		IFDEBUG(D_CCONN)
			printf("PRU_CONNECT 1: local tsuffix 0x%x so->so_head 0x%x nam:\n", 
				copcb->co_lport, so->so_head);
			dump_isoaddr( mtod(nam, struct sockaddr_iso *) );
		ENDDEBUG
		if (copcb->co_lport == 0) {
			if( error = cons_pcbbind( copcb, 0 ))
				break;
		}
		IFDEBUG(D_CCONN)
			printf("PRU_CONNECT 2: local tsuffix 0x%x so->so_head 0x%x nam:\n", 
				copcb->co_lport, so->so_head);
			dump_isoaddr( mtod(nam, struct sockaddr_iso *) );
		ENDDEBUG

		{ 	/* change the destination address so the last 2 digits
			 * are the port/suffix/selector (whatever you want to call it)
			 */
			register struct sockaddr_iso *siso =
							mtod(nam, struct sockaddr_iso *);
			if( (siso->siso_tsuffix < X25_PORT_RESERVED)  || 
				 ((siso->siso_tsuffix >= ISO_PORT_RESERVED) && 
				  (siso->siso_tsuffix <= X25_PORT_USERMAX)))
			munge( siso->siso_tsuffix, 
					siso->siso_addr.t37_idi + ADDR37_IDI_LEN, 
					1 /* nibble */);
		}
		
		soisconnecting(so);
		if (error = iso_pcbconnect(copcb, nam))
			break;
		error = cons_connect( copcb );
		if ( error ) {
			/*
			remque((struct isopcb *)copcb);
			(void) m_free(dtom(copcb));
			*/
			break;
		}
		while( (copcb->co_state != OPEN)&&(copcb->co_socket->so_error == 0) ) {
			IFDEBUG(D_CCONN)
				printf("PRU_CONNECT: error 0x%x sleeping on 0x%x\n", 
					copcb->co_socket->so_error,
					(caddr_t)&copcb->co_state );
			ENDDEBUG
			sleep( (caddr_t)&copcb->co_state, PZERO+3 );
		}

		ASSERT( copcb->co_channel != 0);

		SET_CHANMASK ( (struct isopcb *)copcb, copcb->co_channel);
		break;

	case PRU_ACCEPT: 
		/* so here is the NEW socket */
		so->so_error = 0;
		if ((so->so_state & SS_NBIO) && (so->so_state & SS_ISCONNECTED)== 0) {
			error = EWOULDBLOCK;
			break;
		}
		{
			struct sockaddr_iso *siso = mtod(nam, struct sockaddr_iso *);

			/* copy the peer's address into the return argument */
			nam->m_len = sizeof (struct sockaddr_iso);
			bcopy( (caddr_t)&copcb->co_faddr, (caddr_t)siso, 
				sizeof(struct sockaddr_iso));
		}
		break;

	case PRU_SEND:
	case PRU_SENDEOT:
		/*
		 * sosend calls this until sbspace goes negative.
		 * Sbspace may be made negative by appending this mbuf chain,
		 * possibly by a whole cluster.
		 */
		{
			/* no need to actually queue this stuff and dequeue it,
			 * just bump the pointers in so_snd so that higher
			 * layer of socket code will cause it to sleep when
			 * we've run out of socket space
			 * TODO:
			 * Unfortunately that makes sbflush vomit so we have
			 * to allocate a single real mbuf (say size 240)
			 * and sballoc it and sbfree it upon CONS_SEND_DONE.
			 * Oh, my, is this sickening or what?
			 */
			{
				struct mbuf *mx;
				
				MGET(mx, M_DONTWAIT, MT_DATA);
				mx->m_len = MLEN;
				sbappend((caddr_t)&copcb->co_socket->so_snd, mx);
			}
			if( m ) {
				IFDEBUG(D_CDATA)
					printf("X.25 Usrreq calling cons_senddata(0x%x, 0x%x)\n",
						copcb, m);
				ENDDEBUG
				error = cons_senddata(copcb, m);
			}
			IFDEBUG(D_CCONS)
				printf("PRU_SEND sent tsuffix 0x%x, m 0x%x error 0x%x\n", 
					copcb->co_lport, m, error);
			ENDDEBUG

			if( req == PRU_SENDEOT ) {
				while(copcb->co_socket->so_snd.sb_cc > 0)
					sbwait(&copcb->co_socket->so_snd);
			}
		}
		break;

	case PRU_CONTROL:
		error = cons_ioctl(so, m, (caddr_t)nam);
		break;


	case PRU_RCVD:
	case PRU_RCVOOB:
	case PRU_SENDOOB:
		/* COULD support INTERRUPT packets as oob */
	case PRU_PROTOSEND:
	case PRU_PROTORCV:
	case PRU_SENSE:
	case PRU_SLOWTIMO:
	case PRU_CONNECT2:
	case PRU_FASTTIMO:
	default:
		error = EOPNOTSUPP;
	}

	IFDEBUG(D_CCONS)
		printf("cons_usrreq cmd 0x%x copcb 0x%x returned error 0x%x\n", 
			req, copcb, error);
	ENDDEBUG
	splx(s);
	return error;
}

/*
 * NAME:	cons_input()
 * CALLED FROM:
 *  consintr() through the isosw protosw for "transport" version of X25
 * FUNCTION & ARGUMENTS:
 *  process incoming data
 */
cons_input(m, faddr, laddr, so)
	register struct mbuf *m;
	struct sockaddr_iso *faddr, *laddr; /* not used */
	register struct socket *so;
{
	IFDEBUG(D_CCONS)
		printf("cons_input( m 0x%x, so 0x%x)\n", m,so);
	ENDDEBUG
	sbappend(&so->so_rcv, m);
	sbwakeup(&so->so_rcv);
}

#ifdef notdef
/*  
 * NAME:	cons_ctloutput()
 * CALLED FROM:
 *  set/get sockopts()
 * 	Presently the protosw has 0 in the ctloutput spot
 *	 because we haven't inplemented anything yet.
 * 	If there's reason to put some options in here,
 * 	be sure to stick this routine name in the protosw in iso_proto.c
 */
cons_ctloutput(cmd, so, level, optname, mp)
	int 			cmd, level, optname;
	struct socket	*so;
	struct mbuf 	**mp;
{
	int 			s = splnet();

	splx(s);
	return EOPNOTSUPP;
}
#endif notdef


/* 
 * NAME:	cons_ctlinput()
 * CALLED FROM:
 *  lower layer when ECN_CLEAR occurs : this routine is here
 *  for consistency - cons subnet service calls its higher layer
 *  through the protosw entry.
 * FUNCTION & ARGUMENTS:
 *  cmd is a PRC_* command, list found in ../h/protosw.h
 *  copcb is the obvious.
 *  This serves the higher-layer cons service.
 * NOTE: this takes 3rd arg. because cons uses it to inform itself
 *  of things (timeouts, etc) but has a pcb instead of an address.
 */
cons_ctlinput(cmd, sa, copcb)
	int cmd;
	struct sockaddr *sa;
	register struct cons_pcb *copcb;
{
	int 			error = 0;
	int 			s = splnet();
	extern u_char 	inetctlerrmap[];
	extern int 		iso_rtchange();

	IFDEBUG(D_CCONS)
		printf("cons_ctlinput( cmd 0x%x, copcb 0x%x)\n", cmd, copcb);
	ENDDEBUG
	/* co_socket had better exist */
	switch (cmd) {
		case PRC_CONS_SEND_DONE:
			ASSERT( copcb->co_socket );
			ASSERT( copcb->co_flags & CONSF_XTS );
			sbdrop((caddr_t)&copcb->co_socket->so_snd, MLEN);
			sbwakeup((caddr_t)&copcb->co_socket->so_snd);
			break;

		case PRC_ROUTEDEAD:
			error = ENETUNREACH;
			break;

		case PRC_TIMXCEED_REASS:
			error = ETIMEDOUT;
			break;

	/*
		case PRC_QUENCH:
			iso_pcbnotify(&cons_pcb, sa,
					(int)inetctlerrmap[cmd], iso_rtchange);
			iso_pcbnotify(&tp_incoming_pending, sa,
					(int)inetctlerrmap[cmd], tpiso_quench);
			iso_pcbnotify(&tp_isopcb, sa,
					(int)inetctlerrmap[cmd], tpiso_quench);
	*/

		case PRC_IFDOWN:
			iso_pcbnotify(&cons_isopcb, sa,
					(int)inetctlerrmap[cmd], iso_rtchange);
			iso_pcbnotify(&tp_incoming_pending, sa,
					(int)inetctlerrmap[cmd], iso_rtchange);
			iso_pcbnotify(&tp_isopcb, sa,
					(int)inetctlerrmap[cmd], iso_rtchange);
			break;


		default:
			printf("cons_ctlinput: unknown cmd 0x%x\n", cmd);
	}
	if(error) {
		soisdisconnected(copcb->co_socket);
		sohasoutofband(copcb->co_socket);
	}
	splx(s);
}

/*
 *********************** SERVES ALL cons embodiments  *******************
 */

/* 
 * NAME:	cons_chan_to_pcb()
 * CALLED FROM:
 *  cons_chan_to_tpcb() in tp_cons.c
 * and in this file: incoming requests that give only a channel number, i.e.,
 *  ECN_ACCEPT, ECN_RECEIVE, ECN_CLEAR
 * FUNCTION:
 *  identify the pcb assoc with that channel
 * RETURN:
 *  ptr to the pcb
 */
struct cons_pcb *
#ifdef ARGO_DEBUG
cons_chan_to_pcb( channel, linenumber )
	int	linenumber;
#else ARGO_DEBUG
cons_chan_to_pcb( channel)
#endif ARGO_DEBUG
	register int channel;
{
	register struct cons_pcb **copcblist = (struct cons_pcb **)Xpcblist;
	register struct cons_pcb *copcb;

	/* just to be sure */
	channel = channel & 0xff;

	for( copcb = *copcblist; copcb; copcb = *(++copcblist) ) {
		copcb = (struct cons_pcb *)copcb->co_next;
		while (copcb !=  *copcblist) {
			if ( copcb->co_channel == channel ) 
				goto found; /* want to break out of both loops */

			copcb = (struct cons_pcb *)copcb->co_next;
		}
	}
found: /* or maybe not... */
	IFDEBUG(D_CCONS)
		printf("cons_chan_to_pcb( 0x%x, %d ) %s 0x%x\n", channel, linenumber,
			copcb?"FOUND":"FAILED", copcb);
	ENDDEBUG

	return copcb;
}


/*
 * NAME:	is_me()
 * CALLED FROM:
 *  cons_incoming().  Perhaps could just expand in line.
 * FUNCTION and ARGUMENTS:
 * 	for the given remote address (remadr) if it exactly matches
 *  one of the addresses of ME, and I am up as loopback, 
 *  return TRUE, else return FALSE.
 * RETURNS:
 *  Boolean
 */
Static int
is_me(remaddr)
	struct	sockaddr_iso	*remaddr;
{
	struct	ifnet 			*ifp = consif;
									/* PHASE2: this is ok */
	struct ifaddr 			*ifa = ifa_ifwithaddr(remaddr);

	IFDEBUG(D_CADDR)
		printf("is_me: withaddr returns %s\n", 
			ifa?ifa->ifa_ifp->if_name:"NONE");
	ENDDEBUG
	if( ifa ) {
		/* remaddr matches one of my interfaces exactly */
		if( ifa->ifa_ifp->if_flags & IFF_LOOPBACK ) {
			ASSERT( ifp == ifa->ifa_ifp );
			return 1;
		}
	}
	return 0;
}

find_error_reason( ecnrq )
	register struct eicon_request 	*ecnrq;
{
	extern u_char x25_error_stats[];
	int error;
	struct mbuf *cdm;
	struct e_clear_data *ecd;

	cdm = e_data(ecnrq);
	if( cdm && cdm->m_len > 0 ) {
		ecd = mtod(cdm, struct e_clear_data *);
		switch( ecd->ecd_cause ) {
			case 0x00:
			case 0x80:
				/* DTE originated; look at the diagnostic */
				error = (CONL_ERROR_MASK | ecd->ecd_diagnostic);
				goto done;

			case 0x01: /* number busy */
			case 0x81:
			case 0x09: /* Out of order */
			case 0x89:
			case 0x11: /* Remot Procedure Error */
			case 0x91:
			case 0x19: /* reverse charging accept not subscribed */
			case 0x99:
			case 0x21: /* Incampat destination */
			case 0xa1:
			case 0x29: /* fast select accept not subscribed */
			case 0xa9:
			case 0x39: /* ship absent */
			case 0xb9:
			case 0x03: /* invalid facil request */
			case 0x83:
			case 0x0b: /* access barred */
			case 0x8b:
			case 0x13: /* local procedure error */
			case 0x93:
			case 0x05: /* network congestion */
			case 0x85:
			case 0x8d: /* not obtainable */
			case 0x0d:
			case 0x95: /* RPOA out of order */
			case 0x15:
				/* take out bit 8 
				 * so we don't have to have so many perror entries 
				 */
				error = (CONL_ERROR_MASK | 0x100 | (ecd->ecd_cause & ~0x80));
				goto done;

			case 0xc1: /* gateway-detected proc error */
			case 0xc3: /* gateway congestion */

				error = (CONL_ERROR_MASK | 0x100 | ecd->ecd_cause);
				goto done;
		} 
	} 
	/* otherwise, a *hopefully* valid perror exists in the e_reason field */
	error = ecnrq->e_reason;
	if (error = 0) {
		printf("Incoming PKT TYPE 0x%x with reason 0x%x\n",
			ecnrq->e_cmd,
			ecnrq->e_reason);
		error = E_CO_HLI_DISCA;
	} 

done:
	if(error & 0x1ff == 0) {
		error = 0;
	} else if( error & 0x1ff > sizeof(x25_error_stats)) {
			ASSERT(0);
	} else {
			x25_error_stats[error& 0x1ff] ++;
	}
	return error;
}

/*
 * NAME:	consintr()
 * CALLED FROM:
 *  the eicon driver via software interrupt
 * FUNCTION and ARGUMENTS:
 *  processes incoming indications, passing them
 *  along to clnp, tp, or x.25-transport as appropriate.
 */
consintr()
{
	struct	ifnet 					*ifp = consif;
	register struct eicon_request 	*ecnrq;
	register struct cons_pcb 		*copcb = (struct cons_pcb *)0;
	register struct mbuf 			*m;
	int 							s, s0 = splnet();

	IncStat(co_intr);
	ifp->if_ipackets ++;

	for(;;) {
		/*
		 * Get next request off input queue 
		 */
		s = splimp();
		IF_DEQUEUE(&consintrq, m);
		splx(s);
		IFDEBUG(D_INCOMING)
			printf("cons intr() 0x%x m_off 0x%x m_len 0x%x dequeued\n",
				m, m?m->m_off:0, m?m->m_len:0);
		ENDDEBUG

		if (m == 0) {
			splx(s0);
			return;
		}

		if((m->m_off != MMINOFF)||(m->m_len != sizeof (struct eicon_request))){
			ifp->if_ierrors ++;
			IncStat(co_Rdrops);
			printf("Cons R DROP! BAD MBUF FROM LL 0x%x sizeof(...) 0x%x\n", 
					m, sizeof(struct eicon_request));
			continue;
		}
		
		ecnrq = mtod(m, struct eicon_request *);


		IFDEBUG(D_INCOMING)
			printf("INTR: e_cmd 0x%x, e_data 0x%x\n", ecnrq->e_cmd,
				e_data(ecnrq));
			if( e_data(ecnrq) != 0 ) {
				/* let's just look at the first few bytes */
				/*
				dump_buf( e_data(ecnrq), (e_data(ecnrq))->m_len + 12);
				*/
				dump_buf( e_data(ecnrq), 20  + 12);
			}
		ENDDEBUG
		IFTRACE(D_CDATA)
			tptrace( TPPTmisc, "INTR: req_type m lun\n", 
				ecnrq->e_cmd, m, ecnrq->e_vc, 0);
		ENDTRACE

		switch( ecnrq->e_cmd ) {

			case ECN_ACK:  /* data put on the board */
				IncStat(co_ack);
				ASSERT( ecnrq->e_vc != 0);
				/* from ACKWAIT to OPEN */
				if ( (copcb = 
#ifdef ARGO_DEBUG
					cons_chan_to_pcb( (int)ecnrq->e_vc, __LINE__ )
#else ARGO_DEBUG
					cons_chan_to_pcb( (int)ecnrq->e_vc )
#endif ARGO_DEBUG
										) == (struct cons_pcb *)0 )
					break;
				copcb->co_state = OPEN; 
				/*
				 * Anything on the pending queue for this connection?
				 */
				if( copcb->co_pending.ifq_len == 0 ) {
					if( copcb->co_proto->pr_ctlinput )
						/* for the sake of higher layer protocol (tp) */
						(*copcb->co_proto->pr_ctlinput)
							(PRC_CONS_SEND_DONE, 
							(struct sockaddr_iso *)&copcb->co_faddr, 
							(caddr_t)copcb);
				} else {
					register struct mbuf *m0;

					s = splimp();
					IF_DEQUEUE( &copcb->co_pending, m0 );
					splx(s);
					/* CAN ONLY DO 1 item here
					 * if you change this if to while, HA HA 
					 * it'll go right back onto
					 * the pending queue (which means things will
					 * be reordered on the queue!)
					 */
					if( m0 ) {
						IFDEBUG(D_CDATA)
							printf("ACK sending pending queue 0x%x len 0x%x\n",
								m0, m0->m_len);
						ENDDEBUG
						ASSERT( m0->m_len != 0);
						(void) cons_senddata(copcb, m0); 
					}
				}

				/* send more? */
				break;

			case ECN_ACCEPT:  /* call accepted at other end */
				/* adr_src, adr_dst are as given in the ECN_CALL
				 * pcb field is copied from our ECN_CALL
				 * request, confirm gives me a channel number
				 */ 
				ASSERT( ecnrq->e_vc != 0);

				IncStat(co_accept);
				if(copcb = 
#ifdef ARGO_DEBUG
				cons_chan_to_pcb((int)ecnrq->e_vc, __LINE__ )
#else ARGO_DEBUG
				cons_chan_to_pcb((int)ecnrq->e_vc)
#endif ARGO_DEBUG
												) { 
					/* error: already exists */
					printf("cons PANIC: dbl confirm for channel 0x%x\n",
						ecnrq->e_vc);
					break;
				}
				copcb = (struct cons_pcb *)ecnrq->e_pcb;
				if( copcb->co_myself != copcb ) {
					struct mbuf *mm;
					/* TODO: REMOVE */
					ASSERT(0);
					printf("BAD e_pcb from ecn (0x%x) cmd 0x%x\n", 
						ecnrq->e_pcb, ecnrq->e_cmd);
					mm = dtom( copcb );
					if(mm->m_type == MT_FREE)
						printf("FREED MBUF!\n");
					dump_buf (ecnrq, sizeof (*ecnrq));
					panic("BAD ecnrq");
					break;
				}
				touch(copcb);
				copcb->co_state = OPEN;
				copcb->co_channel = (int)ecnrq->e_vc;
				if(copcb->co_socket) {
					/* tp0 will take care of itself */
					if( copcb->co_flags & CONSF_XTS)
						soisconnected(copcb->co_socket); /* wake 'em up */
				}
				wakeup( (caddr_t)&copcb->co_state );

				/*
				 * Anything on the pending queue for this connection?
				 */
				if( copcb->co_pending.ifq_len > 0 ) {
					register struct mbuf *m0;

					s = splimp();
					IF_DEQUEUE( &copcb->co_pending, m0 );
					splx(s);
					/* CAN ONLY DO 1 item here
					 * if you change this if to while, HA HA 
					 * it'll go right back onto
					 * the pending queue (which means things will
					 * be reordered on the queue!)
					 */
					if( m0 ) {
						IFDEBUG(D_CDATA)
							printf("ACPT sending pending queue 0x%x len 0x%x\n",
								m0, m0->m_len);
						ENDDEBUG
						ASSERT( m0->m_len != 0);
						(void) cons_senddata(copcb, m0); 
					}
				}
				break;

			case ECN_REFUSE: 
				/* other end refused our connect request */
				/* src, dst are as given in the ECN_CALL */

				IncStat(co_refuse);
				copcb = (struct cons_pcb *)ecnrq->e_pcb;
				if( copcb->co_myself != copcb ) {
					struct mbuf *mm;
					/* TODO: REMOVE */
					ASSERT(0);
					printf("BAD e_pcb from ecn (0x%x) cmd 0x%x\n", 
						ecnrq->e_pcb, ecnrq->e_cmd);
					mm = dtom( copcb );
					if(mm->m_type == MT_FREE)
						printf("FREED MBUF!\n");
					dump_buf (ecnrq, sizeof (*ecnrq));
					dump_buf (copcb, sizeof (*copcb));
					panic("BAD ecnrq");
					break;
				}
				touch(copcb);
				copcb->co_state = CLOSED; /* do we have to do a clear?? */
				copcb->co_channel = X_NOCHANNEL;
				if(copcb->co_socket) {
					copcb->co_socket->so_error = ECONNREFUSED;
					/* TODO: if there's diagnostic info in the 
					 * packet, and it's more useful than this E*,
					 * get it
					 */
					soisdisconnected(copcb->co_socket); /* wake 'em up */
					IFDEBUG(D_INCOMING)
						printf("ECN_REFUSE: waking up 0x%x\n", 
							(caddr_t)&copcb->co_state );
					ENDDEBUG
					wakeup( (caddr_t)&copcb->co_state );
				}
				/*
				 * Anything on the pending queue for this connection?
				 */
				while( copcb->co_pending.ifq_len > 0 ) {
					register struct mbuf *m0;

					s = splimp();
					IF_DEQUEUE( &copcb->co_pending, m0 );
					splx(s);
					m_freem(m0);
				}
				if ( ecnrq->e_reason  == E_CO_NORESOURCES ) {
					IncStat(co_noresources);
					cons_clear_and_detach( copcb, DONTCLEAR, PRC_QUENCH );
				} else if(copcb->co_socket ) {
					copcb->co_socket->so_error = find_error_reason( ecnrq );
				}
				break;

			case ECN_CONNECT:  /* incoming call */
				/*
				 * ECN_CONNECT indication gives adc_src, adc_dst  and channel
				 */
				ASSERT( ecnrq->e_vc != 0);

				IncStat(co_connect);
				cons_incoming(ifp, ecnrq); 
				break;

			case ECN_RESET:  
			case ECN_CLEAR:
				/*
				 * ECN_CLEAR(indication) (if we can construct such a beast)
				 * gives e_vc, 
				 * Throw away anything queued pending on this connection
				 * give a reset indication to the upper layer if TP
				 * free the mbufs 
				 */
				ASSERT( ecnrq->e_vc != 0);
				if( ecnrq->e_cmd == ECN_CLEAR )
					IncStat(co_clear_in);
				else
					IncStat(co_reset_in);
#ifdef ARGO_DEBUG
				if( ! (copcb=cons_chan_to_pcb((int)ecnrq->e_vc, __LINE__)) )
#else ARGO_DEBUG
				if( ! (copcb=cons_chan_to_pcb((int)ecnrq->e_vc)) )
#endif ARGO_DEBUG

					break;
				while( copcb->co_pending.ifq_len ) {
					register struct mbuf *m0;

					s = splimp();
					IF_DEQUEUE( &copcb->co_pending, m0 );
					splx(s);
					m_freem(m0);
				}
				copcb->co_state = CLOSED; /* do we have to do a clear? */
				copcb->co_channel = X_NOCHANNEL;

				cons_clear_and_detach( copcb, DONTCLEAR, PRC_ROUTEDEAD );
				if (copcb->co_socket ) {
					copcb->co_socket->so_error = find_error_reason( ecnrq ); 
				}
				break;
				
			case ECN_RECEIVE:
				 /*
				  * ECN_RECEIVE (read) 
				  */
				ASSERT( ecnrq->e_vc != 0);
				IncStat(co_receive);
				{
					/* TODO: REMOVE */
					struct mbuf *thedata = e_data(ecnrq);
					u_int *firstint = mtod( thedata, u_int *);
					
					if( (*firstint & 0xff000000) != 0x81000000 ) {
						/* not clnp */
						switch( ((*firstint) & 0x00ff0000) >> 20 ) {
						case 0x1:
						case 0x2:
						case 0x3:
						case 0x6:
						case 0x7:
						case 0x8:
						case 0xc:
						case 0xd:
						case 0xe:
						case 0xf:
							break;
						default:
							printf(" ECN_RECEIVE! BAD DATA\n" );
							dump_buf( thedata, 20 + 12 );
							m_freem( m );
							splx(s0);
						}
					}
				}
				if ( (copcb = 
#ifdef ARGO_DEBUG
					cons_chan_to_pcb( (int)ecnrq->e_vc, __LINE__ )
#else ARGO_DEBUG
					cons_chan_to_pcb( (int)ecnrq->e_vc )
#endif ARGO_DEBUG
											) == (struct cons_pcb *)0 ) {
					ifp->if_ierrors ++;
					IFTRACE(D_CDATA)
						tptrace(TPPTmisc, "ECN_RECEIVE DROPPED chan \n",
							ecnrq->e_vc, 0, 0, 0);
					ENDTRACE
					break;
				}

				touch(copcb);
				if( ecnrq->e_info & ECN_INFO_RCVD_INT )  {
					/* interrupt packet */
						printf("consintr: interrupt pkttype : DROPPED\n");
					IncStat(co_intrpt_pkts_in);
					IncStat(co_Rdrops);
					break;
				}
				/* new way */
				if( copcb->co_proto == CLNP_proto ) 
				{
					/* IP: put it on the queue and set soft interrupt */
					struct ifqueue *ifq;
					extern struct ifqueue clnlintrq;
					register struct mbuf *ifpp; /* for ptr to ifp */
					register struct mbuf *data = e_data(ecnrq);

					total_pkts_to_clnp ++;

					/* when acting as a subnet service, have to prepend a
					 * pointer to the ifnet before handing this to clnp
					 * GAG
					 */
					if( ( data->m_off > MMINOFF + sizeof(struct snpa_hdr)) &&
						( data->m_off <= MMAXOFF )) {
						data->m_off -= sizeof(struct snpa_hdr);
						data->m_len += sizeof(struct snpa_hdr);
					} else {
						MGET(ifpp, M_DONTWAIT, MT_XHEADER);
						if( !ifpp ) {
							ifp->if_ierrors ++;
							splx(s0);
							m_freem(m); /* frees everything */
							return; 
						}
						ifpp->m_len = sizeof(struct snpa_hdr);
						ifpp->m_act = 0;
						ifpp->m_next = data;
						data = ifpp;
					}
					IFTRACE(D_CDATA)
						tptrace(TPPTmisc, "-->CLNP copcb\n", copcb, 0, 0, 0);
					ENDTRACE
					{
						/*
						 *	TODO: if we ever use esis/cons we have to
						 *	think of something reasonable to stick in the
						 *	snh_shost,snh_dhost fields. I guess
						 *	the x.121 address is what we want.
						 *
						 *	That would also require length fields in the
						 *	snpa_hdr structure.
						 */
						struct snpa_hdr 	*snh = 
							mtod(data, struct snpa_hdr *);
						bzero((caddr_t)&snh, sizeof(struct snpa_hdr));
						bcopy((caddr_t)&ifp, (caddr_t)&snh->snh_ifp, 
							sizeof(struct ifnet *));
					}
					*( mtod(data, struct ifnet **) ) = ifp; /* KLUDGE */

					ifq = &clnlintrq;
					splimp();
					if (IF_QFULL(ifq)) {
						IF_DROP(ifq);
						m_freem(m);
						IFDEBUG(D_INCOMING)
							printf("DROPPED! ecnrq 0x%x, data 0x%x\n", m,data);
						ENDDEBUG
						splx(s0);
						ifp->if_ierrors ++;
						return;
					}
					IF_ENQUEUE(ifq, data);
					IFDEBUG(D_INCOMING) 
						printf(
				"0x%x enqueued on ip Q: m_len 0x%x m_type 0x%x m_off 0x%x\n", 
							data, data->m_len, data->m_type, data->m_off);
						dump_buf(mtod(data, caddr_t), data->m_len);
					ENDDEBUG
					e_data(ecnrq) = (struct mbuf *)0;
					schednetisr(NETISR_CLNP);
				} else {
					/* HL is NOT clnp */
					IFTRACE(D_CDATA)
						tptrace(TPPTmisc,
							"-->HL pr_input so copcb channel\n", 
							copcb->co_proto->pr_input, 
							copcb->co_socket, copcb, 
							copcb->co_channel);
					ENDTRACE
					IFDEBUG(D_INCOMING) 
						printf( "0x%x --> HL proto 0x%x chan 0x%x\n", 
							e_data(ecnrq), copcb->co_proto, copcb->co_channel );
					ENDDEBUG

					(*copcb->co_proto->pr_input)(e_data(ecnrq), 
						&copcb->co_faddr,
						&copcb->co_laddr,
						copcb->co_socket, /* used by cons-transport interface */
						(copcb->co_flags & CONSF_DGM)?0:
							copcb->co_channel);/* used by tp-cons interface */

					/* 
					 * the pr_input will free the data chain, so we must
					 * zero the ptr to is so that m_free doesn't panic
					 */
					e_data(ecnrq) = (struct mbuf *)0;
				}
				break;

			default:
				/* error */
				ifp->if_ierrors ++;
				printf("consintr: unknown request\n");
		}
		IFDEBUG(D_INCOMING)
			printf("consintr: m_freem( 0x%x )\n", m);
		ENDDEBUG
		m_freem( m );
	}
	splx(s0);
}

/*
 * Process an ioctl request.
 * also set-time-limit, extend-time-limit
 * for ALL channels, the time-limit ioctls will be done by open-a-dummy-socket,
 * do ioctl with the channel number, close the socket (dumb!).
 */
/* ARGSUSED */
cons_ioctl(so, cmd, data)
	struct socket *so;
	int cmd;
	caddr_t data;
{
	int 	s = splnet();
	int 	error = 0;

	IFDEBUG(D_CCONS)
		printf("cons_ioctl( cmd 0x%x )\n", cmd);
	ENDDEBUG

#ifdef notdef
	switch (cmd) {

	default:
#endif notdef
		error = EOPNOTSUPP;
#ifdef notdef
	}
#endif notdef

	splx(s);
	return (error);
}


/*
 *************************************************************
 *                                                           *
 *                                                           *
 * Interface to CO Subnetwork service from CLNP              *
 * Must be a device interface.                             *****
 *                                                          *** 
 *                                                           *
 *                                                          Poof!
 */

/*
 * NAME:	consioctl()
 * CALLED FROM:
 * 	called through the ifnet structure.
 * FUNCTION and ARGUMENTS:
 *	the usual ioctl stuff
 * RETURNS:
 * 	E*
 * SIDE EFFECTS:
 * NOTES:
 */
consioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	register int cmd;
	register caddr_t data;
{
	register struct ifaddr 		*ifa = (struct ifaddr *)data;
	register int 				s = splimp();
	register struct ifreq 		*ifr = (struct ifreq *)data;
	register int 				error = 0;
	void						consshutdown();

	switch (cmd) {
	case SIOCSIFADDR:
		switch (ifa->ifa_addr.sa_family) {
		case AF_ISO:
			if( (ifp->if_flags & IFF_UP ) == 0)
				consinit(ifp->if_unit);
			break;
		default:
			printf("CANNOT config cons with address family %d\n",
				ifa->ifa_addr.sa_family);
			break;
		}
		break;
	case SIOCSIFFLAGS:
		IFDEBUG(D_CCONS)
			printf("consioctl: set flags to x%x\n", ifr->ifr_flags);
			printf("consioctl: ifp flags are x%x\n", ifp->if_flags);
		ENDDEBUG
		if( ifr->ifr_flags & IFF_LOOPBACK )
			ifp->if_flags |= IFF_LOOPBACK;
		else
			ifp->if_flags &= ~IFF_LOOPBACK;

		/* if board is down but request takes it up, init the board */
		if (ifr->ifr_flags & IFF_UP && (ifp->if_flags & IFF_UP) == 0)
			consinit(ifp->if_unit);

		/* if board is up but request takes it down, shut the board down */
		if (((ifr->ifr_flags & IFF_UP) == 0) && (ifp->if_flags & IFF_UP)) {
			consshutdown(ifp->if_unit);
		}
		IFDEBUG(D_CCONS)
			printf("consioctl: flags are x%x\n", ifp->if_flags);
		ENDDEBUG
		break;
	case SIOCGSTATUS:
		/* warning: must coerse ifp to (struct ifstatus *) in order to use */
		IFDEBUG(D_CCONS)
			printf("consioctl: EICON status request\n");
		ENDDEBUG
#if NECN>0
		ecnioctl(ifp, cmd, data);
#else 
		error = ENODEV;
#endif NECN>0
		break;
	default:
		error = EINVAL;
	}
	splx(s);
	return error;
}

/*
 * NAME:	consattach()
 * CALLED FROM:
 * 	cons_init() (which comes from autoconf)
 * FUNCTION and ARGUMENTS:
 *	creates an ifp and fills it in; calls ifattach() on it.	
 * RETURNS:
 *  no return value
 * SIDE EFFECTS:
 * NOTES:
 */
consattach()
{
	register struct ifnet		*ifp;
	register struct mbuf 		*m;

	if(sizeof(struct ifnet) > MLEN) {
		printf("Can't attach cons!  sizeof(struct ifnet) > MLEN\n");
		return;
	}
	MGET(m, M_DONTWAIT, MT_IFADDR);
	if( !m ) {
		printf("Can't attach cons!  NO MBUFS!\n");
		return;
	}
	m->m_len = sizeof(struct ifnet);
	ifp = consif = mtod(m, struct ifnet *); 
	ifp->if_unit = 0;
	ifp->if_name = "cons";
	ifp->if_mtu = ECN_MTU;
	ifp->if_init = consinit;
	ifp->if_ioctl = consioctl;
	ifp->if_output = cosns_output; /* called by clnp */
	ifp->if_flags = IFF_LOOPBACK;  /* default */
	if_attach(ifp); 
	printf("cons%d: pseudo device attached \n", ifp->if_unit);
}

/*
 * NAME:	consinit()
 * CALLED FROM:
 * 	consioctl()
 * FUNCTION and ARGUMENTS:
 * 	Initializes apropos data structures, etc.
 *  Marks the device as up.
 *  Zaps the address list.
 *  Calls device layer restart on the device if necessary.
 */
Static
consinit(_unit)
register int	_unit;	/* unit to initialize */
{
	struct ifnet			*ecnifp();
	struct ifnet 			*ifp;
	int		s;

	if ((ifp = ecnifp(_unit)) != (struct ifnet *)0 ) {
		ecnrestart(ifp);
		IncStat(co_restart);
	}
	if (consif->if_addrlist == (struct ifaddr *)0)
		return;
	if ((consif->if_flags & IFF_UP) == 0) {
		s = splimp();
		consif->if_flags |= IFF_UP;
		splx(s);
	}

}

/* 
 * NAME:	consshutdown()
 * CALLED FROM:
 *	cons_ioctl() when user takes down an interface w/ SIOCSIFFLAGS 
 * FUNCTION and ARGUMENTS:
 *  calls lower layer shutdown routine on the device.
 *  and marks the if as down if the if is the sw loopback pseudodevice.
 * RETURNS:
 *	no return value
 */
void
consshutdown(_unit)
register int	_unit;	/* unit to shutdown */
{
	extern	struct ifnet 	*ecnifp();
	struct ifnet 			*ifp;
	int 					s;

	if ((ifp = ecnifp(_unit)) != (struct ifnet *)0 ) {
		ecnshutdown(ifp);
	}
	if ((consif->if_flags & IFF_UP) ) {
		s = splimp();
		consif->if_flags &= ~IFF_UP;
		splx(s);
	}
}
#endif KERNEL

/*
 * NAME:	munge()
 * CALLED FROM:
 * 	cons_pcbbind(), cons_usrreq()
 * FUNCTION and ARGUMENTS:
 *  Takes the argument (value) and stashes it into the last two
 *  nibbles of an X.121 address.  Does this in the two nibbles beginning
 *  at the location defined by the character pointer (dst_octet) and the 
 *  integer (dst_nibble).  Nibble 0 is the lower nibble (high
 *  order 4 bits); nibble 1 is the low order 4 bits of *(dst_octet).
 *
 * RETURNS:
 * 	no return value
 */
Static 
munge( value, dst_octet, dst_nibble)
	int value;
	caddr_t dst_octet;
	int dst_nibble;
{
	IFDEBUG(D_CCONN)
		printf("MUNGE: value 0x%x dst_octet 0x%x, nibble 0x%x)\n",
			value, dst_octet, dst_nibble);
	ENDDEBUG
	if (value >= ISO_PORT_RESERVED)
		value -= 1000;

	{
		/* convert so it  looks like a decimal number */
		register int tens, ones;

		tens = value/10;
		ASSERT( tens <= 9 );
		ones = value - (tens * 10);

		value = tens * 16 + ones;
	}

	dst_octet --;
	/* leave nibble same 'cause it's one after the last set nibble */

	*dst_octet &= ~(0xff<<(dst_nibble << 2)); /* zero it */
	*dst_octet |= ((value>>4) << (dst_nibble<<2));
	dst_nibble = 1-dst_nibble;
	dst_octet += dst_nibble;

	*dst_octet &= ~(0xff<<(dst_nibble << 2)); /* zero it */
	*dst_octet |= ((value&0xff) << (dst_nibble<<2));
} 

/*
 * NAME:	unmunge()
 * CALLED FROM:
 *  DTEtoNSAP(), FACILtoNSAP()
 * FUNCTION and ARGUMENTS:
 *  return the port/tsuffix represented by the two digits found in a
 *  bcd string beginning at the (dst_nibble)th nibble of the
 *  octet BEFORE (dst_octet).
 *  
 * dst_octet,dst_nibble  is the nibble after the one we'll look at
 * RETURNS:
 *  an integer, the port/tsuffix
 *  Note- converts to a port > 1000 if necessary.
 */
Static int
unmunge( dst_octet, dst_nibble )
	caddr_t dst_octet;
	int dst_nibble;
{
		register u_short last = 0;

		dst_octet --;
		/* leave nibble same 'cause it's one after the last set nibble */
		IFDEBUG(D_CADDR)
			printf("unmunge: *octet 0x%x, nibble 0x%x\n", *dst_octet,
				dst_nibble);
		ENDDEBUG

		last = ((*dst_octet) & (0xff<<(dst_nibble<<2)));
		dst_nibble = 1-dst_nibble;
		dst_octet += dst_nibble;

		last |= ((*dst_octet) & (0xff<<(dst_nibble << 2)));
		{
			/* convert to a decimal number */
			register int tens, ones;

			tens = (last&0xf0)>>4;
			ones = last&0xf;

			last = tens * 10 + ones;
		}

		IFDEBUG(D_CADDR)
			printf("unmunge computes 0x%x\n", last);
		ENDDEBUG
		if((int)last+1000 >= ISO_PORT_RESERVED)
			last += 1000;
		IFDEBUG(D_CADDR)
			printf("unmunge returns 0x%x\n", last);
		ENDDEBUG
		return last;
} 

/*
 * NAME:	make_partial_x25_packet()
 *
 * FUNCTION and ARGUMENTS:
 *	Makes part of an X.25 call packet, for use by the eicon board.
 *  (src) and (dst) are the NSAP-addresses of source and destination.
 *	(proto) is the higher-layer protocol number (in iso.h)
 *	(buf) is a ptr to a buffer into which to write this partial header.
 *
 *  The partial header looks like (choke):
 *	octet		meaning
 *  1			calling DTE len  |  called DTE len (lengths in nibbles)
 *  2..n-1		called DTE addr  | (<-- boundary may be middle of an octet)
 *  			calling DTE addr  | zero nibble to round to octet boundary.
 *	n			Facility length (in octets)
 *	n+1			Facility field, which is a set of:
 *	  m			facil code
 *	  m+1		facil param len (for >2-byte facilities) in octets
 *	  m+2..p	facil param field
 *  q			user data (protocol identification octet)
 * 
 *
 * RETURNS: 
 *  0 if OK
 *  E* if failed.
 */

#ifdef X25_1984 
int cons_use_facils = 1;
#else X25_1984 
int cons_use_facils = 0;
#endif X25_1984 

int cons_use_udata = 1; /* KLUDGE FOR DEBUGGING */

Static int
make_partial_x25_packet(copcb, m)
	struct cons_pcb *copcb;
	struct mbuf *m;
{
	struct sockaddr_iso	*src, *dst;
	u_int				proto;
	int					flag;
	caddr_t 			buf = mtod(m, caddr_t);
	register caddr_t	ptr	= buf + 1;  /* make room for 2 length nibbles */
	register int		len	= 0;
	int 				buflen	=0;
	caddr_t				facil_len;
	int 				oddness	= 0;

	src = &copcb->co_laddr;
	dst = &copcb->co_faddr;
	proto = copcb->co_proto->pr_protocol, 
	flag = copcb->co_flags & CONSF_XTS;


	IFDEBUG(D_CCONN)
		printf("make_partial_x25_packet(0x%x, 0x%x, 0x%x, 0x%x, 0x%x)\n",
			src, dst, proto, m, flag);
	ENDDEBUG
	
	/*
	 * Note - order of addrs in x25 pkt hdr is wierd: 
	 * calling len/called len/called addr/calling addr (p.40 ISO 8202)
	 */
	if( (len = copcb->co_peer_dte.dtea_niblen) > 0 ) {
		nibble_copy( (char *)(copcb->co_peer_dte.dtea_addr), HIGH_NIBBLE,
			ptr, HIGH_NIBBLE, len);
	} else {
		if ((len =  NSAPtoDTE( ptr, HIGH_NIBBLE, dst)) <=0 ) {
			return E_CO_OSI_UNSAP;
		}
	}
	*buf = len; /* fill in called dte addr length */
	ptr += len>>1; /* len is in nibbles */
	oddness += len&0x1;

	if ((len =  NSAPtoDTE( ptr, 1-(len&0x1), src)) <=0 ) {
		return E_CO_OSI_UNSAP;
	}
	ptr += len>>1; /* len is in nibbles */
	*buf |= len << 4; /* fill in calling dte addr length */
	oddness += len&0x1;

	IFDEBUG(D_CADDR)
		printf("make_partial  2: ptr 0x%x, len 0x%x oddness 0x%x\n", 
			ptr, len, oddness );
	ENDDEBUG
	/* if either of the addresses were an odd length, the count is off by 1 */
	if( oddness ) {
		ptr ++;
	}

	/* ptr now points to facil length (len of whole facil field in OCTETS */
	facil_len = ptr ++;

	IFDEBUG(D_CADDR)
		printf("make_partial  calling: ptr 0x%x, len 0x%x\n", ptr, 
				src->siso_addr.isoa_len);
	ENDDEBUG
	if( cons_use_facils ) {
		*ptr = 0xcb; /* calling facility code */
		ptr ++;
		ptr ++; /* leave room for facil param len (in OCTETS + 1) */
		ptr ++; /* leave room for the facil param len (in nibbles),
				* high two bits of which indicate full/partial NSAP
				*/
		len = src->siso_addr.isoa_len;
		bcopy( &src->siso_addr.isoa_afi, ptr, len);
		*(ptr-2) = len+2; /* facil param len in octets */
		*(ptr-1) = len<<1; /* facil param len in nibbles */
		ptr += len;

		IFDEBUG(D_CADDR)
			printf("make_partial  called: ptr 0x%x, len 0x%x\n", ptr, 
					dst->siso_addr.isoa_len);
		ENDDEBUG
		*ptr = 0xc9; /* called facility code */
		ptr ++;
		ptr ++; /* leave room for facil param len (in OCTETS + 1) */
		ptr ++; /* leave room for the facil param len (in nibbles),
				* high two bits of which indicate full/partial NSAP
				*/
		len = dst->siso_addr.isoa_len;
		bcopy( &dst->siso_addr.isoa_afi, ptr, len);
		*(ptr-2) = len+2; /* facil param len = addr len + 1 for each of these
						  * two length fields, in octets */
		*(ptr-1) = len<<1; /* facil param len in nibbles */
		ptr += len;

	}
	*facil_len = ptr - facil_len - 1;
	if(*facil_len > X25_FACIL_LEN_MAX )
		return E_CO_PNA_LONG;

	if( cons_use_udata ) {
		if (copcb->co_x25crud_len > 0) {
			/*
			 *	The user specified something. Stick it in
			 */
			bcopy(copcb->co_x25crud, ptr, copcb->co_x25crud_len);
			ptr += copcb->co_x25crud_len;
		} else {
			/* protocol identifier */
			switch (proto) {
					/* unfortunately all are considered 1 protocol */
				case ISOPROTO_TP0: 
				case ISOPROTO_TP1:
				case ISOPROTO_TP2:
				case ISOPROTO_TP3:
				case ISOPROTO_TP4:
				case ISOPROTO_CLTP:
					/* no user data for TP */
					break;

				case ISOPROTO_CLNP:
					*ptr = 0x81;
					ptr++; /* count the proto id byte! */
					break;
				case ISOPROTO_INACT_NL:
					*ptr = 0x0;
					ptr++; /* count the proto id byte! */
					break;
				case ISOPROTO_X25:
					*ptr = 0xff; /* reserved for future extensions */
						  /* we're stealing this value for local use */
					ptr++; /* count the proto id byte! */
					break;
				default:
					return EPROTONOSUPPORT;
			}
		}
	}

	buflen = (int)(ptr - buf);

	IFDEBUG(D_CDUMP_REQ)
		register int i;

		printf("ECN_CONNECT DATA buf 0x%x len %d (0x%x)\n", 
			buf, buflen, buflen);
		for( i=0; i < buflen; ) {
			printf("+%d: %x %x %x %x    %x %x %x %x\n",
				i,
				*(buf+i), *(buf+i+1), *(buf+i+2), *(buf+i+3),
				*(buf+i+4), *(buf+i+5), *(buf+i+6), *(buf+i+7));
			i+=8;
		}
	ENDDEBUG
	IFDEBUG(D_CADDR)
		printf("make_partial returns buf 0x%x size 0x%x bytes\n", 
			mtod(m, caddr_t), buflen);
	ENDDEBUG

	ASSERT( X25_PARTIAL_PKT_LEN_MAX < MLEN );

	if(buflen > X25_PARTIAL_PKT_LEN_MAX)
		return E_CO_PNA_LONG;

	m->m_len = buflen;
	return  0;
}

/*
 * NAME:	NSAPtoDTE()
 * CALLED FROM:
 *  make_partial_x25_packet()
 * FUNCTION and ARGUMENTS: 
 *  get a DTE address from an NSAP-address (struct sockaddr_iso)
 *  (dst_octet) is the octet into which to begin stashing the DTE addr
 *  (dst_nibble) takes 0 or 1.  1 means begin filling in the DTE addr
 * 		in the high-order nibble of dst_octet.  0 means low-order nibble.
 *  (addr) is the NSAP-address
 *  (flag) is true if the transport suffix is to become the
 *		last two digits of the DTE address
 *  A DTE address is a series of BCD digits
 *
 *	A DTE address may have leading zeros. The are significant.
 *		1 digit per nibble, may be an odd number of nibbles.
 *
 *  An NSAP-address has the DTE address in the IDI. Leading zeros are
 *		significant. Trailing hex f indicates the end of the DTE address.
 *  	Also is a series of BCD digits, one per nibble.
 *
 * RETURNS
 *  # significant digits in the DTE address, -1 if error.
 */

Static int
NSAPtoDTE( dst_octet, dst_nibble, addr)
	caddr_t 	dst_octet;
	int			dst_nibble;
	register struct sockaddr_iso *addr;
{
	int 	error;
	u_char	x121string[7]; /* maximum is 14 digits */
	int		x121strlen;
	struct	dte_addr *dtea;

	IFDEBUG(D_CADDR)
		printf("NSAPtoDTE: nsap: %s\n", clnp_iso_addrp(&addr->siso_addr));
	ENDDEBUG

	error = iso_8208snparesolve(addr, x121string, &x121strlen);
	ASSERT(error == 0);
	if(  error != 0 ) {
		/* no snpa - cannot send */
		IFDEBUG(D_CADDR)
			printf("NSAPtoDTE: 8208resolve: %d\n", error );
		ENDDEBUG
		return 0;
	}
	ASSERT(x121strlen == sizeof(struct dte_addr));
	dtea = (struct dte_addr *)x121string;
	x121strlen = dtea->dtea_niblen;

	nibble_copy((char *)x121string, HIGH_NIBBLE, 
		dst_octet, dst_nibble, x121strlen);
	return x121strlen;
}

/*
 * NAME:	FACILtoNSAP()
 * CALLED FROM:
 *  parse_facil()
 * FUNCTION and ARGUMENTS:
 * 	Creates and NSAP in the sockaddr_iso (addr) from the
 *  x.25 facility found at (buf), of length (buf_len).
 * RETURNS:
 *  0 if ok, non-zero if error;
 */

Static int
FACILtoNSAP( buf, buf_len, addr)
	caddr_t 		buf;
	u_char			buf_len; /* in bytes */
	register struct sockaddr_iso *addr;
{
	int len_in_nibbles;

	IFDEBUG(D_CADDR)
		printf("FACILtoNSAP( 0x%x, 0x%x, 0x%x )\n", 
			buf, buf_len, addr );
	ENDDEBUG

	len_in_nibbles = *buf;
	/* despite the fact that X.25 makes us put a length in nibbles
	 * here, the NSAP-addrs are always in full octets
	 */
	buf ++;

	bzero( addr, sizeof (struct sockaddr_iso) );

	ASSERT(buf_len <= 1+sizeof (struct iso_addr));
	if(buf_len > 1+sizeof (struct iso_addr)) {
		return -1; /* error */
	}
	ASSERT(len_in_nibbles == (buf_len - 1)<<1);
	if(len_in_nibbles != (buf_len - 1)<<1) {
		return -2; /* error */
	}
	bcopy(buf, &addr->siso_addr.isoa_afi, buf_len-1);
	addr->siso_addr.isoa_len = buf_len-1; 
	IFDEBUG(D_CADDR)
		printf("FACILtoNSAP: isoa_len 0x%x\n",
			addr->siso_addr.isoa_len);
	ENDDEBUG
	addr->siso_family = AF_ISO;

	addr->siso_tsuffix = 
		unmunge( ((caddr_t)&addr->siso_addr.t37_idi) + ADDR37_IDI_LEN , 1 );
	return 0;
}

/*
 * NAME:	DTEtoNSAP()
 * CALLED FROM:
 *  parse_facil()
 * FUNCTION and ARGUMENTS:
 *  Creates a type 37 NSAP in the sockaddr_iso (addr)
 * 	from a DTE address found at the (src_nibble)th nibble of
 * 	the octet (src_octet), of length (src_nib_len).
 *  
 * RETURNS:
 *  0 if ok; E* otherwise.
 */

Static  int
DTEtoNSAP(addr, src_octet, src_nibble, src_nib_len)
	struct sockaddr_iso *addr;
	caddr_t src_octet;
	int src_nibble, src_nib_len;
{
	caddr_t				dst_octet;
	int					pad_len;
	int					dst_nibble;
	char				first_nib;
	static				char *z_pad = "\0\0\0\0\0\0\0";
	static				char *f_pad = "\021\021\021\021\021\021\021";

	IFDEBUG(D_CADDR)
		printf("DTEtoNSAP( 0x%x, 0x%x, 0x%x, 0x%x )\n", 
			src_octet, src_nibble, src_nib_len, addr );
	ENDDEBUG

	bzero( addr, sizeof(*addr));
	addr->siso_family = AF_ISO;
	/*
	 * Coming from a DTE addr it's always type 37.
	 * src_octet <-- starting place in the NSAP-address of 
	 * the embedded SNPA-address (x.121 addr or DTE addr).
	 */
	addr->siso_addr.isoa_afi = 0x37;

	/* first, figure out what pad to use and pad */

	first_nib = (*src_octet) >> (SHIFT*(1-src_nibble));
	pad_len = (ADDR37_IDI_LEN<<1 - src_nib_len);
	nibble_copy(first_nib? z_pad : f_pad, HIGH_NIBBLE,
		(caddr_t) addr->siso_addr.t37_idi, HIGH_NIBBLE, pad_len);

	dst_octet += (pad_len>>1);
	dst_nibble = 1-(pad_len & 0x1);
	IFDEBUG(D_CADDR)
		printf("DTEtoNSAP 2( 0x%x, 0x%x, 0x%x, 0x%x )\n", 
			dst_octet, dst_nibble, pad_len, src_nib_len );
	ENDDEBUG

	/* now copy the dte address */
	nibble_copy( src_octet, src_nibble, dst_octet, dst_nibble, src_nib_len);

	addr->siso_addr.isoa_len = ADDR37_IDI_LEN + ADDR37_DSP_LEN +1 /* for afi */;
		/* kludge */

	addr->siso_tsuffix = unmunge(
		(caddr_t) &(addr->siso_addr.t37_idi[ADDR37_IDI_LEN]), HIGH_NIBBLE);

	IFDEBUG(D_CADDR)
		printf("DTEtoNSAP 3 returning 0 tsuffix 0x%x\n", addr->siso_tsuffix);
	ENDDEBUG

	return 0; /* ok */
}

/*
 * FUNCTION and ARGUMENTS:
 *	parses (buf_len) bytes beginning at (buf) and finds
 *  a called nsap, a calling nsap, and protocol identifier.
 * RETURNS:
 *  0 if ok, E* otherwise.
 */

Static int
parse_facil( buf, buf_len, called, calling, proto, peer_dte)
	caddr_t 		buf;
	u_char			buf_len; /* in bytes */
	register struct sockaddr_iso *called, *calling;
	int				*proto;
	struct	dte_addr	*peer_dte;
{
	register int 	i;
	caddr_t			ptr;
	caddr_t 		facil_len;
	int 			facil_param_len;
	struct 	sockaddr_iso *addr;
	int				addrs_not_parsed = (int)0xcb + (int)0xc9;

	IFDEBUG(D_CADDR)
		printf("parse_facil( 0x%x, 0x%x, 0x%x, 0x%x, 0x%x )\n", 
			buf, buf_len, called, calling, *proto);
		dump_buf(buf, buf_len);
	ENDDEBUG

	/* find the beginnings of the facility fields in buf 
	 * by skipping over the called & calling DTE addresses
	 * i <- # nibbles in called + # nibbles in calling
	 * i += 1 so that an odd nibble gets rounded up to even  
	 * before dividing by 2, then divide by two to get # octets
	 */
	i = (int)(*buf >> 4) + (int)(*buf&0xf);
	i++;
	ptr = (caddr_t) (buf + (i>>1));
	/* now i is number of octets */

	ptr ++; /* plus one for the DTE lengths byte */

	/* ptr now is at facil_length field */
	facil_len = ptr++;
	IFDEBUG(D_CADDR)
		printf("parse_facils: facil length is  0x%x\n", (int) *facil_len);
	ENDDEBUG

	while( ptr <= (caddr_t)(facil_len + (int)*facil_len) ) {
		/* get NSAP addresses from facilities */
		switch (*ptr) {
			case 0xcb:
				facil_param_len = 0;
				addr = calling;
				addrs_not_parsed -= 0xcb;
				break;
			case 0xc9:
				facil_param_len = 0;
				addr = called;
				addrs_not_parsed -= 0xc9;
				break;

				/* from here to default are legit cases that I ignore */

				/* variable length */
			case 0xca:  /* end-to-end transit delay negot */
			case 0xc6:  /* network user id */
			case 0xc5: 	/* charging info : indicating monetary unit */
			case 0xc2: 	/* charging info : indicating segment count */
			case 0xc1: 	/* charging info : indicating call duration */
			case 0xc4: 	/* RPOA extended format */
			case 0xc3: 	/* call redirection notification */
				facil_param_len = 0;
				addr = (struct sockaddr_iso *)0;
				break;

				/* 1 octet */
			case 0x0a:  /* min. throughput class negot */
			case 0x02:  /* throughput class */
			case 0x03:  case 0x47:  /* CUG shit */
			case 0x0b:  /* expedited data negot */
			case 0x01:  /* Fast select or reverse charging 
						(example of intelligent protocol design) */
			case 0x04: 	/* charging info : requesting service */
			case 0x08: 	/* called line addr modified notification */
				facil_param_len = 1;
				addr = (struct sockaddr_iso *)0;
				break;

				/* any 2 octets */
			case 0x42:  /* pkt size */
			case 0x43:  /* win size */
			case 0x44:  /* RPOA basic format */
			case 0x41:  /* bilateral CUG shit */
			case 0x49: 	/* transit delay selection and indication */
				facil_param_len = 2;
				addr = (struct sockaddr_iso *)0;
				break;

				/* don't have any 3 octets */
				/*
				facil_param_len = 3;
				*/
			default:
				ASSERT(0);
				printf(
"BOGUS FACILITY CODE facil_len 0x%x *facil_len 0x%x, ptr 0x%x *ptr 0x%x\n",
					facil_len, *facil_len,
					ptr, *ptr);
				addr = (struct sockaddr_iso *)0;
				/* facil that we don't handle */
				return E_CO_HLI_REJI;
		}
		ptr++; /* one for facil code */
		if(facil_param_len == 0) /* variable length */ 
			facil_param_len = (int)*ptr; /* 1 + the real facil param */
		if( addr &&  FACILtoNSAP(ptr+1, facil_param_len-1, addr) ) {
			return E_CO_OSI_UNSAP;
		}
		ptr += facil_param_len;
	}
	if( addrs_not_parsed ) {
		/* no facilities, get NSAP addresses from DTE addresses */
		register int ed, ing;

		ed = (int)(*buf&0xf);
		if( ed == 0 ) {
			panic("Called DTE address absent");
		}
		DTEtoNSAP(called, (buf + 1)/*octet*/, 
			1/*nibble*/, ed);

		ing = (int)(*buf >> 4); 
		if( ing == 0 ) {
			printf("cons: panic: Calling DTE address absent");
			return E_CO_HLI_REJI;
		}
		nibble_copy((buf + (ed>>1)+1)/*octet*/, 1-(ed&0x1)/*nibble*/, 
			peer_dte->dtea_addr, HIGH_NIBBLE, ing);
		DTEtoNSAP(calling, (buf + (ed>>1)+1)/*octet*/, 
			1-(ed&0x1)/*nibble*/, ing);

	}

	ASSERT( ptr == (caddr_t)(facil_len + 1 + (int)*facil_len) );

	/* 
	 * now look for user data to find protocol identifier
	 */
	if( ptr == buf + buf_len ) {
		/* no user data */
		*proto = ISOPROTO_TP; /* to proto id --> use TP */
		IFDEBUG(D_CADDR)
			printf("NO USER DATA: use TP\n");
		ENDDEBUG
	} else {
		ASSERT ( ptr < buf + buf_len );
		if ( ptr >= buf + buf_len ) {
			printf("ptr 0x%x buf 0x%x buf_len 0x%x buf+buf_len 0x%x\n",
				ptr, buf, buf_len, buf+buf_len);
		}
		IFDEBUG(D_CADDR)
			printf("proto byte 0x%x, value 0x%x\n", ptr, *ptr);
		ENDDEBUG
		switch(*ptr) {
		case 0x81:
			*proto = ISOPROTO_CLNP;
			break;
		case 0x0:
			*proto = ISOPROTO_INACT_NL;
			break;
		case  'e': /* for EAN */
			*proto = ISOPROTO_TP; 
			/* can check for "an2" or can ignore the rest of the u data */
			break;
		case 0xff: /* reserved for future extensions */
			*proto =  ISOPROTO_X25;
			break;
		case 0x82: /* 9542 not implemented */
		case 0x84: /* 8878/A SNDCP not implemented */
		default:
			*proto =  -1; 
			return E_CO_HLI_PROTOID;
		}
	}
	return 0;
}

#endif NARGOXTWENTYFIVE > 0
