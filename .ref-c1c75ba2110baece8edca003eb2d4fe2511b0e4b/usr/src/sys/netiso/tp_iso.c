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
 * $Header: tp_iso.c,v 5.3 88/11/18 17:27:57 nhall Exp $
 * $Source: /usr/argo/sys/netiso/RCS/tp_iso.c,v $
 *
 * Here is where you find the iso-dependent code.  We've tried
 * keep all net-level and (primarily) address-family-dependent stuff
 * out of the tp source, and everthing here is reached indirectly
 * through a switch table (struct nl_protosw *) tpcb->tp_nlproto 
 * (see tp_pcb.c). 
 * The routines here are:
 * 		iso_getsufx: gets transport suffix out of an isopcb structure.
 * 		iso_putsufx: put transport suffix into an isopcb structure.
 *		iso_putnetaddr: put a whole net addr into an isopcb.
 *		iso_getnetaddr: get a whole net addr from an isopcb.
 *		iso_recycle_suffix: clear suffix for reuse in isopcb
 * 		tpclnp_ctlinput: handle ER CNLPdu : icmp-like stuff
 * 		tpclnp_mtu: figure out what size tpdu to use
 *		tpclnp_input: take a pkt from clnp, strip off its clnp header, 
 *				give to tp
 *		tpclnp_output_dg: package a pkt for clnp given 2 addresses & some data
 *		tpclnp_output: package a pkt for clnp given an isopcb & some data
 */

#ifndef lint
static char *rcsid = "$Header: tp_iso.c,v 5.3 88/11/18 17:27:57 nhall Exp $";
#endif lint

#ifdef ISO

#include "../h/types.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/domain.h"
#include "../h/mbuf.h"
#include "../h/errno.h"
#include "../h/time.h"
#include "../net/if.h"
#include "../net/route.h"
#include "../h/protosw.h"

#include "../netiso/tp_param.h"
#include "../netiso/argo_debug.h"
#include "../netiso/tp_stat.h"
#include "../netiso/tp_pcb.h"
#include "../netiso/tp_trace.h"
#include "../netiso/tp_stat.h"
#include "../netiso/tp_tpdu.h"
#include "../netiso/tp_clnp.h"

/*
 * CALLED FROM:
 * 	pr_usrreq() on PRU_BIND, PRU_CONNECT, PRU_ACCEPT, and PRU_PEERADDR
 * FUNCTION, ARGUMENTS, and RETURN VALUE:
 * 	Return a transport suffix from an isopcb structure (inp).
 *  (CAST TO AN INT)
 * 	The argument (which) takes the value TP_LOCAL or TP_FOREIGN.
 */

short
iso_getsufx(isop,  which)
	struct isopcb *isop;
	int which;
{
	switch (which) {
	case TP_LOCAL:
		return  htons(isop->isop_laddr.siso_tsuffix);

	case TP_FOREIGN:
		return  htons(isop->isop_faddr.siso_tsuffix);
	}
}

/* CALLED FROM:
 * 	tp_newsocket(); i.e., when a connection is being established by an
 * 	incoming CR_TPDU.
 *
 * FUNCTION, ARGUMENTS:
 * 	Put a transport suffix (found in name) into an isopcb structure (isop).
 * 	The argument (which) takes the value TP_LOCAL or TP_FOREIGN.
 */
void
iso_putsufx(isop, name, which)
	struct isopcb *isop;
	struct sockaddr_iso *name;
	int which;
{
	switch (which) {
	case TP_LOCAL:
		isop->isop_lport = ntohs(name->siso_tsuffix);
		break;
	case TP_FOREIGN:
		isop->isop_fport = ntohs(name->siso_tsuffix);
		break;
	}
}

/*
 * CALLED FROM:
 * 	tp.trans whenever we go into REFWAIT state.
 * FUNCTION and ARGUMENT:
 *	 Called when a ref is frozen, to allow the suffix to be reused. 
 * 	(isop) is the net level pcb.  This really shouldn't have to be
 * 	done in a NET level pcb but... for the internet world that just
 * 	the way it is done in BSD...
 * 	The alternative is to have the port unusable until the reference
 * 	timer goes off.
 */
void
iso_recycle_tsuffix(isop)
	struct isopcb	*isop;
{
	isop->isop_laddr.siso_tsuffix = isop->isop_faddr.siso_tsuffix = 0;
}

/*
 * CALLED FROM:
 * 	tp_newsocket(); i.e., when a connection is being established by an
 * 	incoming CR_TPDU.
 *
 * FUNCTION and ARGUMENTS:
 * 	Copy a whole net addr from a struct sockaddr (name).
 * 	into an isopcb (isop).
 * 	The argument (which) takes values TP_LOCAL or TP_FOREIGN
 */ 
void
iso_putnetaddr(isop, name, which)
	register struct isopcb	*isop;
	struct sockaddr_iso	*name;
	int which;
{
	switch (which) {
	case TP_LOCAL:
		isop->isop_laddr.siso_family = AF_ISO;
		bcopy((caddr_t)&name->siso_addr,
			(caddr_t)&isop->isop_laddr.siso_addr, sizeof(struct iso_addr));
		IFDEBUG(D_TPISO)
			printf("PUT TP_LOCAL addr\n");
			dump_isoaddr(&isop->isop_laddr);
		ENDDEBUG
		break;
	case TP_FOREIGN:
		isop->isop_faddr.siso_family = AF_ISO;
		if( name != (struct sockaddr_iso *)0 ) {
			bcopy((caddr_t)&name->siso_addr, 
				(caddr_t)&isop->isop_faddr.siso_addr, sizeof(struct iso_addr));
		}
		IFDEBUG(D_TPISO)
			printf("PUT TP_FOREIGN addr\n");
			dump_isoaddr(&isop->isop_faddr);
		ENDDEBUG
	}
}

/*
 * CALLED FROM:
 *  pr_usrreq() PRU_SOCKADDR, PRU_ACCEPT, PRU_PEERADDR
 * FUNCTION and ARGUMENTS:
 * 	Copy a whole net addr from an isopcb (isop) into
 * 	a struct sockaddr (name).
 * 	The argument (which) takes values TP_LOCAL or TP_FOREIGN.
 */ 

void
iso_getnetaddr( isop, name, which)
	struct isopcb *isop;
	struct sockaddr_iso *name;
	int which;
{
	switch (which) {
	case TP_LOCAL:
		bcopy( (caddr_t)&isop->isop_laddr.siso_addr, 
			(caddr_t)&name->siso_addr, sizeof(struct iso_addr));
		break;

	case TP_FOREIGN:
		bcopy( (caddr_t)&isop->isop_faddr.siso_addr, 
			(caddr_t)&name->siso_addr, sizeof(struct iso_addr));
		break;
	}
}

/*
 * CALLED FROM:
 *  tp_input() on incoming CR, CC, and pr_usrreq() for PRU_CONNECT
 * FUNCTION, ARGUMENTS, SIDE EFFECTS and RETURN VALUE:
 * Determine the proper maximum transmission unit, i.e., MTU, to use, given
 * a) the header size for the network protocol and the max transmission
 *	  unit on the subnet interface, determined from the information in (isop),
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
 */

void
tpclnp_mtu(so, isop, size, negot )
	struct socket *so;
	struct isopcb *isop;
	int *size;
	u_char *negot;
{
	struct ifnet *ifp;
	register int i;
	int windowsize = so->so_rcv.sb_hiwat;
	int clnp_size;
	int sizeismtu = 0;

	struct ifnet	*iso_routeifp();

	IFDEBUG(D_CONN)
		printf("tpclnp_mtu(0x%x,0x%x,0x%x,0x%x)\n", so, isop, size, negot);
	ENDDEBUG
	IFTRACE(D_CONN)
		tptrace(TPPTmisc, "ENTER GET MTU: size negot \n",*size, *negot, 0, 0);
	ENDTRACE

	*size = 1 << *negot;

	if( *size > windowsize ) {
		*size = windowsize;
	}

	if ((ifp = iso_routeifp(&isop->isop_faddr)) == (struct ifnet *)0)
		return;

	/* TODO - make this indirect off the socket structure to the
	 * network layer to get headersize
	 */
	clnp_size = clnp_hdrsize(isop->isop_laddr.siso_addr.isoa_len);

	if(*size > ifp->if_mtu - clnp_size) {
		*size = ifp->if_mtu - clnp_size;
		sizeismtu = 1;
	}
	IFTRACE(D_CONN)
		tptrace(TPPTmisc, "GET MTU MID: tpcb size negot i \n",
		*size, *negot, i, 0);
	ENDTRACE

	/* have to transform size to the log2 of size */
	for(i=TP_MIN_TPDUSIZE; (i<TP_MAX_TPDUSIZE && ((1<<i) <= *size)) ; i++)
		;
	i--;

	/* are we on the same LAN? if so, negotiate one tpdu size larger,
	 * and actually send the real mtu size
	 */
	/* PHASE2: replace with iso_on_localnet(&isop->isop_faddr);
	 * or something along those lines
	 */
	if ( iso_netmatch(&isop->isop_laddr, &isop->isop_faddr) && sizeismtu ) {
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
 * CALLED FROM:
 *  tp_emit()
 * FUNCTION and ARGUMENTS:
 *  Take a packet(m0) from tp and package it so that clnp will accept it.
 *  This means prepending space for the clnp header and filling in a few
 *  of the fields.
 *  inp is the isopcb structure; datalen is the length of the data in the
 *  mbuf string m0.
 * RETURN VALUE:
 *  whatever (E*) is returned form the net layer output routine.
 */

int
tpclnp_output(isop, m0, datalen, nochksum)
	struct isopcb		*isop;
	struct mbuf 		*m0;
	int 				datalen;
	int					nochksum;
{
	IncStat(ts_tpdu_sent);

	IFDEBUG(D_TPISO)
		struct tpdu *hdr = mtod(m0, struct tpdu *);

		printf(
"abt to call clnp_output: datalen 0x%x, hdr.li 0x%x, hdr.dutype 0x%x nocsum x%x dst addr:\n",
			datalen,
			(int)hdr->tpdu_li, (int)hdr->tpdu_type, nochksum);
		dump_isoaddr(&isop->isop_faddr);
		printf("\nsrc addr:\n");
		dump_isoaddr(&isop->isop_laddr);
		dump_mbuf(m0, "at tpclnp_output");
	ENDDEBUG

	return 
		clnp_output(m0, isop, datalen, nochksum?CLNP_NO_CKSUM:0 /* flags */);
}

/*
 * CALLED FROM:
 *  tp_error_emit()
 * FUNCTION and ARGUMENTS:
 *  This is a copy of tpclnp_output that takes the addresses
 *  instead of a pcb.  It's used by the tp_error_emit, when we
 *  don't have an iso_pcb with which to call the normal output rtn.
 * RETURN VALUE:
 *  ENOBUFS or
 *  whatever (E*) is returned form the net layer output routine.
 */

int
tpclnp_output_dg(laddr, faddr, m0, datalen, ro, nochksum)
	struct iso_addr		*laddr, *faddr;
	struct mbuf 		*m0;
	int 				datalen;
	struct route 		*ro;
	int					nochksum;
{
	struct isopcb		tmppcb;
	struct iso_addr		*isoa;
	int					err;
	int					flags;

	IFDEBUG(D_TPISO)
		printf("tpclnp_output_dg  datalen 0x%x m0 0x%x\n", datalen, m0);
	ENDDEBUG

	/*
	 *	Fill in minimal portion of isopcb so that clnp can send the
	 *	packet.
	 */
	bzero((caddr_t)&tmppcb, sizeof(tmppcb));
	isoa = &(tmppcb.isop_laddr.siso_addr);
	bcopy((caddr_t)laddr, (caddr_t)isoa, sizeof (struct iso_addr));
	isoa = &(tmppcb.isop_faddr.siso_addr);
	bcopy((caddr_t)faddr, (caddr_t)isoa, sizeof (struct iso_addr));

	IFDEBUG(D_TPISO)
		printf("tpclnp_output_dg  faddr: \n");
		dump_isoaddr(&tmppcb.isop_faddr);
		printf("\ntpclnp_output_dg  laddr: \n");
		dump_isoaddr(&tmppcb.isop_laddr);
		printf("\n");
	ENDDEBUG

	/*
	 *	Do not use packet cache since this is a one shot error packet
	 */
	flags = (CLNP_NOCACHE|(nochksum?CLNP_NO_CKSUM:0));

	IncStat(ts_tpdu_sent);

	err = clnp_output(m0, &tmppcb, datalen, flags);
	
	/*
	 *	Free route allocated by clnp (if the route was indeed allocated)
	 */
	if (tmppcb.isop_route.ro_rt)
		RTFREE(tmppcb.isop_route.ro_rt);
	
	return(err);
}

/*
 * CALLED FROM:
 * 	clnp's input routine, indirectly through the protosw.
 * FUNCTION and ARGUMENTS:
 * Take a packet (m) from clnp, strip off the clnp header and give it to tp
 * No return value.  
 */
ProtoHook
tpclnp_input(m, faddr, laddr, clnp_len)
	struct mbuf *m;
	struct iso_addr *faddr, *laddr;
	int clnp_len;
{
	struct sockaddr_iso src, dst;
	int s = splnet();

	IncStat(ts_pkt_rcvd);

	IFDEBUG(D_TPINPUT)
		printf("tpclnp_input: m 0x%x clnp_len 0x%x\n", m, clnp_len);
		dump_mbuf(m, "at tpclnp_input");
	ENDDEBUG
	/*
	 * CLNP gives us an mbuf chain WITH the clnp header pulled up,
	 * and the length of the clnp header.
	 * First, strip off the Clnp header. leave the mbuf there for the
	 * pullup that follows.
	 */

	m->m_len -= clnp_len;
	m->m_off += clnp_len;

	m = (struct mbuf *)tp_inputprep(m);

	IFDEBUG(D_TPINPUT)
		dump_mbuf(m, "after tpclnp_input both pullups");
	ENDDEBUG

	src.siso_family = dst.siso_family = AF_ISO;
	bcopy(faddr, &src.siso_addr, sizeof(struct iso_addr));
	bcopy(laddr, &dst.siso_addr, sizeof(struct iso_addr));

	IFDEBUG(D_TPISO)
		printf("calling tp_input: &src 0x%x  &dst 0x%x, src addr:\n", 
			&src, &dst);
		printf(" dst addr:\n");
		dump_isoaddr(&src);
		dump_isoaddr(&dst);
	ENDDEBUG

	(void) tp_input(m, &src, &dst, 0, tpclnp_output_dg);

	IFDEBUG(D_QUENCH)
		{ 
			if(time.tv_usec & 0x4 && time.tv_usec & 0x40) {
				printf("tpclnp_input: FAKING %s\n", 
					tp_stat.ts_pkt_rcvd & 0x1?"QUENCH":"QUENCH2");
				if(tp_stat.ts_pkt_rcvd & 0x1) {
					tpclnp_ctlinput(PRC_QUENCH, &src);
				} else {
					tpclnp_ctlinput(PRC_QUENCH2, &src);
				}
			}
		}
	ENDDEBUG

	splx(s);
	return 0;

discard:
	IFDEBUG(D_TPINPUT)
		printf("tpclnp_input DISCARD\n");
	ENDDEBUG
	IFTRACE(D_TPINPUT)
		tptrace(TPPTmisc, "tpclnp_input DISCARD m",  m,0,0,0);
	ENDTRACE
	m_freem(m);
	IncStat(ts_recv_drop);
	splx(s);

	return 0;
}

ProtoHook
iso_rtchange()
{
	return 0;
}

/*
 * CALLED FROM:
 *  tpclnp_ctlinput()
 * FUNCTION and ARGUMENTS:
 *  find the tpcb pointer and pass it to tp_quench
 */
void
tpiso_decbit(isop)
	struct isopcb *isop;
{
	tp_quench( isop->isop_socket->so_tpcb, PRC_QUENCH2 );
}
/*
 * CALLED FROM:
 *  tpclnp_ctlinput()
 * FUNCTION and ARGUMENTS:
 *  find the tpcb pointer and pass it to tp_quench
 */
void
tpiso_quench(isop)
	struct isopcb *isop;
{
	tp_quench( isop->isop_socket->so_tpcb, PRC_QUENCH );
}

/*
 * CALLED FROM:
 *  The network layer through the protosw table.
 * FUNCTION and ARGUMENTS:
 *	When clnp an ICMP-like msg this gets called.
 *	It either returns an error status to the user or
 *	it causes all connections on this address to be aborted
 *	by calling the appropriate xx_notify() routine.
 *	(cmd) is the type of ICMP error.   
 * 	(siso) is the address of the guy who sent the ER CLNPDU
 */
ProtoHook
tpclnp_ctlinput(cmd, siso)
	int cmd;
	struct sockaddr_iso *siso;
{
	extern u_char inetctlerrmap[];
	extern ProtoHook tpiso_abort();
	extern ProtoHook iso_rtchange();
	extern ProtoHook tpiso_reset();

	IFDEBUG(D_TPINPUT)
		printf("tpclnp_ctlinput: cmd 0x%x addr: ", cmd);
		dump_isoaddr(siso);
		printf("\n");
	ENDDEBUG

	if (cmd < 0 || cmd > PRC_NCMDS)
		return 0;
	switch (cmd) {

		case	PRC_QUENCH2:
			iso_pcbnotify(&tp_isopcb, &siso->siso_addr, 0, tpiso_decbit);
			break;

		case	PRC_QUENCH:
			iso_pcbnotify(&tp_isopcb, &siso->siso_addr, 0, tpiso_quench);
			break;

		case	PRC_TIMXCEED_REASS:
		case	PRC_ROUTEDEAD:
			iso_pcbnotify(&tp_isopcb, &siso->siso_addr, 0, tpiso_reset);
			break;

		case	PRC_HOSTUNREACH:
		case	PRC_UNREACH_NET:
		case	PRC_IFDOWN:
		case	PRC_HOSTDEAD:
			iso_pcbnotify(&tp_isopcb, &siso->siso_addr,
					(int)inetctlerrmap[cmd], iso_rtchange);
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
		case	PRC_PARAMPROB:
		*/
		iso_pcbnotify(&tp_isopcb, &siso->siso_addr, 
			(int)inetctlerrmap[cmd], tpiso_abort);
		break;
	}
	return 0;
}

/*
 * These next 2 routines are
 * CALLED FROM:
 *	xxx_notify() from tp_ctlinput() when
 *  net level gets some ICMP-equiv. type event.
 * FUNCTION and ARGUMENTS:
 *  Cause the connection to be aborted with some sort of error
 *  reason indicating that the network layer caused the abort.
 *  Fakes an ER TPDU so we can go through the driver.
 *  abort always aborts the TP connection.
 *  reset may or may not, depending on the TP class that's in use.
 */
ProtoHook
tpiso_abort(isop)
	struct isopcb *isop;
{
	struct tp_event e;

	IFDEBUG(D_CONN)
		printf("tpiso_abort 0x%x\n", isop);
	ENDDEBUG
	e.ev_number = ER_TPDU;
	e.ATTR(ER_TPDU).e_reason = ECONNABORTED;
	return  tp_driver((struct tp_pcb *)isop->isop_socket->so_tpcb, &e);
}

ProtoHook
tpiso_reset(isop)
	struct isopcb *isop;
{
	struct tp_event e;

	e.ev_number = T_NETRESET;
	return tp_driver((struct tp_pcb *)isop->isop_socket->so_tpcb, &e);

}

#endif ISO
