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
 * $Header: /var/src/sys/netiso/RCS/tp_iso.c,v 5.1 89/02/09 16:20:51 hagens Exp $
 * $Source: /var/src/sys/netiso/RCS/tp_iso.c,v $
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
static char *rcsid = "$Header: /var/src/sys/netiso/RCS/tp_iso.c,v 5.1 89/02/09 16:20:51 hagens Exp $";
#endif lint

#ifdef ISO

#include "param.h"
#include "socket.h"
#include "socketvar.h"
#include "domain.h"
#include "malloc.h"
#include "mbuf.h"
#include "errno.h"
#include "time.h"
#include "protosw.h"

#include "../net/if.h"
#include "../net/route.h"

#include "argo_debug.h"
#include "tp_param.h"
#include "tp_stat.h"
#include "tp_pcb.h"
#include "tp_trace.h"
#include "tp_stat.h"
#include "tp_tpdu.h"
#include "tp_clnp.h"

/*
 * CALLED FROM:
 * 	pr_usrreq() on PRU_BIND, PRU_CONNECT, PRU_ACCEPT, and PRU_PEERADDR
 * FUNCTION, ARGUMENTS:
 * 	The argument (which) takes the value TP_LOCAL or TP_FOREIGN.
 */

iso_getsufx(isop, lenp, data_out, which)
	struct isopcb *isop;
	u_short *lenp;
	caddr_t data_out;
	int which;
{
	register struct sockaddr_iso *addr = 0;

	switch (which) {
	case TP_LOCAL:
		addr = isop->isop_laddr;
		break;

	case TP_FOREIGN:
		addr = isop->isop_faddr;
	}
	if (addr)
		bcopy(TSEL(addr), data_out, (*lenp = addr->siso_tsuffixlen));
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
iso_putsufx(isop, sufxloc, sufxlen, which)
	struct isopcb *isop;
	caddr_t sufxloc;
	int sufxlen, which;
{
	struct sockaddr_iso **dst, *backup;
	register struct sockaddr_iso *addr;
	struct mbuf *m;
	int len;

	switch (which) {
	default:
		return;

	case TP_LOCAL:
		dst = &isop->isop_laddr;
		backup = &isop->isop_sladdr;
		break;

	case TP_FOREIGN:
		dst = &isop->isop_faddr;
		backup = &isop->isop_sfaddr;
	}
	if ((addr = *dst) == 0) {
		addr = *dst = backup;
		addr->siso_nlen = 0;
		addr->siso_ssuffixlen = 0;
		printf("iso_putsufx on un-initialized isopcb\n");
	}
	len = sufxlen + addr->siso_nlen +
			(sizeof(struct sockaddr_iso) - sizeof(struct iso_addr));
	if (addr == backup) {
		if (len > sizeof(isop->isop_sladdr)) {
				m = m_getclr(M_DONTWAIT, MT_SONAME);
				if (m == 0)
					return;
				addr = *dst = mtod(m, struct sockaddr_iso *);
				*addr = *backup;
				m->m_len = len;
		}
	} else
		dtom(addr)->m_len = len;
	bcopy(sufxloc, TSEL(addr), sufxlen);
	addr->siso_tsuffixlen = sufxlen;
	addr->siso_len = len;
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
	isop->isop_laddr->siso_tsuffixlen = isop->isop_faddr->siso_tsuffixlen = 0;
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
	struct sockaddr_iso **sisop, *backup;
	register struct sockaddr_iso *siso;

	switch (which) {
	case TP_LOCAL:
		sisop = &isop->isop_laddr;
		backup = &isop->isop_sladdr;
		break;
	case TP_FOREIGN:
		sisop = &isop->isop_faddr;
		backup = &isop->isop_sfaddr;
	}
	siso = ((*sisop == 0) ? (*sisop = backup) : *sisop);
	IFDEBUG(D_TPISO)
		printf("ISO_PUTNETADDR\n");
		dump_isoaddr(isop->isop_faddr);
	ENDDEBUG
	siso->siso_addr = name->siso_addr;
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
	struct mbuf *name;
	int which;
{
	struct sockaddr_iso *siso =
		(which == TP_LOCAL ? isop->isop_laddr : isop->isop_faddr);
	if (siso)
		bcopy((caddr_t)siso, mtod(name, caddr_t),
				(unsigned)(name->m_len = siso->siso_len));
	else
		name->m_len = 0;
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
	struct iso_ifaddr *ia;
	register int i;
	int windowsize = so->so_rcv.sb_hiwat;
	int clnp_size;
	int sizeismtu = 0;

	struct iso_ifaddr	*iso_routeifa();

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

	if  (((ia = iso_routeifa(isop->isop_faddr)) == 0)
	      || (ifp = ia->ia_ifp) == 0)
		return;

	/* TODO - make this indirect off the socket structure to the
	 * network layer to get headersize
	 */
	if (isop->isop_laddr)
		clnp_size = clnp_hdrsize(isop->isop_laddr->siso_addr.isoa_len);
	else
		clnp_size = 20;

	if(*size > ifp->if_mtu - clnp_size) {
		*size = ifp->if_mtu - clnp_size;
		sizeismtu = 1;
	}
	/* have to transform size to the log2 of size */
	for(i=TP_MIN_TPDUSIZE; (i<TP_MAX_TPDUSIZE && ((1<<i) <= *size)) ; i++)
		;
	i--;

	IFTRACE(D_CONN)
		tptrace(TPPTmisc, "GET MTU MID: tpcb size negot i \n",
		*size, *negot, i, 0);
	ENDTRACE

	/* are we on the same LAN? if so, negotiate one tpdu size larger,
	 * and actually send the real mtu size
	 */
	if (iso_localifa(isop->isop_faddr)) {
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
	register struct mbuf *m = m0;
	IncStat(ts_tpdu_sent);

	IFDEBUG(D_TPISO)
		struct tpdu *hdr = mtod(m0, struct tpdu *);

		printf(
"abt to call clnp_output: datalen 0x%x, hdr.li 0x%x, hdr.dutype 0x%x nocsum x%x dst addr:\n",
			datalen,
			(int)hdr->tpdu_li, (int)hdr->tpdu_type, nochksum);
		dump_isoaddr(isop->isop_faddr);
		printf("\nsrc addr:\n");
		dump_isoaddr(isop->isop_laddr);
		dump_mbuf(m0, "at tpclnp_output");
	ENDDEBUG
	if ((m->m_flags & M_PKTHDR) == 0) {
		IFDEBUG(D_TPISO)
		printf("tpclnp_output: non headered mbuf");
		ENDDEBUG
		MGETHDR(m, M_DONTWAIT, MT_DATA);
		if (m == 0) {
			m_freem(m0);
			return ENOBUFS;
		}
		m->m_next = m0;
		m->m_len = 0;
		m->m_pkthdr.len = datalen;
		m0 = m;
	}

	return 
		clnp_output(m0, isop, /* flags */nochksum ? CLNP_NO_CKSUM : 0);
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
	int					err;
	int					flags;
	register struct mbuf *m = m0;

	IFDEBUG(D_TPISO)
		printf("tpclnp_output_dg  datalen 0x%x m0 0x%x\n", datalen, m0);
	ENDDEBUG

	/*
	 *	Fill in minimal portion of isopcb so that clnp can send the
	 *	packet.
	 */
	bzero((caddr_t)&tmppcb, sizeof(tmppcb));
	tmppcb.isop_laddr = &tmppcb.isop_sladdr;
	tmppcb.isop_laddr->siso_addr = *laddr;
	tmppcb.isop_faddr = &tmppcb.isop_sfaddr;
	tmppcb.isop_faddr->siso_addr = *faddr;

	IFDEBUG(D_TPISO)
		printf("tpclnp_output_dg  faddr: \n");
		dump_isoaddr(&tmppcb.isop_sfaddr);
		printf("\ntpclnp_output_dg  laddr: \n");
		dump_isoaddr(&tmppcb.isop_sladdr);
		printf("\n");
	ENDDEBUG

	/*
	 *	Do not use packet cache since this is a one shot error packet
	 */
	flags = (CLNP_NOCACHE|(nochksum?CLNP_NO_CKSUM:0));

	IncStat(ts_tpdu_sent);

	if ((m->m_flags & M_PKTHDR) == 0) {
		printf("tpclnp_output: non headered mbuf");
		MGETHDR(m, M_DONTWAIT, MT_DATA);
		if (m == 0) {
			m_freem(m0);
			return ENOBUFS;
		}
		m->m_next = m0;
		m->m_len = 0;
		m->m_pkthdr.len = datalen;
		m0 = m;
	}
	err = clnp_output(m0, &tmppcb, flags);
	
	/*
	 *	Free route allocated by clnp (if the route was indeed allocated)
	 */
	if (tmppcb.isop_route.ro_rt)
		RTFREE(tmppcb.isop_route.ro_rt);
	
	return(err);
}
extern struct sockaddr_iso blank_siso;
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
	struct mbuf *tp_inputprep();

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
	m->m_data += clnp_len;

	m = tp_inputprep(m);

	IFDEBUG(D_TPINPUT)
		dump_mbuf(m, "after tpclnp_input both pullups");
	ENDDEBUG

	src = blank_siso; dst = blank_siso;
	bcopy((caddr_t)faddr, (caddr_t)&src.siso_addr, 1 + faddr->isoa_len);
	bcopy((caddr_t)laddr, (caddr_t)&dst.siso_addr, 1 + laddr->isoa_len);

	IFDEBUG(D_TPISO)
		printf("calling tp_input: &src 0x%x  &dst 0x%x, src addr:\n", 
			&src, &dst);
		printf(" dst addr:\n");
		dump_isoaddr(&src);
		dump_isoaddr(&dst);
	ENDDEBUG

	(void) tp_input(m, (struct sockaddr *)&src, (struct sockaddr *)&dst,
				0, tpclnp_output_dg);

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
	tp_quench((struct tp_pcb *)isop->isop_socket->so_tpcb, PRC_QUENCH2);
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
	tp_quench((struct tp_pcb *)isop->isop_socket->so_tpcb, PRC_QUENCH);
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
	return tpclnp_ctlinput1(cmd, &siso->siso_addr);
}

/*
 *	Entry to ctlinput with argument of an iso_addr rather than a sockaddr
 */
ProtoHook
tpclnp_ctlinput1(cmd, isoa)
	int cmd;
	struct iso_addr	*isoa;
{
	extern u_char inetctlerrmap[];
	extern ProtoHook tpiso_abort();
	extern ProtoHook iso_rtchange();
	extern ProtoHook tpiso_reset();
	void iso_pcbnotify();

	IFDEBUG(D_TPINPUT)
		printf("tpclnp_ctlinput1: cmd 0x%x addr: %s\n", cmd, 
			clnp_iso_addrp(isoa));
	ENDDEBUG

	if (cmd < 0 || cmd > PRC_NCMDS)
		return 0;
	switch (cmd) {

		case	PRC_QUENCH2:
			iso_pcbnotify(&tp_isopcb, isoa, 0, (int (*)())tpiso_decbit);
			break;

		case	PRC_QUENCH:
			iso_pcbnotify(&tp_isopcb, isoa, 0, (int (*)())tpiso_quench);
			break;

		case	PRC_TIMXCEED_REASS:
		case	PRC_ROUTEDEAD:
			iso_pcbnotify(&tp_isopcb, isoa, 0, tpiso_reset);
			break;

		case	PRC_HOSTUNREACH:
		case	PRC_UNREACH_NET:
		case	PRC_IFDOWN:
		case	PRC_HOSTDEAD:
			iso_pcbnotify(&tp_isopcb, isoa,
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
		iso_pcbnotify(&tp_isopcb, isoa, (int)inetctlerrmap[cmd], tpiso_abort);
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
