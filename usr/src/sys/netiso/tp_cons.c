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
 * $Header: tp_cons.c,v 5.6 88/11/18 17:27:13 nhall Exp $
 * $Source: /usr/argo/sys/netiso/RCS/tp_cons.c,v $
 *	@(#)tp_cons.c	7.3 (Berkeley) %G% *
 *
 * Here is where you find the iso-dependent code.  We've tried
 * keep all net-level and (primarily) address-family-dependent stuff
 * out of the tp source, and everthing here is reached indirectly
 * through a switch table (struct nl_protosw *) tpcb->tp_nlproto 
 * (see tp_pcb.c). 
 * The routines here are:
 *		tpcons_mtu: figure out what size tpdu to use
 *		tpcons_input: pullup and call tp_input w/ correct arguments
 *		tpcons_output_dg: package a pkt for cons given 2 addresses & some data
 *		tpcons_output: package a pkt for cons given an isopcb & some data
 *		cons_chan_to_tpcb: find a tpcb based on the channel #
 */

#ifndef lint
static char *rcsid = "$Header: tp_cons.c,v 5.6 88/11/18 17:27:13 nhall Exp $";
#endif lint

#include "argoxtwentyfive.h"

#ifdef ISO
#if NARGOXTWENTYFIVE > 0

#include "param.h"
#include "socket.h"
#include "domain.h"
#include "mbuf.h"
#include "errno.h"
#include "time.h"
#include "../net/if.h"

#include "tp_param.h"
#include "argo_debug.h"
#include "tp_stat.h"
#include "tp_pcb.h"
#include "tp_trace.h"
#include "tp_stat.h"
#include "tp_tpdu.h"
#include "../net/route.h"
#include "iso.h"
#include "iso_pcb.h"
#include "cons.h"
#include "tp_seq.h"

int tpcons_output();

/*
 * CALLED FROM:
 *  tp_input() on incoming CR, CC, and pr_usrreq() for PRU_CONNECT
 * FUNCTION, ARGUMENTS, SIDE EFFECTS and RETURN VALUE:
 *  version of the previous procedure for X.25
 */

void
tpcons_mtu(so, isop, size, negot)
	struct socket *so;
	struct isopcb *isop;
	int *size;
	u_char *negot;
{
	register struct ifnet *ifp;
	register int i=0;
	int windowsize = so->so_rcv.sb_hiwat;
	struct ifnet	*iso_routeifp();

	IFTRACE(D_CONN)
		tptrace(TPPTmisc, "ENTER GET MTU: size negot ",*size, *negot, 0, 0);
	ENDTRACE


	*size = 1 << *negot;
	if ((ifp = iso_routeifp(&isop->isop_faddr)) == (struct ifnet *)0)
		return;

	if( *size > windowsize ) {
		*size = windowsize;
		i++;
	}

	if(*size > ifp->if_mtu) {
		*size = ifp->if_mtu ;
		i++;
	}
	if(i) {
		/* size was changed by this routine - have to transform it to
		 * the log2 of size
		 */
		for(i=TP_MIN_TPDUSIZE; (i<TP_MAX_TPDUSIZE && ((1<<i)<*size)) ; i++)
			;
		/* are we on the same LAN? if so, negotiate one tpdu size larger,
		 * and actually send the real mtu size
		 */
		/* PHASE2: replace with iso_on_localnet(&isop->isop_faddr);
		 */
		if ( !iso_netmatch(&isop->isop_laddr, &isop->isop_faddr) ) {
			i--;
			*size = 1<<i;
		}
		*negot = i;
	}

	IFDEBUG(D_CONN)
		printf("GET MTU RETURNS: ifp %s size 0x%x negot 0x%x\n",
		ifp->if_name,	*size, *negot);
	ENDDEBUG
	IFTRACE(D_CONN)
		tptrace(TPPTmisc, "EXIT GET MTU: tpcb size negot ",
		*size, *negot, 0, 0);
	ENDTRACE
}

/*
 * CALLED FROM:
 * 	cons
 * FUNCTION and ARGUMENTS:
 * THIS MAYBE BELONGS IN SOME OTHER PLACE??? but i think not -
 */
ProtoHook
tpcons_ctlinput(cmd, siso, isop)
	int cmd; 
	struct sockaddr_iso *siso;
	struct isopcb *isop;
{
	switch (cmd) {

	case PRC_CONS_SEND_DONE:
		if( isop->isop_socket ) { /* tp 0 only */
			register struct tp_pcb *tpcb = 
				(struct tp_pcb *)isop->isop_socket->so_tpcb;
			struct 	tp_event 		E;
			int 					error = 0;

			if( tpcb->tp_class == TP_CLASS_0 ) {
				/* only if class is exactly class zero, not
				 * still in class negotiation
				 */
				/* fake an ack */
				register SeqNum	seq =  SEQ_ADD(tpcb, tpcb->tp_snduna, 1);

				IFTRACE(D_DATA)
					tptrace(TPPTmisc, "FAKE ACK seq cdt 1", 
						seq, 0,0,0);
				ENDTRACE
				IFDEBUG(D_DATA)
					printf("FAKE ACK seq 0x%x cdt 1\n", seq );
				ENDDEBUG
				E.ATTR(AK_TPDU).e_cdt = 1;
				E.ATTR(AK_TPDU).e_seq = seq;
				E.ATTR(AK_TPDU).e_subseq = 0;
				E.ATTR(AK_TPDU).e_fcc_present = 0;
				error =  DoEvent(AK_TPDU);
				if( error ) {
					tpcb->tp_sock->so_error = error;
				}
			} /* else ignore it */
		} 
		break;
	case PRC_ROUTEDEAD:
		if( isop->isop_socket ) { /* tp 0 only */
			tpiso_reset(isop);
			break;
		} /* else drop through */
	default:
		(void) tpclnp_ctlinput(cmd, siso);
		break;
	}
	return 0;
}

/*
 * CALLED FROM:
 * 	cons's intr routine
 * FUNCTION and ARGUMENTS:
 * Take a packet (m) from cons, pullup m as required by tp,
 *  ignore the socket argument, and call tp_input. 
 * No return value.  
 */
ProtoHook
tpcons_input(m, faddr, laddr, so, channel)
	struct mbuf 		*m;
	struct sockaddr_iso	*faddr, *laddr;
	struct socket 		*so; /* not used */
	int					channel;
{
	if( m == MNULL)
		return 0;

	m = (struct mbuf *)tp_inputprep(m);

	IFDEBUG(D_TPINPUT)
		printf("tpcons_input before tp_input(m 0x%x)\n", m);
		dump_buf( m, 12+ m->m_len);
	ENDDEBUG
	tp_input(m, faddr, laddr, channel, tpcons_output);
	return 0;
}


/*
 * CALLED FROM:
 *  tp_emit()
 * FUNCTION and ARGUMENTS:
 *  Take a packet(m0) from tp and package it so that cons will accept it.
 *  This means filling in a few of the fields.
 *  inp is the isopcb structure; datalen is the length of the data in the
 *  mbuf string m0.
 * RETURN VALUE:
 *  whatever (E*) is returned form the net layer output routine.
 */

int
tpcons_output(isop, m0, datalen, nochksum)
	struct isopcb		*isop;
	struct mbuf 		*m0;
	int 				datalen;
	int					nochksum;
{
	struct tp_pcb 		*tpcb;
	int					error;

	IFDEBUG(D_EMIT)
		printf(
		"tpcons_output(isop 0x%x, m 0x%x, len 0x%x socket 0x%x\n",
			isop, m0, datalen, isop->isop_socket);
	ENDDEBUG
	if(m0 == MNULL)
		return 0;
	ASSERT(m0->m_len > 0);
	tpcb = (struct tp_pcb *)isop->isop_socket->so_tpcb;

	/* check is for class EQUAL to 4: if still in negotiation stage, 
	 * cannot send as dgm
	 */
	error = cons_output(isop, m0,  datalen, (tpcb->tp_class == TP_CLASS_4));
	IncStat(ts_tpdu_sent);

	IFTRACE(D_EMIT)
		tptrace( TPPTmisc, 
		"tpcons_output( isop  m isdgm cons_output returns", 
			isop, m0, (tpcb->tp_class == TP_CLASS_4), error );
	ENDTRACE
	return error;
}

/*
 * CALLED FROM:
 *  tp_error_emit()
 * FUNCTION and ARGUMENTS:
 *  This is a copy of tpcons_output that takes the addresses
 *  instead of a pcb.  It's used by the tp_error_emit, when we
 *  don't have an iso_pcb with which to call the normal output rtn.
 * RETURN VALUE:
 *  ENOBUFS or
 *  whatever (E*) is returned form the net layer output routine.
 */

int
tpcons_output_dg(laddr, faddr, m0, datalen, ro, nochksum)
	struct iso_addr		*laddr, *faddr;
	struct mbuf 		*m0;
	int 				datalen;
	struct route 		*ro;
	int					nochksum;
{
	IFDEBUG(D_TPISO)
		printf("PANIC: tpcons_output_dg  datalen 0x%x m0 0x%x\n", datalen, m0);
	ENDDEBUG

	return 0;
}

struct tp_pcb *
cons_chan_to_tpcb(chan)
	int chan;
{
	extern struct isopcb *cons_chan_to_pcb ();
#ifdef ARGO_DEBUG
	struct isopcb *isop = cons_chan_to_pcb (chan, -1);
#else ARGO_DEBUG
	struct isopcb *isop = cons_chan_to_pcb (chan);
#endif ARGO_DEBUG

	IFTRACE(D_CONN)
		tptrace(TPPTmisc, "vc->tpcb(chan) socket",
			chan, isop->isop_socket, 0, 0);
	ENDTRACE
	IFDEBUG(D_CONN)
		printf("vc->tpcb(0x%x) socket 0x%x, *ISOP dump:",
			chan, isop->isop_socket);
		dump_buf( isop, 32);
	ENDDEBUG
	if( isop->isop_socket == (struct socket *)0 )
		return (struct tp_pcb *) 0;
	else {
		return (struct tp_pcb *)(isop->isop_socket->so_tpcb);
}
}
#endif NARGOXTWENTYFIVE
#endif ISO
