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
 *
 * $Header: tp_usrreq.c,v 5.4 88/11/18 17:29:18 nhall Exp $
 * $Source: /usr/argo/sys/netiso/RCS/tp_usrreq.c,v $
 *
 * tp_usrreq(), the fellow that gets called from most of the socket code.
 * Pretty straighforward.
 * THe only really awful stuff here is the OOB processing, which is done
 * wholly here.
 * tp_rcvoob() and tp_sendoob() are contained here and called by tp_usrreq().
 */

#ifndef lint
static char *rcsid = "$Header: tp_usrreq.c,v 5.4 88/11/18 17:29:18 nhall Exp $";
#endif lint

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "mbuf.h"
#include "socket.h"
#include "socketvar.h"
#include "domain.h"
#include "protosw.h"
#include "errno.h"

#include "../netiso/tp_param.h"
#include "../netiso/tp_timer.h"
#include "../netiso/tp_stat.h"
#include "../netiso/tp_seq.h"
#include "../netiso/tp_ip.h"
#include "../netiso/tp_pcb.h"
#include "../netiso/argo_debug.h"
#include "../netiso/tp_trace.h"
#include "../netiso/tp_meas.h"
#include "../netiso/iso.h"
#include "../netiso/iso_errno.h"

int tp_attach(), tp_driver();

#ifdef ARGO_DEBUG
/*
 * CALLED FROM:
 *  anywhere you want to debug...
 * FUNCTION and ARGUMENTS:
 *  print (str) followed by the control info in the mbufs of an mbuf chain (n)
 */
void
dump_mbuf(n, str)
	struct mbuf *n;
	char *str;
{
	struct mbuf *nextrecord;

	printf("dump %s\n", str);

	if( n == MNULL)  {
		printf("EMPTY:\n");
		return;
	}
		
	for(;n;) {
		nextrecord = n->m_act;
		printf("RECORD:\n");
		while (n) {
			printf("%x : Len %x Of %x A %x Nx %x Tp %x\n",
				n, n->m_len, n->m_off, n->m_act, n->m_next, n->m_type);
#ifdef notdef
			{
				register char *p = mtod(n, char *);
				register int i;

				printf("data: ");
				for(i=0; i < n->m_len; i++ ) {
					if(i%8 == 0)
						printf("\n");
					printf("0x%x ", *(p+i));
				}
				printf("\n");
			}
#endif notdef
			if( n->m_next == n ) {
				printf("LOOP!\n");
				return;
			}
			n = n->m_next;
		}
		n = nextrecord;
	}
	printf("\n");
}

#endif ARGO_DEBUG

/*
 * CALLED FROM:
 *  tp_usrreq(), PRU_RCVOOB
 * FUNCTION and ARGUMENTS:
 * 	Copy data from the expedited data socket buffer into
 * 	the pre-allocated mbuf m.
 * 	There is an isomorphism between XPD TPDUs and expedited data TSDUs.
 * 	XPD tpdus are limited to 16 bytes of data so they fit in one mbuf.
 * RETURN VALUE:
 *  EINVAL if debugging is on and a disaster has occurred
 *  ENOTCONN if the socket isn't connected
 *  EWOULDBLOCK if the socket is in non-blocking mode and there's no
 *		xpd data in the buffer
 *  E* whatever is returned from the fsm.
 */
static int 
tp_rcvoob(tpcb, so, m, outflags, inflags)
	struct tp_pcb	*tpcb;
	register struct socket	*so;
	register struct mbuf 	*m;
	int 		 	*outflags;
	int 		 	inflags;
{
	register struct mbuf *n;
	register struct sockbuf *sb = &tpcb->tp_Xrcv;
	struct tp_event E;
	int error = 0;

	IFDEBUG(D_XPD)
		printf("PRU_RCVOOB, sostate 0x%x\n", so->so_state);
	ENDDEBUG

	/* if you use soreceive */
	if (m==MNULL)
		return ENOBUFS;

restart:
	sblock(sb);

	if ((((so->so_state & SS_ISCONNECTED) == 0)
		 || (so->so_state & SS_ISDISCONNECTING) != 0) &&
		(so->so_proto->pr_flags & PR_CONNREQUIRED)) {
			return ENOTCONN;
	}

	if ( sb->sb_cc == 0) {
		ASSERT( (tpcb->tp_flags & TPF_DISC_DATA_IN)  == 0 );
		IFDEBUG(D_XPD)
			printf("RCVOOB: empty queue!\n");
		ENDDEBUG
		if (so->so_state & SS_NBIO) {
			return  EWOULDBLOCK;
		}
		sbunlock(sb);
		sbwait(sb);
		goto restart;
	}
	/* Take the first mbuf off the chain.
	 * Each XPD TPDU gives you a complete TSDU so the chains don't get 
	 * coalesced, but one TSDU may span several mbufs.
	 * Nevertheless, since n should have a most 16 bytes, it
	 * will fit into m.  (size was checked in tp_input() )
	 */

	n = sb->sb_mb;
	ASSERT((n->m_type == TPMT_DATA) ||
		n->m_type == TPMT_IPHDR || n->m_type == TPMT_TPHDR);

	/* 
	 * mtod doesn't work for cluster-type mbufs, hence this disaster check: 
	 */
	if( n->m_off > MMAXOFF )
		panic("tp_rcvoob: unexpected cluster ");

	m->m_next = MNULL;
	m->m_act = MNULL;
	m->m_off = MMINOFF;
	m->m_len = 0;

	/* Assuming at most one xpd tpdu is in the buffer at once */
	while ( n != MNULL ) {
		m->m_len += n->m_len;
		bcopy(mtod(n, caddr_t), mtod(m, caddr_t), n->m_len);
		m->m_off += n->m_len; /* so mtod() in bcopy() above gives right addr */
		n = n->m_next;
	}
	m->m_off = MMINOFF; /* restore it to its proper value */

	IFDEBUG(D_XPD)
		printf("tp_rcvoob: xpdlen 0x%x\n", m->m_len);
		dump_mbuf(so->so_rcv.sb_mb, "RCVOOB: Rcv socketbuf");
		dump_mbuf(sb->sb_mb, "RCVOOB: Xrcv socketbuf");
	ENDDEBUG

	if( (inflags & MSG_PEEK) == 0 )
		sbdrop(sb, m->m_len);

release:
	sbunlock(sb);

	IFTRACE(D_XPD)
		tptraceTPCB(TPPTmisc, "PRU_RCVOOB @ release sb_cc m_len",
			tpcb->tp_Xrcv.sb_cc, m->m_len,0,0 );
	ENDTRACE
	if(outflags)
		*outflags = MSG_OOB | MSG_EOTSDU | inflags; /* always on xpd recv */
	if (error == 0)
		error = DoEvent(T_USR_Xrcvd); 
	return error;
}

/*
 * CALLED FROM:
 *  tp_usrreq(), PRU_SENDOOB
 * FUNCTION and ARGUMENTS:
 * 	Send what's in the mbuf chain (m) as an XPD TPDU.
 * 	The mbuf may not contain more then 16 bytes of data.
 * 	XPD TSDUs aren't segmented, so they translate into
 * 	exactly one XPD TPDU, with EOT bit set.
 * RETURN VALUE:
 *  EWOULDBLOCK if socket is in non-blocking mode and the previous
 *   xpd data haven't been acked yet.
 *  EMSGSIZE if trying to send > max-xpd bytes (16)
 *  ENOBUFS if ran out of mbufs
 */
static int
tp_sendoob(tpcb, so, xdata, outflags)
	struct tp_pcb	*tpcb;
	register struct socket	*so;
	register struct mbuf 	*xdata;
	int 		 	*outflags; /* not used */
{
	/*
	 * Each mbuf chain represents a sequence # in the XPD seq space.
	 * The first one in the queue has sequence # tp_Xuna.
	 * When we add to the XPD queue, we stuff a zero-length
	 * mbuf (mark) into the DATA queue, with its sequence number in m_next
	 * to be assigned to this XPD tpdu, so data xfer can stop
	 * when it reaches the zero-length mbuf if this XPD TPDU hasn't
	 * yet been acknowledged.  
	 */
	register struct sockbuf *sb = &(tpcb->tp_Xsnd);
	register struct mbuf 	*xmark;
	register int 			len=0;
	struct tp_event E;

	IFDEBUG(D_XPD)
		printf("tp_sendoob:");
		if(xdata)
			printf("xdata len 0x%x\n", xdata->m_len);
	ENDDEBUG
oob_again:
	/* DO NOT LOCK the Xsnd buffer!!!! You can have at MOST one 
	 * socket buf locked at any time!!! (otherwise you might
	 * sleep() in sblock() w/ a signal pending and cause the
	 * system call to be aborted w/ a locked socketbuf, which
	 * is a problem.  So the so_snd buffer lock
	 * (done in sosend()) serves as the lock for Xpd.
	 */
	if (sb->sb_mb) { /* anything already in this sockbuf? */
		if (so->so_state & SS_NBIO) {
			return EWOULDBLOCK;
		}
		sbunlock(&so->so_snd);
		sbwait(&so->so_snd);
		sblock(&so->so_snd);
		goto oob_again;
	}

	if (xdata == (struct mbuf *)0) {
		/* empty xpd packet */
		MGET(xdata, M_WAIT, TPMT_DATA);
		if (xdata == NULL) {
			return ENOBUFS;
		}
		xdata->m_len = 0;
		xdata->m_act = MNULL;
	}
	IFDEBUG(D_XPD)
		printf("tp_sendoob 1:");
		if(xdata)
			printf("xdata len 0x%x\n", xdata->m_len);
	ENDDEBUG
	xmark = xdata; /* temporary use of variable xmark */
	while (xmark) {
		len += xmark->m_len;
		xmark = xmark->m_next;
	}
	if (len > TP_MAX_XPD_DATA) {
		return EMSGSIZE;
	}
	IFDEBUG(D_XPD)
		printf("tp_sendoob 2:");
		if(xdata)
			printf("xdata len 0x%x\n", len);
	ENDDEBUG

	/* stick an xpd mark in the normal data queue
	 * make sure we have enough mbufs before we stick anything into any queues
	 */
	MGET(xmark,M_WAIT, TPMT_XPD);
	if (xmark == MNULL) {
		return ENOBUFS;
	}
	xmark->m_len = 0;
	xmark->m_act = MNULL;
	
	{	/* stash the xpd sequence number in the mark */ 
		SeqNum *Xuna = mtod(xmark, SeqNum *);
		*Xuna = tpcb->tp_Xuna;
	}

	IFTRACE(D_XPD)
		tptraceTPCB(TPPTmisc, "XPD mark m_next ", xmark->m_next, 0, 0, 0);
	ENDTRACE

	sbappendrecord(&so->so_snd, xmark); /* the mark */
	sbappendrecord(sb, xdata);	

	IFDEBUG(D_XPD)
		printf("tp_sendoob len 0x%x\n", len);
		dump_mbuf(so->so_snd.sb_mb, "XPD request Regular sndbuf:");
		dump_mbuf(tpcb->tp_Xsnd.sb_mb, "XPD request Xsndbuf:");
	ENDDEBUG
	u.u_r.r_val1  += len; 
	return DoEvent(T_XPD_req); 

}

/*
 * CALLED FROM:
 *  the socket routines
 * FUNCTION and ARGUMENTS:
 * 	Handles all "user requests" except the [gs]ockopts() requests.
 * 	The argument (req) is the request type (PRU*), 
 * 	(m) is an mbuf chain, generally used for send and
 * 	receive type requests only.
 * 	(nam) is used for addresses usually, in particular for the bind request.
 * 
 * 	The last argument (rights in most usrreq()s) has been stolen for 
 * 	returning flags values.  Since rights can't be passed around w/ tp,
 * 	this field is used only for RCVOOB user requests, and is assumed
 * 	to be either 0 (as soreceive() would have it) or a ptr to the int flags
 * 	(as recvv()'s version of soreceive() would have it
 */
/*ARGSUSED*/
ProtoHook
tp_usrreq(so, req, m, nam, rightsp, outflags)
	struct socket *so;
	u_int req;
	struct mbuf *m, *nam, *rightsp /* not used */;
	int *outflags; 
{	
	register struct tp_pcb *tpcb =  sototpcb(so);
	int s = splnet();
	int error = 0;
	u_long eotsdu = 0;
	struct tp_event E;

	IFDEBUG(D_REQUEST)
		printf("usrreq(0x%x,%d,0x%x,0x%x,0x%x)\n",so,req,m,nam,outflags);
		if(so->so_error)
			printf("WARNING!!! so->so_error is 0x%x\n", so->so_error);
	ENDDEBUG
	IFTRACE(D_REQUEST)
		tptraceTPCB(TPPTusrreq, "req so m state [", req, so, m, 
			tpcb?tpcb->tp_state:0);
	ENDTRACE

	if ((u_int)tpcb == 0 && req != PRU_ATTACH) {
		IFTRACE(D_REQUEST)
			tptraceTPCB(TPPTusrreq, "req failed NO TPCB[", 0, 0, 0, 0);
		ENDTRACE
		splx(s);
		return ENOTCONN;
	}


	IFDEBUG(D_XPD)
		extern struct mbuf *mfree;
		struct mbuf *m = mfree, *n=MNULL;

		if ( (u_int) tpcb != 0 )  {
			n = tpcb->tp_Xrcv.sb_mb;
			if(n) while(m) {
				if(m == n) {
				printf("enter usrreq %d Xrcv sb_mb 0x%x is on freelist!\n",
					req, n);
				}
				m = m->m_next;
			}
		}
	ENDDEBUG

	switch (req) {

	case PRU_ATTACH:
		if (tpcb) {
			error = EISCONN;
			break;
		}
		if( error = tp_attach(so, so->so_proto->pr_domain->dom_family ) )
			break;
		tpcb = sototpcb(so);
		break;

	case PRU_ABORT: 	/* called from close() */
		/* called for each incoming connect queued on the 
		 *	parent (accepting) socket 
		 */
		if( tpcb->tp_state == TP_OPEN ) {
			E.ATTR(T_DISC_req).e_reason = E_TP_NO_SESSION;
			error = DoEvent(T_DISC_req); /* pretend it was a close() */
			break;
		} /* else DROP THROUGH */

	case PRU_DETACH: 	/* called from close() */
		/* called only after disconnect was called */
		error = DoEvent(T_DETACH);
		break;

	case PRU_SHUTDOWN:
		/* recv end may have been released; local credit might be zero  */
	case PRU_DISCONNECT:
		E.ATTR(T_DISC_req).e_reason = E_TP_NORMAL_DISC;
		error = DoEvent(T_DISC_req);
		break;

	case PRU_BIND:
		error =  (tpcb->tp_nlproto->nlp_pcbbind)( so->so_pcb, nam );
		if (error == 0) {
			tpcb->tp_lsuffixlen = sizeof(short); /* default */ 
			*(u_short *)(tpcb->tp_lsuffix) = (u_short) 
				(tpcb->tp_nlproto->nlp_getsufx)( so->so_pcb, TP_LOCAL );
		}
		break;

	case PRU_LISTEN:
		if ( *SHORT_LSUFXP(tpcb) == (short)0 ) {
			/* note: this suffix is independent of the extended suffix */
			if( error = (tpcb->tp_nlproto->nlp_pcbbind)(so->so_pcb, MNULL) )
				break;
		}
		if( tpcb->tp_lsuffixlen ==  0) {
			tpcb->tp_lsuffixlen = sizeof(short); /* default */ 
			*SHORT_LSUFXP(tpcb)  = (short) 
				(tpcb->tp_nlproto->nlp_getsufx)( so->so_pcb, TP_LOCAL );
		}
		IFDEBUG(D_TPISO)
			if(tpcb->tp_state != TP_CLOSED)
				printf("LISTEN ERROR: state 0x%x\n", tpcb->tp_state);
		ENDDEBUG
		error = DoEvent(T_LISTEN_req);
		break;

	case PRU_CONNECT2:
		error = EOPNOTSUPP; /* for unix domain sockets */
		break;

	case PRU_CONNECT:
		IFTRACE(D_CONN)
			tptraceTPCB(TPPTmisc, 
			"PRU_CONNECT: so *SHORT_LSUFXP(tpcb) 0x%x lsuflen 0x%x, class 0x%x",
			tpcb->tp_sock, *SHORT_LSUFXP(tpcb), tpcb->tp_lsuffixlen,
				tpcb->tp_class);
		ENDTRACE
		IFDEBUG(D_CONN)
			printf("PRU_CONNECT: so *SHORT_LSUFXP(tpcb) 0x%x lsuflen 0x%x, class 0x%x",
			tpcb->tp_sock, *SHORT_LSUFXP(tpcb), tpcb->tp_lsuffixlen,
				tpcb->tp_class);
		ENDDEBUG
		if (*SHORT_LSUFXP(tpcb) == (short)0) {
			/* no bind was done */
			/* note: this suffix is independent of the extended suffix */
			if( error = (tpcb->tp_nlproto->nlp_pcbbind)(so->so_pcb, MNULL) ) {
				IFDEBUG(D_CONN)
					printf("pcbbind returns error 0x%x\n", error );
				ENDDEBUG
				break;
			}
		}
		if (tpcb->tp_lsuffixlen == 0) { 
			/* didn't set an extended suffix */
			tpcb->tp_lsuffixlen = sizeof(short);
			*SHORT_LSUFXP(tpcb) = (short)
					(tpcb->tp_nlproto->nlp_getsufx)( so->so_pcb, TP_LOCAL );
		} 

		IFDEBUG(D_CONN)
			printf("isop 0x%x isop->isop_socket offset 12 :\n", tpcb->tp_npcb);
			dump_buf( tpcb->tp_npcb, 16);
		ENDDEBUG
		if( error = tp_route_to( nam, tpcb, /* channel */0) )
			break;
		IFDEBUG(D_CONN)
			printf(
				"PRU_CONNECT after tpcb 0x%x so 0x%x npcb 0x%x flags 0x%x\n", 
				tpcb, so, tpcb->tp_npcb, tpcb->tp_flags);
			printf("isop 0x%x isop->isop_socket offset 12 :\n", tpcb->tp_npcb);
			dump_buf( tpcb->tp_npcb, 16);
		ENDDEBUG
		if( tpcb->tp_fsuffixlen == 0 ) {
			/* didn't set peer extended suffix */
			tpcb->tp_fsuffixlen = sizeof(short);
			*SHORT_FSUFXP(tpcb) = (short)
					(tpcb->tp_nlproto->nlp_getsufx)(so->so_pcb, TP_FOREIGN);
		}
		(void) (tpcb->tp_nlproto->nlp_mtu)(so, so->so_pcb,
					&tpcb->tp_l_tpdusize, &tpcb->tp_tpdusize, 0);
		if( tpcb->tp_state == TP_CLOSED) {
			soisconnecting(so);  
			error = DoEvent(T_CONN_req);
		} else {
			(tpcb->tp_nlproto->nlp_pcbdisc)(so->so_pcb);
			error = EISCONN;
		}
		IFPERF(tpcb)
			u_int lsufx, fsufx;
			lsufx = *(u_int *)(tpcb->tp_lsuffix);
			fsufx = *(u_int *)(tpcb->tp_fsuffix);

			tpmeas( tpcb->tp_lref, 
				TPtime_open | (tpcb->tp_xtd_format <<4 ), 
				&time, lsufx, fsufx, tpcb->tp_fref);
		ENDPERF
		break;

	case PRU_ACCEPT: 
		/* all this garbage is to keep accept from returning
		 * before the 3-way handshake is done in class 4.
		 * it'll have to be modified for other classes 
		 */
		IFDEBUG(D_REQUEST)
			printf("PRU_ACCEPT so_error 0x%x\n", so->so_error);
		ENDDEBUG
		so->so_error = 0;
		if ((so->so_state & SS_NBIO) && (so->so_state & SS_ISCONNECTED)== 0) {
			error = EWOULDBLOCK;
			break;
		}
		while ((so->so_state & SS_ISCONNECTED) == 0 && so->so_error == 0) {
			sleep((caddr_t)&so->so_timeo, PZERO+1);
		}
		if (so->so_error) {
			error = so->so_error;
		} else {
			struct sockaddr *sa = mtod(nam, struct sockaddr *);

			nam->m_len = sizeof (struct sockaddr);
			(tpcb->tp_nlproto->nlp_getnetaddr)(so->so_pcb, sa, TP_FOREIGN);

			switch(sa->sa_family = sototpcb(so)->tp_domain) {
			case AF_INET:
				satosin(sa)->sin_port =
					(tpcb->tp_nlproto->nlp_getsufx)(so->so_pcb, TP_FOREIGN);
				break;
			case AF_ISO:
				satosiso(sa)->siso_tsuffix =
					(tpcb->tp_nlproto->nlp_getsufx)(so->so_pcb, TP_FOREIGN);
				/* doesn't cover the case where the suffix is extended -
				 * grotesque - the user *has* to do the getsockopt */
				break;
			}
			IFDEBUG(D_REQUEST)
				printf("ACCEPT PEERADDDR:");
				dump_buf(sa, sizeof(struct sockaddr));
			ENDDEBUG
		}
		IFPERF(tpcb)
			u_int lsufx, fsufx;
			lsufx = *(u_int *)(tpcb->tp_lsuffix);
			fsufx = *(u_int *)(tpcb->tp_fsuffix);

			tpmeas( tpcb->tp_lref, TPtime_open, 
				&time, lsufx, fsufx, tpcb->tp_fref);
		ENDPERF
		break;

	case PRU_RCVD:
		IFTRACE(D_DATA)
			tptraceTPCB(TPPTmisc,
			"RCVD BF: lcredit sent_lcdt cc hiwat \n",
				tpcb->tp_lcredit, tpcb->tp_sent_lcdt,
				so->so_rcv.sb_cc, so->so_rcv.sb_hiwat);
			LOCAL_CREDIT(tpcb);
			tptraceTPCB(TPPTmisc, 
				"PRU_RCVD AF sbspace lcredit hiwat cc",
				sbspace(&so->so_rcv), tpcb->tp_lcredit,
				so->so_rcv.sb_cc, so->so_rcv.sb_hiwat);
		ENDTRACE
		error = DoEvent(T_USR_rcvd); 
		break;

	case PRU_RCVOOB:
		if ((so->so_state & SS_ISCONNECTED) == 0) {
			error = ENOTCONN;
			break;
		}
		if( ! tpcb->tp_xpd_service ) {
			error = EOPNOTSUPP;
			break;
		}
		/* kludge - nam is really flags here */
		error = tp_rcvoob(tpcb, so, m, outflags, (int)nam);
		break;

	case PRU_SENDOOB:
		if ((so->so_state & SS_ISCONNECTED) == 0) {
			error = ENOTCONN;
			break;
		}
		if( ! tpcb->tp_xpd_service ) {
			error = EOPNOTSUPP;
			break;
		}
		error = tp_sendoob(tpcb, so, m, outflags);
		break;

	case PRU_SENDEOT:
		eotsdu = 1;
		/* fall through */
	case PRU_SEND:
		/*
		 * The protocol machine copies mbuf chains,
		 * prepends headers, assigns seq numbers, and
		 * puts the packets on the device.
		 * When they are acked they are removed from the socket buf.
		 *
		 * sosend calls this up until sbspace goes negative.
		 * Sbspace may be made negative by appending this mbuf chain,
		 * possibly by a whole cluster.
		 */
		if ((so->so_state & SS_ISCONNECTED) == 0) {
			error = ENOTCONN;
			break;
		}
		{
			register struct mbuf *n;
			register int len=0;
			register struct sockbuf *sb = &so->so_snd;

			n = m;
			while (n) { /* Could have eotsdu and no data.(presently MUST have
						 *	an mbuf though, even if its length == 0) 
						 */
				len += n->m_len;
				if( n->m_next == MNULL && eotsdu )  {
					CHANGE_MTYPE(n, TPMT_EOT);
				}
				n = n->m_next;
			}
			IFPERF(tpcb)
			   PStat(tpcb, Nb_from_sess) += len;
			   tpmeas(tpcb->tp_lref, TPtime_from_session, 0, 0, 
					PStat(tpcb, Nb_from_sess), len);
			ENDPERF
			IFDEBUG(D_SYSCALL)
				printf(
				"PRU_SEND: eot %d before sbappend 0x%x len 0x%x to sb @ 0x%x\n",
					eotsdu, m,len, &sb->sb_mb);
				dump_mbuf(sb->sb_mb, "so_snd.sb_mb");
				dump_mbuf(m, "m : to be added");
			ENDDEBUG
			/* The last mbuf has type TPMT_EOT so it will never be compressed
			 * with TPMT_DATA mbufs, but if this was an EOTSDU request w/o
			 * any data, the only way to keep this mbuf from being thrown
			 * away is to link it through the m_act field
			 * We are ASSUMING that if there are any data at all with this
			 * request, the last mbuf will be non-empty!!!
			 */
			if( m->m_type == TPMT_EOT ) /* first mbuf in chain is EOT? */
				sbappendrecord(sb, m);  /* to keep 2 TPMT_EOTs from being
												compressed */
			else
				sbappend(sb, m);
			IFDEBUG(D_SYSCALL)
				printf("PRU_SEND: eot %d after sbappend 0x%x len 0x%x\n",
					eotsdu, m,len);
				dump_mbuf(sb->sb_mb, "so_snd.sb_mb");
			ENDDEBUG
			u.u_r.r_val1  += len; 
			error = DoEvent(T_DATA_req); 
			IFDEBUG(D_SYSCALL)
				printf("PRU_SEND: after driver error 0x%x \n",error);
			ENDDEBUG
		}
		break;

	case PRU_SOCKADDR: {
			struct sockaddr *sa = mtod(nam, struct sockaddr *);

			nam->m_len = sizeof (struct sockaddr);
			(tpcb->tp_nlproto->nlp_getnetaddr)(so->so_pcb, sa, TP_LOCAL);
			switch ( sa->sa_family = sototpcb(so)->tp_domain ) {
			case AF_INET:
				satosin(sa)->sin_port =
					(tpcb->tp_nlproto->nlp_getsufx)(so->so_pcb, TP_LOCAL);
				break;
			case AF_ISO:
				satosiso(sa)->siso_tsuffix =
					(tpcb->tp_nlproto->nlp_getsufx)(so->so_pcb, TP_LOCAL);
				break;
			}
		}
		break;

	case PRU_PEERADDR:
		if( (so->so_state & SS_ISCONNECTED) && 
			(so->so_state & SS_ISDISCONNECTING) == 0) {
				struct sockaddr *sa = mtod(nam, struct sockaddr *);

			nam->m_len = sizeof (struct sockaddr);

			(tpcb->tp_nlproto->nlp_getnetaddr)(so->so_pcb, sa, TP_FOREIGN);

			switch ( sa->sa_family = sototpcb(so)->tp_domain ) {
			case AF_INET:
				satosin(sa)->sin_port =
					(tpcb->tp_nlproto->nlp_getsufx)(so->so_pcb, TP_FOREIGN);
				break;
			case AF_ISO:
				satosiso(sa)->siso_tsuffix =
					(tpcb->tp_nlproto->nlp_getsufx)(so->so_pcb, TP_FOREIGN);
				break;
			}
			IFDEBUG(D_REQUEST)
				printf("PEERADDDR:");
				dump_buf(sa, sizeof(struct sockaddr));
			ENDDEBUG
		} else 
			error = ENOTCONN;
		break;

	case PRU_CONTROL:
		error = EOPNOTSUPP;
		break;

	case PRU_PROTOSEND:
	case PRU_PROTORCV:
	case PRU_SENSE:
	case PRU_SLOWTIMO:
	case PRU_FASTTIMO:
		error = EOPNOTSUPP;
		break;

	default:
#ifdef ARGO_DEBUG
		printf("tp_usrreq UNKNOWN PRU %d\n", req);
#endif ARGO_DEBUG
		error = EOPNOTSUPP;
	}

	IFDEBUG(D_REQUEST)
		printf("returning from tp_usrreq(so 0x%x) error 0x%x\n", so, error);
	ENDDEBUG
	IFTRACE(D_REQUEST)
		tptraceTPCB(TPPTusrreq, "END req so m state [", req, so, m, 
			tpcb?0:tpcb->tp_state);
	ENDTRACE
	splx(s);
	return error;
}
