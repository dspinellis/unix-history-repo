/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tp_subr.c	7.14 (Berkeley) %G%
 */

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
 * $Header: tp_subr.c,v 5.3 88/11/18 17:28:43 nhall Exp $
 * $Source: /usr/argo/sys/netiso/RCS/tp_subr.c,v $
 *
 * The main work of data transfer is done here.
 * These routines are called from tp.trans.
 * They include the routines that check the validity of acks and Xacks,
 * (tp_goodack() and tp_goodXack() )
 * take packets from socket buffers and send them (tp_send()),
 * drop the data from the socket buffers (tp_sbdrop()),  
 * and put incoming packet data into socket buffers (tp_stash()).
 */

#include "param.h"
#include "mbuf.h"
#include "socket.h"
#include "socketvar.h"
#include "protosw.h"
#include "errno.h"
#include "types.h"
#include "time.h"
#include "kernel.h"

#include "tp_ip.h"
#include "iso.h"
#include "argo_debug.h"
#include "tp_timer.h"
#include "tp_param.h"
#include "tp_stat.h"
#include "tp_pcb.h"
#include "tp_tpdu.h"
#include "tp_trace.h"
#include "tp_meas.h"
#include "tp_seq.h"

int 		tp_emit(), tp_sbdrop();

/*
 * CALLED FROM:
 *	tp.trans, when an XAK arrives
 * FUNCTION and ARGUMENTS:
 * 	Determines if the sequence number (seq) from the XAK 
 * 	acks anything new.  If so, drop the appropriate tpdu
 * 	from the XPD send queue.
 * RETURN VALUE:
 * 	Returns 1 if it did this, 0 if the ack caused no action.
 */
int
tp_goodXack(tpcb, seq)
	struct tp_pcb	*tpcb;
	SeqNum 			seq; 
{

	IFTRACE(D_XPD)
		tptraceTPCB(TPPTgotXack, 
			seq, tpcb->tp_Xuna, tpcb->tp_Xsndnxt, tpcb->tp_sndhiwat, 
			tpcb->tp_snduna); 
	ENDTRACE

	if ( seq == tpcb->tp_Xuna ) {
			tpcb->tp_Xuna = tpcb->tp_Xsndnxt;

			/* DROP 1 packet from the Xsnd socket buf - just so happens
			 * that only one packet can be there at any time
			 * so drop the whole thing.  If you allow > 1 packet
			 * the socket buffer, then you'll have to keep
			 * track of how many characters went w/ each XPD tpdu, so this
			 * will get messier
			 */
			IFDEBUG(D_XPD)
				dump_mbuf(tpcb->tp_Xsnd.sb_mb,
					"tp_goodXack Xsnd before sbdrop");
			ENDDEBUG

			IFTRACE(D_XPD)
				tptraceTPCB(TPPTmisc, 
					"goodXack: dropping cc ",
					(int)(tpcb->tp_Xsnd.sb_cc),
					0,0,0);
			ENDTRACE
			sbdrop( &tpcb->tp_Xsnd, (int)(tpcb->tp_Xsnd.sb_cc));
			CONG_ACK(tpcb, seq);
			return 1;
	} 
	return 0;
}

/*
 * CALLED FROM:
 *  tp_good_ack()
 * FUNCTION and ARGUMENTS:
 *  updates
 *  smoothed average round trip time (*rtt)
 *  roundtrip time variance (*rtv) - actually deviation, not variance
 *  given the new value (diff)
 * RETURN VALUE:
 * void
 */

void
tp_rtt_rtv( rtt, rtv, newmeas )
	register int	 *rtt, *rtv;
	int newmeas;
{
	int delta = newmeas - *rtt;

	if ((*rtt += (delta >> TP_RTT_ALPHA)) <= 0)
		*rtt = 1;
	if (delta < 0)
		delta = -delta;
	if (*rtv = ((delta - *rtv) >> TP_RTV_ALPHA) <= 0)
		*rtv = 1;
}

/*
 * CALLED FROM:
 *  tp.trans when an AK arrives
 * FUNCTION and ARGUMENTS:
 * 	Given (cdt), the credit from the AK tpdu, and 
 *	(seq), the sequence number from the AK tpdu,
 *  tp_goodack() determines if the AK acknowledges something in the send
 * 	window, and if so, drops the appropriate packets from the retransmission
 *  list, computes the round trip time, and updates the retransmission timer
 *  based on the new smoothed round trip time.
 * RETURN VALUE:
 * 	Returns 1 if
 * 	EITHER it actually acked something heretofore unacknowledged
 * 	OR no news but the credit should be processed.
 * 	If something heretofore unacked was acked with this sequence number,
 * 	the appropriate tpdus are dropped from the retransmission control list,
 * 	by calling tp_sbdrop().
 * 	No need to see the tpdu itself.
 */
int
tp_goodack(tpcb, cdt, seq, subseq)
	register struct tp_pcb	*tpcb;
	u_int					cdt;
	register SeqNum			seq, subseq;
{
	int 	old_fcredit = tpcb->tp_fcredit; 
	int 	bang = 0; 	/* bang --> ack for something heretofore unacked */

	IFDEBUG(D_ACKRECV)
		printf("goodack seq 0x%x cdt 0x%x snduna 0x%x sndhiwat 0x%x\n",
			seq, cdt, tpcb->tp_snduna, tpcb->tp_sndhiwat);
	ENDDEBUG
	IFTRACE(D_ACKRECV)
		tptraceTPCB(TPPTgotack, 
			seq,cdt, tpcb->tp_snduna,tpcb->tp_sndhiwat,subseq); 
	ENDTRACE

	IFPERF(tpcb)
		tpmeas(tpcb->tp_lref, TPtime_ack_rcvd, (struct timeval *)0, seq, 0, 0);
	ENDPERF

	if ( subseq != 0 && (subseq <= tpcb->tp_r_subseq) ) {
		/* discard the ack */
		IFTRACE(D_ACKRECV)
			tptraceTPCB(TPPTmisc, "goodack discard : subseq tp_r_subseq",
				subseq, tpcb->tp_r_subseq, 0, 0);
		ENDTRACE
		return 0;
	} else {
		tpcb->tp_r_subseq = subseq;
	}

	if ( IN_SWINDOW(tpcb, seq, 
			tpcb->tp_snduna, SEQ(tpcb, tpcb->tp_sndhiwat+1)) ) {

		IFDEBUG(D_XPD)
			dump_mbuf(tpcb->tp_sock->so_snd.sb_mb, 
				"tp_goodack snd before sbdrop");
		ENDDEBUG
		tpsbcheck(tpcb, 0);
		(void)tp_sbdrop(tpcb, seq);
		tpsbcheck(tpcb, 1);

		/* increase congestion window but don't let it get too big */
		{
			register int maxcdt = tpcb->tp_xtd_format?0xffff:0xf;
			CONG_ACK(tpcb, seq);
		}

		/* Compute smoothed round trip time.
		 * Only measure rtt for tp_snduna if acked and the data
		 * were not retransmitted.
		 */
		if (tpcb->tp_rttemit && SEQ_GT(tpcb, seq, tpcb->tp_rttseq)) {
			int x = tick - tpcb->tp_rttemit;

			if (tpcb->tp_rtt)
				tp_rtt_rtv(&(tpcb->tp_rtt), &(tpcb->tp_rtv), x);
			else {
				tpcb->tp_rtt = x;
				tpcb->tp_rtv = x >> 1;
			}

			{	/* update the global rtt, rtv stats */
				int i = tpcb->tp_flags & (TPF_PEER_ON_SAMENET | TPF_NLQOS_PDN);
				tp_rtt_rtv(tp_stat.ts_rtt + i, tp_stat.ts_rtv + i, x);

				IFTRACE(D_RTT)
					tptraceTPCB(TPPTmisc, "Global rtt, rtv: i", i, 0, 0, 0);
				ENDTRACE
			}

			IFTRACE(D_RTT)
				tptraceTPCB(TPPTmisc, 
				"Smoothed rtt: tp_snduna, (time.sec, time.usec), peer_acktime",
				tpcb->tp_snduna, time.tv_sec, time.tv_usec,
					tpcb->tp_peer_acktime);

				tptraceTPCB(TPPTmisc, 
					"(secs): emittime diff(x) rtt, rtv",
						tpcb->tp_rttemit, x, tpcb->tp_rtt, tpcb->tp_rtv);
			ENDTRACE

			{
				/* Update data retransmission timer based on the smoothed
				 * round trip time, peer ack time, and the pseudo-arbitrary
				 * number 2.
				 * new ticks: (avg rtt + 4*dev)
				 * rtt, rtv are in hz-ticks,
				 * and slowtimo-ticks are hz / 2;
				 * We want no less than peer ack time and no less than 2
				 */


				int rtt = tpcb->tp_rtt, rtv = tpcb->tp_rtv,
					old = tpcb->tp_dt_ticks, new;

				new = (((rtt + (rtv << 2)) << 1) + hz) / hz;
				new = MAX(new + 1, old);
				new = MAX(new, tpcb->tp_peer_acktime);
				new = MAX(new, 2);
				IFTRACE(D_RTT)
					tptraceTPCB(TPPTmisc, "oldticks ,rtv, rtt, newticks",
						old, rtv, rtt, new);
				ENDTRACE
				tpcb->tp_dt_ticks = new;
			}
			tpcb->tp_rxtcur = tpcb->tp_dt_ticks;
			tpcb->tp_rxtshift = 0;

		}
		tpcb->tp_snduna = seq;
		tpcb->tp_retrans = tpcb->tp_Nretrans; /* CE_BIT */

		bang++;
	} 

	if( cdt != 0 && old_fcredit == 0 ) {
		tpcb->tp_sendfcc = 1;
	}
	if( cdt == 0 && old_fcredit != 0 ) {
		IncStat(ts_zfcdt);
	}
	tpcb->tp_fcredit = cdt;

	IFDEBUG(D_ACKRECV)
		printf("goodack returning 0x%x, bang 0x%x cdt 0x%x old_fcredit 0x%x\n",
			(bang || (old_fcredit < cdt) ), bang, cdt, old_fcredit );
	ENDDEBUG

	return (bang || (old_fcredit < cdt)) ;
}

/*
 * CALLED FROM:
 *  tp_goodack()
 * FUNCTION and ARGUMENTS:
 *  drops everything up TO but not INCLUDING seq # (seq)
 *  from the retransmission queue.
 */
tp_sbdrop(tpcb, seq) 
	register struct 	tp_pcb 			*tpcb;
	SeqNum					seq;
{
	struct sockbuf *sb = &tpcb->tp_sock->so_snd;
	register int i = ((int)seq)-((int)tpcb->tp_snduna);
	int	oldcc = sb->sb_cc;

	if (i < 0) i += tpcb->tp_seqhalf;
	IFDEBUG(D_ACKRECV)
		printf("tp_sbdroping %d up through seq 0x%x\n", i, seq);
	ENDDEBUG
	while (i-- > 0)
		sbdroprecord(sb);
	if (SEQ_LT(tpcb, tpcb->tp_sndhiwat, seq))
		tpcb->tp_sndhiwat_m = 0;
	return (oldcc - sb->sb_cc);
}

/*
 * CALLED FROM:
 * 	tp.trans on user send request, arrival of AK and arrival of XAK
 * FUNCTION and ARGUMENTS:
 * 	Emits tpdus starting at sequence number (lowseq).
 * 	Emits until a) runs out of data, or  b) runs into an XPD mark, or
 * 			c) it hits seq number (highseq)
 *
 * 	If you want XPD to buffer > 1 du per socket buffer, you can
 * 	modifiy this to issue XPD tpdus also, but then it'll have
 * 	to take some argument(s) to distinguish between the type of DU to
 * 	hand tp_emit.
 *
 * 	When something is sent for the first time, its time-of-send
 * 	is stashed (the last RTT_NUM of them are stashed).  When the
 * 	ack arrives, the smoothed round-trip time is figured using this value.
 * RETURN VALUE:
 * 	the highest seq # sent successfully.
 */
tp_send(tpcb)
	register struct tp_pcb	*tpcb;
{
	register int			len;
	register struct mbuf	*m; /* the one we're inspecting now */
	struct mbuf				*mb;/* beginning of this tpdu */
	struct mbuf 			*nextrecord; /* NOT next tpdu but next sb record */
	struct 	sockbuf			*sb = &tpcb->tp_sock->so_snd;
	unsigned int			eotsdu_reached=0;
	SeqNum					lowseq, highseq ;
	SeqNum					lowsave; 
#ifdef TP_PERF_MEAS

	int			 			send_start_time = tick;
#endif TP_PERF_MEAS

	lowsave =  lowseq = SEQ(tpcb, tpcb->tp_sndhiwat + 1);

	ASSERT( tpcb->tp_cong_win > 0 && tpcb->tp_cong_win < 0xffff);

	if( tpcb->tp_rx_strat & TPRX_USE_CW ) {
			/*first hiseq is temp vbl*/
		highseq = MIN(tpcb->tp_fcredit, tpcb->tp_cong_win); 
	} else {
		highseq = tpcb->tp_fcredit;
	}
	highseq = SEQ(tpcb, tpcb->tp_snduna + highseq);
		
	SEQ_DEC(tpcb, highseq);

	IFDEBUG(D_DATA)
		printf( 
			"tp_send enter tpcb 0x%x l %d -> h %d\ndump of sb_mb:\n",
			tpcb, lowseq, highseq);
		dump_mbuf(sb->sb_mb, "sb_mb:");
	ENDDEBUG
	IFTRACE(D_DATA)
		tptraceTPCB( TPPTmisc, "tp_send lowsave sndhiwat snduna", 
			lowsave, tpcb->tp_sndhiwat,  tpcb->tp_snduna, 0);
		tptraceTPCB( TPPTmisc, "tp_send low high fcredit congwin", 
			lowseq, highseq, tpcb->tp_fcredit,  tpcb->tp_cong_win);
	ENDTRACE


	if	( SEQ_GT(tpcb, lowseq, highseq) )
			return ; /* don't send, don't change hiwat, don't set timers */

	ASSERT( SEQ_LEQ(tpcb, lowseq, highseq) );
	SEQ_DEC(tpcb, lowseq);

	if (tpcb->tp_Xsnd.sb_mb) {
		IFTRACE(D_XPD)
			tptraceTPCB( TPPTmisc,
				"tp_send XPD mark low high tpcb.Xuna", 
				lowseq, highseq, tpcb->tp_Xsnd.sb_mb, 0);
		ENDTRACE
		/* stop sending here because there are unacked XPD present
		 */
		IncStat(ts_xpd_intheway);
		goto done;
	}
	IFTRACE(D_DATA)
		tptraceTPCB( TPPTmisc, "tp_send 2 low high fcredit congwin", 
			lowseq, highseq, tpcb->tp_fcredit,  tpcb->tp_cong_win);
	ENDTRACE

	if (m = tpcb->tp_sndhiwat_m)
		mb  = m->m_nextpkt;
	else
		mb = sb->sb_mb;
	while ((SEQ_LT(tpcb, lowseq, highseq)) && mb ) {

		/* 
		 * In all cases, m points to mbuf containing first octet to be
		 * sent in the tpdu AFTER the one we're going to send now,
		 * or else m is null.
		 *
		 * The chain we're working on now begins at mb and has length <len>.
		 */

		eotsdu_reached = (mb->m_flags & M_EOR) != 0;
		len = mb->m_pkthdr.len;
		IFTRACE(D_STASH)
			tptraceTPCB( TPPTmisc, 
				"tp_send mcopy low high eotsdu_reached len", 
				lowseq, highseq, eotsdu_reached, len);
		ENDTRACE

		/* make a copy - mb goes into the retransmission list 
		 * while m gets emitted.  m_copy won't copy a zero-length mbuf.
		 */
		m = m_copy(mb, 0, M_COPYALL);
		if (m == MNULL)
				goto done;
		SEQ_INC(tpcb,lowseq);	/* it was decremented at the beginning */
		IFTRACE(D_DATA)
			tptraceTPCB( TPPTmisc, 
				"tp_send emitting DT lowseq eotsdu_reached len",
				lowseq, eotsdu_reached, len, 0);
		ENDTRACE
		if (mb->m_nextpkt == 0 && tpcb->tp_oktonagle) {
			SEQ_INC(tpcb, tpcb->tp_sndnum);
			tpcb->tp_oktonagle = 0;
			/* when headers are precomputed, may need to fill
			   in checksum here */
		}
		if (tpcb->tp_sock->so_error =
			tp_emit(DT_TPDU_type, tpcb, lowseq, eotsdu_reached, m)) {
			/* error */
			SEQ_DEC(tpcb, lowseq); 
			goto done;
		}
		/* set the transmit-time for computation of round-trip times */
		if (tpcb->tp_rttemit == 0) {
			tpcb->tp_rttemit = tick;
			tpcb->tp_rttseq = lowseq;
		}
		tpcb->tp_sndhiwat_m = mb;
		mb = mb->m_nextpkt;
	}

done:
#ifdef TP_PERF_MEAS
	IFPERF(tpcb)
		{
			register int npkts;
			int	 elapsed = tick - send_start_time, *t;
			struct timeval now;

			npkts = lowseq;
			SEQ_INC(tpcb, npkts);
			npkts = SEQ_SUB(tpcb, npkts, lowsave);

			if(npkts > 0) 
				tpcb->tp_Nwindow++;

			if (npkts > TP_PM_MAX) 
				npkts = TP_PM_MAX; 

			t = &(tpcb->tp_p_meas->tps_sendtime[npkts]);
			*t += (t - elapsed) >> TP_RTT_ALPHA;

			if ( SEQ_LT(tpcb, lowseq, highseq) ) {
				IncPStat(tpcb, tps_win_lim_by_data[npkts] );
			} else {
				IncPStat(tpcb, tps_win_lim_by_cdt[npkts] );
				/* not true with congestion-window being used */
			}
			now.tv_sec = elapsed / hz;
			now.tv_usec = (elapsed - (hz * now.tv_sec)) * 1000000 / hz;
			tpmeas( tpcb->tp_lref, 
					TPsbsend, &elapsed, lowsave, tpcb->tp_Nwindow, npkts);
		}
	ENDPERF
#endif TP_PERF_MEAS

	tpcb->tp_sndhiwat = lowseq;

	if ( SEQ_LEQ(tpcb, lowsave, tpcb->tp_sndhiwat)  && 
			(tpcb->tp_class != TP_CLASS_0) ) 
			tp_etimeout(tpcb->tp_refp, TM_data_retrans, lowsave, 
				tpcb->tp_sndhiwat,
				(u_int)tpcb->tp_Nretrans, (int)tpcb->tp_dt_ticks);
	IFTRACE(D_DATA)
		tptraceTPCB( TPPTmisc, 
			"tp_send at end: sndhiwat lowseq eotsdu_reached error",
			tpcb->tp_sndhiwat, lowseq, eotsdu_reached, tpcb->tp_sock->so_error);
		
	ENDTRACE
}

int TPNagleok;
int TPNagled;

tp_packetize(tpcb, m, eotsdu)
register struct tp_pcb *tpcb;
register struct mbuf *m;
int eotsdu;
{
	register struct mbuf *n;
	register struct sockbuf *sb = &tpcb->tp_sock->so_snd;
	int	maxsize = tpcb->tp_l_tpdusize 
			- tp_headersize(DT_TPDU_type, tpcb)
			- (tpcb->tp_use_checksum?4:0) ;
	int totlen = m->m_pkthdr.len;
	struct mbuf *m_split();
	/*
	 * Pre-packetize the data in the sockbuf
	 * according to negotiated mtu.  Do it here
	 * where we can safely wait for mbufs.
	 *
	 * This presumes knowledge of sockbuf conventions.
	 * TODO: allocate space for header and fill it in (once!).
	 */
	IFTRACE(D_DATA)
		tptraceTPCB(TPPTmisc,
		"SEND BF: maxsize totlen eotsdu",
			maxsize, totlen, eotsdu, 0);
	ENDTRACE
	if (tpcb->tp_oktonagle) {
		if ((n = sb->sb_mb) == 0)
			panic("tp_packetize");
		while (n->m_act)
			n = n->m_act;
		if (n->m_flags & M_EOR)
			panic("tp_packetize 2");
		SEQ_INC(tpcb, tpcb->tp_sndnum);
		if (totlen + n->m_pkthdr.len < maxsize) {
			/* There is an unsent packet with space, combine data */
			struct mbuf *old_n = n;
			tpsbcheck(tpcb,3);
			n->m_pkthdr.len += totlen;
			while (n->m_next)
				n = n->m_next;
			sbcompress(sb, m, n);
			tpsbcheck(tpcb,4);
			n = old_n;
			TPNagled++;
			goto out;
		}
	}
	while (m) {
		n = m;
		if (totlen > maxsize) {
			if ((m = m_split(n, maxsize, M_WAIT)) == 0)
				panic("tp_packetize");
		} else
			m = 0;
		totlen -= maxsize;
		tpsbcheck(tpcb, 5);
		sbappendrecord(sb, n);
		tpsbcheck(tpcb, 6);
		SEQ_INC(tpcb, tpcb->tp_sndnum);
	}
out:
	if (eotsdu) {
		n->m_flags |= M_EOR;  /* XXX belongs at end */
		tpcb->tp_oktonagle = 0;
	} else {
		SEQ_DEC(tpcb, tpcb->tp_sndnum);
		tpcb->tp_oktonagle = 1;
		TPNagleok++;
	}
	return 0;
}


/*
 * NAME: tp_stash()
 * CALLED FROM:
 *	tp.trans on arrival of a DT tpdu
 * FUNCTION, ARGUMENTS, and RETURN VALUE:
 * 	Returns 1 if 
 *		a) something new arrived and it's got eotsdu_reached bit on,
 * 		b) this arrival was caused other out-of-sequence things to be
 *    	accepted, or
 * 		c) this arrival is the highest seq # for which we last gave credit
 *   	(sender just sent a whole window)
 *  In other words, returns 1 if tp should send an ack immediately, 0 if 
 *  the ack can wait a while.
 *
 * Note: this implementation no longer renegs on credit, (except
 * when debugging option D_RENEG is on, for the purpose of testing
 * ack subsequencing), so we don't  need to check for incoming tpdus 
 * being in a reneged portion of the window.
 */

tp_stash( tpcb, e )
	register struct tp_pcb		*tpcb;
	register struct tp_event	*e;
{
	register int		ack_reason= tpcb->tp_ack_strat & ACK_STRAT_EACH;
									/* 0--> delay acks until full window */
									/* 1--> ack each tpdu */
#ifndef lint
#define E e->ATTR(DT_TPDU)
#else lint
#define E e->ev_union.EV_DT_TPDU
#endif lint

	if ( E.e_eot ) {
		register struct mbuf *n = E.e_data;
		n->m_flags |= M_EOR;
		n->m_act = 0;
	}
		IFDEBUG(D_STASH)
			dump_mbuf(tpcb->tp_sock->so_rcv.sb_mb, 
				"stash: so_rcv before appending");
			dump_mbuf(E.e_data,
				"stash: e_data before appending");
		ENDDEBUG

	IFPERF(tpcb)
		PStat(tpcb, Nb_from_ll) += E.e_datalen;
		tpmeas(tpcb->tp_lref, TPtime_from_ll, &e->e_time,
			E.e_seq, (u_int)PStat(tpcb, Nb_from_ll), (u_int)E.e_datalen);
	ENDPERF

	if (E.e_seq == tpcb->tp_rcvnxt) {

		IFDEBUG(D_STASH)
			printf("stash EQ: seq 0x%x datalen 0x%x eot 0x%x\n", 
			E.e_seq, E.e_datalen, E.e_eot);
		ENDDEBUG

		IFTRACE(D_STASH)
			tptraceTPCB(TPPTmisc, "stash EQ: seq len eot", 
			E.e_seq, E.e_datalen, E.e_eot, 0);
		ENDTRACE

		SET_DELACK(tpcb);

		sbappend(&tpcb->tp_sock->so_rcv, E.e_data);

		SEQ_INC( tpcb, tpcb->tp_rcvnxt );
		/* 
		 * move chains from the reassembly queue to the socket buffer
		 */
		if (tpcb->tp_rsycnt) {
			register struct mbuf **mp;
			struct mbuf **mplim;

			mp = tpcb->tp_rsyq + (tpcb->tp_rcvnxt % tpcb->tp_maxlcredit);
			mplim = tpcb->tp_rsyq + tpcb->tp_maxlcredit;

			while (tpcb->tp_rsycnt && *mp) {
				sbappend(&tpcb->tp_sock->so_rcv, *mp);
				tpcb->tp_rsycnt--;
				*mp = 0;
				SEQ_INC(tpcb, tpcb->tp_rcvnxt);
				ack_reason |= ACK_REORDER;
				if (++mp == mplim)
					mp = tpcb->tp_rsyq;
			}
		}
		IFDEBUG(D_STASH)
			dump_mbuf(tpcb->tp_sock->so_rcv.sb_mb, 
				"stash: so_rcv after appending");
		ENDDEBUG

	} else {
		register struct mbuf **mp;
		SeqNum uwe;

		IFTRACE(D_STASH)
			tptraceTPCB(TPPTmisc, "stash Reseq: seq rcvnxt lcdt", 
			E.e_seq, tpcb->tp_rcvnxt, tpcb->tp_lcredit, 0);
		ENDTRACE

		if (tpcb->tp_rsyq == 0)
			tp_rsyset(tpcb);
		uwe = SEQ(tpcb, tpcb->tp_rcvnxt + tpcb->tp_maxlcredit);
		if (tpcb->tp_rsyq == 0 ||
						!IN_RWINDOW(tpcb, E.e_seq, tpcb->tp_rcvnxt, uwe)) {
			ack_reason = ACK_DONT;
			m_freem(E.e_data);
		} else if (*(mp = tpcb->tp_rsyq + (E.e_seq % tpcb->tp_maxlcredit))) {
			IFDEBUG(D_STASH)
				printf("tp_stash - drop & ack\n");
			ENDDEBUG

			/* retransmission - drop it and force an ack */
			IncStat(ts_dt_dup);
			IFPERF(tpcb)
				IncPStat(tpcb, tps_n_ack_cuz_dup);
			ENDPERF

			m_freem(E.e_data);
			ack_reason |= ACK_DUP;
		} else {
			*mp = E.e_data;
			tpcb->tp_rsycnt++;
			ack_reason = ACK_DONT;
		}
	}
	/* there were some comments of historical interest here. */
	{
		LOCAL_CREDIT(tpcb);

		if ( E.e_seq ==  tpcb->tp_sent_uwe )
			ack_reason |= ACK_STRAT_FULLWIN;

		IFTRACE(D_STASH)
			tptraceTPCB(TPPTmisc, 
				"end of stash, eot, ack_reason, sent_uwe ",
				E.e_eot, ack_reason, tpcb->tp_sent_uwe, 0); 
		ENDTRACE

		if ( ack_reason == ACK_DONT ) {
			IncStat( ts_ackreason[ACK_DONT] );
			return 0;
		} else {
			IFPERF(tpcb)
				if(ack_reason & ACK_STRAT_EACH) {
					IncPStat(tpcb, tps_n_ack_cuz_strat);
				} else if(ack_reason & ACK_STRAT_FULLWIN) {
					IncPStat(tpcb, tps_n_ack_cuz_fullwin);
				} else if(ack_reason & ACK_REORDER) {
					IncPStat(tpcb, tps_n_ack_cuz_reorder);
				}
				tpmeas(tpcb->tp_lref, TPtime_ack_sent, 0, 
							SEQ_ADD(tpcb, E.e_seq, 1), 0, 0);
			ENDPERF
			{
				register int i;

				/* keep track of all reasons that apply */
				for( i=1; i<_ACK_NUM_REASONS_ ;i++) {
					if( ack_reason & (1<<i) ) 
						IncStat( ts_ackreason[i] );
				}
			}
			return 1;
		}
	}
}

/*
 * tp_rsyflush - drop all the packets on the reassembly queue.
 * Do this when closing the socket, or when somebody has changed
 * the space avaible in the receive socket (XXX).
 */
tp_rsyflush(tpcb)
register struct tp_pcb *tpcb;
{
	register struct mbuf *m, **mp;
	if (tpcb->tp_rsycnt) {
		for (mp == tpcb->tp_rsyq + tpcb->tp_maxlcredit;
									 --mp >= tpcb->tp_rsyq; )
			if (*mp) {
				tpcb->tp_rsycnt--;
				m_freem(*mp);
			}
		if (tpcb->tp_rsycnt)
			panic("tp_rsyflush");
	}
	free((caddr_t)tpcb->tp_rsyq, M_PCB);
	tpcb->tp_rsyq = 0;
}

tp_rsyset(tpcb)
register struct tp_pcb *tpcb;
{
	register struct socket *so = tpcb->tp_sock;
	int maxcredit  = tpcb->tp_xtd_format ? 0xffff : 0xf;
	int old_credit = tpcb->tp_maxlcredit;
	caddr_t	rsyq;

	tpcb->tp_maxlcredit = maxcredit = min(maxcredit,
		  (so->so_rcv.sb_hiwat + tpcb->tp_l_tpdusize)/ tpcb->tp_l_tpdusize);

	if (old_credit == tpcb->tp_maxlcredit && tpcb->tp_rsyq != 0)
		return;
	maxcredit *= sizeof(struct mbuf *);
	if (tpcb->tp_rsyq)
		tp_rsyflush(tpcb);
	if (rsyq = (caddr_t)malloc(maxcredit, M_PCB, M_NOWAIT))
		bzero(rsyq, maxcredit);
	tpcb->tp_rsyq = (struct mbuf **)rsyq;
}

tpsbcheck(tpcb, i)
struct tp_pcb *tpcb;
{
	register struct mbuf *n, *m;
	register int len = 0, mbcnt = 0, pktlen;
	struct sockbuf *sb = &tpcb->tp_sock->so_snd;

	for (n = sb->sb_mb; n; n = n->m_nextpkt) {
		if ((n->m_flags & M_PKTHDR) == 0)
			panic("tpsbcheck nohdr");
		pktlen = len + n->m_pkthdr.len;
	    for (m = n; m; m = m->m_next) {
			len += m->m_len;
			mbcnt += MSIZE;
			if (m->m_flags & M_EXT)
				mbcnt += m->m_ext.ext_size;
		}
		if (len != pktlen) {
			printf("test %d; len %d != pktlen %d on mbuf 0x%x\n",
				i, len, pktlen, n);
			panic("tpsbcheck short");
		}
	}
	if (len != sb->sb_cc || mbcnt != sb->sb_mbcnt) {
		printf("test %d: cc %d != %d || mbcnt %d != %d\n", i, len, sb->sb_cc,
		    mbcnt, sb->sb_mbcnt);
		panic("tpsbcheck");
	}
}
