/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tp_timer.c	7.8 (Berkeley) %G%
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
 * $Header: tp_timer.c,v 5.2 88/11/18 17:29:07 nhall Exp $
 * $Source: /usr/argo/sys/netiso/RCS/tp_timer.c,v $
 *
 * Contains all the timer code.  
 * There are two sources of calls to these routines:
 * the clock, and tp.trans. (ok, and tp_pcb.c calls it at init time)
 *
 * Timers come in two flavors - those that generally get
 * cancelled (tp_ctimeout, tp_cuntimeout)
 * and those that either usually expire (tp_etimeout, 
 * tp_euntimeout, tp_slowtimo) or may require more than one instance
 * of the timer active at a time.
 *
 * The C timers are stored in the tp_ref structure. Their "going off"
 * is manifested by a driver event of the TM_xxx form.
 *
 * The E timers are handled like the generic kernel callouts.
 * Their "going off" is manifested by a function call w/ 3 arguments.
 */

#include "param.h"
#include "types.h"
#include "time.h"
#include "malloc.h"
#include "socket.h"

#include "tp_param.h"
#include "tp_timer.h"
#include "tp_stat.h"
#include "tp_pcb.h"
#include "tp_tpdu.h"
#include "argo_debug.h"
#include "tp_trace.h"
#include "tp_seq.h"

struct	Ecallout *TP_callfree;
struct	Ecallout *TP_callout; 
struct	tp_ref *tp_ref;
int		N_TPREF = 127;
struct	tp_refinfo tp_refinfo;

/*
 * CALLED FROM:
 *  at autoconfig time from tp_init() 
 * 	a combo of event, state, predicate
 * FUNCTION and ARGUMENTS:
 *  initialize data structures for the timers
 */
void
tp_timerinit()
{
	register struct Ecallout *e;
	register int s;
#define GETME(x, t, n) {s = (n)*sizeof(*x); x = (t) malloc(s, M_PCB, M_NOWAIT);\
if (x == 0) panic("tp_timerinit"); bzero((caddr_t)x, s);}
	/*
	 * Initialize storage
	 */
	GETME(tp_ref, struct tp_ref *, 1 +  N_TPREF);
	tp_refinfo.tpr_base = tp_ref;
	tp_refinfo.tpr_size = N_TPREF + 1;  /* Need to start somewhere */
#undef GETME
}

/**********************  e timers *************************/

int Enoisy = 1;
/*
 * CALLED FROM:
 *  tp.trans all over
 * FUNCTION and ARGUMENTS:
 * Set an E type timer.  (refp) is the ref structure.
 * Causes  fun(arg1,arg2,arg3) to be called after time t.
 */
void
tp_etimeout(refp, fun, arg1, arg2, arg3, ticks)
	struct tp_ref	*refp;		
	int 			fun; 	/* function to be called */
	u_int			arg1, arg2; 
	int				arg3;
	register int	ticks;
{

	register struct tp_pcb *tpcb = refp->tpr_pcb;
	register struct Ccallout *callp;
	IFDEBUG(D_TIMER)
		printf("etimeout pcb 0x%x state 0x%x\n", refp->tpr_pcb,
		refp->tpr_pcb->tp_state);
	ENDDEBUG
	IFTRACE(D_TIMER)
		tptrace(TPPTmisc, "tp_etimeout ref refstate tks Etick", refp-tp_ref,
		refp->tpr_state, ticks, tp_stat.ts_Eticks);
	ENDTRACE
	if (tpcb == 0)
		return;
	IncStat(ts_Eset);
	if (ticks == 0)
		ticks = 1;
	if (fun == TM_data_retrans) {
		tpcb->tp_retransargs.c_arg1 = arg1;
		tpcb->tp_retransargs.c_arg2 = arg2;
		tpcb->tp_retransargs.c_arg3 = arg3;
	}
	callp = tpcb->tp_refcallout + fun;
	if (Enoisy && callp->c_time)
		printf("E timer allready set: %d of ref %d\n", fun, tpcb->tp_lref);
	if (callp->c_time == 0 || callp->c_time > ticks)
		callp->c_time = ticks;
}

/*
 * CALLED FROM:
 *  tp.trans all over
 * FUNCTION and ARGUMENTS:
 *  Cancel all occurrences of E-timer function (fun) for reference (refp)
 */
void
tp_euntimeout(refp, fun)
	struct tp_ref *refp;
	int			  fun;
{
	register struct tp_pcb *tpcb = refp->tpr_pcb;

	IFTRACE(D_TIMER)
		tptrace(TPPTmisc, "tp_euntimeout ref", refp-tp_ref, 0, 0, 0);
	ENDTRACE

	if (tpcb)
		tpcb->tp_refcallout[fun].c_time = 0;
}

/*
 * CALLED FROM:
 *  tp.trans, when an incoming ACK causes things to be dropped
 *  from the retransmission queue, and we want their associated
 *  timers to be cancelled.
 *  NOTE: (by sklower) only called with TM_data_retrans.
 * FUNCTION and ARGUMENTS:
 *  cancel all occurrences of function (fun) where (arg2) < (seq)
 */
void
tp_euntimeout_lss(refp, fun, seq)
	struct tp_ref *refp;
	int			  fun;
	SeqNum		  seq;
{
	register struct tp_pcb *tpcb = refp->tpr_pcb;

	IFTRACE(D_TIMER)
		tptrace(TPPTmisc, "tp_euntimeoutLSS ref", refp-tp_ref, seq, 0, 0);
	ENDTRACE

	if (tpcb == 0 || tpcb->tp_refcallout[fun].c_time == 0)
		return;
	if (SEQ_LT(tpcb, tpcb->tp_retransargs.c_arg2, seq))  {
			IncStat(ts_Ecan_act);
			tpcb->tp_refcallout[fun].c_time = 0;
	}
}

/****************  c timers **********************
 *
 * These are not chained together; they sit
 * in the tp_ref structure. they are the kind that
 * are typically cancelled so it's faster not to
 * mess with the chains
 */

/*
 * CALLED FROM:
 *  the clock, every 500 ms
 * FUNCTION and ARGUMENTS:
 *  Look for open references with active timers.
 *  If they exist, call the appropriate timer routines to update
 *  the timers and possibly generate events.
 *  (The E timers are done in other procedures; the C timers are
 *  updated here, and events for them are generated here.)
 */
ProtoHook
tp_slowtimo()
{
	register struct Ccallout 	*cp, *cpbase;
	register struct tp_ref		*rp;
	struct tp_pcb		*tpcb;
	struct tp_event		E;
	int 				s = splnet(), t;

	/* check only open reference structures */
	IncStat(ts_Cticks);
	/* tp_ref[0] is never used */
	for (rp = tp_ref + tp_refinfo.tpr_maxopen; rp > tp_ref; rp--) {
		if ((tpcb = rp->tpr_pcb) == 0 || rp->tpr_state < REF_OPEN) 
			continue;
		cpbase = tpcb->tp_refcallout;
		t = N_CTIMERS;
		/* check the C-type timers */
		for (cp = cpbase + t; (--t, --cp) >= cpbase; ) {
			if (cp->c_time && --(cp->c_time) <= 0 ) {
				cp->c_time = 0;
				E.ev_number = t;
				if (t == TM_data_retrans) {
					register struct Ecallarg *p1 = &tpcb->tp_retransargs;
					E.ATTR(TM_data_retrans).e_low = (SeqNum) p1->c_arg1;
					E.ATTR(TM_data_retrans).e_high = (SeqNum) p1->c_arg2;
					E.ATTR(TM_data_retrans).e_retrans =  p1->c_arg3;
				}
				IFDEBUG(D_TIMER)
					printf("C expired! type 0x%x\n", t);
				ENDDEBUG
				IncStat(ts_Cexpired);
				tp_driver( rp->tpr_pcb, &E);
				if (t == TM_reference && tpcb->tp_state == TP_CLOSED) {
					if (tpcb->tp_notdetached) {
						IFDEBUG(D_CONN)
							printf("PRU_DETACH: not detached\n");
						ENDDEBUG
						tp_detach(tpcb);
					}
					/* XXX wart; where else to do it? */
					free((caddr_t)tpcb, M_PCB);
				}
			}
		}
	}
	splx(s);
	return 0;
}

/*
 * CALLED FROM:
 *  tp.trans, tp_emit()
 * FUNCTION and ARGUMENTS:
 * 	Set a C type timer of type (which) to go off after (ticks) time.
 */
void
tp_ctimeout(refp, which, ticks)
	register struct tp_ref	*refp;
	int 					which, ticks; 
{
	register struct Ccallout *cp = &(refp->tpr_callout[which]);

	IFTRACE(D_TIMER)
		tptrace(TPPTmisc, "tp_ctimeout ref which tpcb active", 
			(int)(refp - tp_ref), which, refp->tpr_pcb, cp->c_time);
	ENDTRACE
	if(cp->c_time)
		IncStat(ts_Ccan_act);
	IncStat(ts_Cset);
	if (ticks <= 0)
		ticks = 1;
	cp->c_time = ticks;
}

/*
 * CALLED FROM:
 *  tp.trans 
 * FUNCTION and ARGUMENTS:
 * 	Version of tp_ctimeout that resets the C-type time if the 
 * 	parameter (ticks) is > the current value of the timer.
 */
void
tp_ctimeout_MIN(refp, which, ticks)
	register struct tp_ref	*refp;
	int						which, ticks; 
{
	register struct Ccallout *cp = &(refp->tpr_callout[which]);

	IFTRACE(D_TIMER)
		tptrace(TPPTmisc, "tp_ctimeout_MIN ref which tpcb active", 
			(int)(refp - tp_ref), which, refp->tpr_pcb, cp->c_time);
	ENDTRACE
	IncStat(ts_Cset);
	if (cp->c_time)  {
		cp->c_time = MIN(ticks, cp->c_time);
		IncStat(ts_Ccan_act);
	} else
		cp->c_time = ticks;
}

/*
 * CALLED FROM:
 *  tp.trans
 * FUNCTION and ARGUMENTS:
 *  Cancel the (which) timer in the ref structure indicated by (refp).
 */
void
tp_cuntimeout(refp, which)
	int						which;
	register struct tp_ref	*refp;
{
	register struct Ccallout *cp;

	cp = &(refp->tpr_callout[which]);

	IFDEBUG(D_TIMER)
		printf("tp_cuntimeout(0x%x, %d) active %d\n", refp, which, cp->c_time);
	ENDDEBUG

	IFTRACE(D_TIMER)
		tptrace(TPPTmisc, "tp_cuntimeout ref which, active", refp-tp_ref, 
			which, cp->c_time, 0);
	ENDTRACE

	if (cp->c_time)
		IncStat(ts_Ccan_act);
	else
		IncStat(ts_Ccan_inact);
	cp->c_time = 0;
}
