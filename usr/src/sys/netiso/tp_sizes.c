/************************************************************
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

#define KERNEL
#define MERGED
#define IBMRTPC
#define CLNPECHO
/* #define TP_PERF_MEAS */
#define CONS
#define TPPT
#define ARGO_TP
#define ARGO_DEBUG
#define ISO
#define RDB
#define SHOW_LOAD
#define DEBUG
#define INET
#define MAXUSERS 32
#define DST 1
#define TIMEZONE 360

/* 
 * ARGO TP
 *
 * $Header: tp_sizes.c,v 5.1 88/10/12 12:21:03 root Exp $
 * $Source: /usr/argo/sys/netiso/RCS/tp_sizes.c,v $
 *
 *
 * This is the initialization and cleanup stuff - 
 * for the tp machine in general as well as  for the individual pcbs.
 * tp_init() is called at system startup.  tp_attach() and tp_getref() are
 * called when a socket is created.  tp_detach() and tp_freeref()
 * are called during the closing stage and/or when the reference timer 
 * goes off. 
 * tp_soisdisconnecting() and tp_soisdisconnected() are tp-specific 
 * versions of soisconnect*
 * and are called (obviously) during the closing phase.
 *
 */

#ifndef lint
static char *rcsid = "$Header: tp_sizes.c,v 5.1 88/10/12 12:21:03 root Exp $";
#endif lint

#include "argoxtwentyfive.h"
#include "types.h"
#include "param.h"
#include "mbuf.h"
#include "socket.h"
#include "socketvar.h"
#include "protosw.h"
#include "errno.h"
#include "time.h"
#include "tp_param.h"
#include "tp_timer.h"
#include "tp_ip.h"
#include "tp_stat.h"
#include "tp_pcb.h"
#include "tp_tpdu.h"
#include "tp_trace.h"
#include "tp_meas.h"
#include "tp_seq.h"
#include "tp_clnp.h"

#include "iso_errno.h"
#include "cons.h"
#undef IncStat
#include "cons_pcb.h"

#define DUP(x) x, x
#define SIZE(P) printf("Size of %s: 0x%x %d\n", "P", DUP(sizeof(struct P)))
#define OFF(P, Q) printf("\toffset of %s in %s: 0x%x %d\n", "P", "Q", \
		DUP(_offsetof(struct Q, P)))
main()
{
	printf( "TP struct sizes:\n");
	SIZE(tp_pcb);
#define O(y) OFF(tp_/**/y,tp_pcb);
	O(state) O(retrans) O(snduna)
	O(lref) O(fref) O(fsuffix)
	O(fsuffixlen) O(lsuffix) O(lsuffixlen)
	O(Xsnd) O(Xuna)
	SIZE(tp_ref); OFF(tpr_pcb,tp_ref);OFF(tpr_calltodo,tp_ref);
	SIZE(tp_stat); SIZE(tp_param);
	SIZE(tp_conn_param); SIZE(tp_rtc); SIZE(nl_protosw);
#ifdef TP_PERF_MEAS
	SIZE(tp_Meas);
#endif
	printf( "ISO struct sizes:\n");
	SIZE(socket);
	OFF(so_timeo,socket); OFF(so_rcv,socket); OFF(so_snd,socket);
	OFF(so_tpcb,socket); OFF(so_pcb,socket); OFF(so_qlen,socket);
	OFF(so_error,socket); OFF(so_state,socket);
	SIZE(sockbuf);
	OFF(sb_flags,sockbuf); OFF(sb_cc,sockbuf);
	OFF(sb_mb,sockbuf); OFF(sb_mbcnt,sockbuf);
	SIZE(isopcb);
	 SIZE(cons_pcb); OFF(co_state,cons_pcb);
}
