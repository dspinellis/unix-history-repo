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
 * $Header: tp_user.h,v 5.2 88/11/04 15:44:44 nhall Exp $
 * $Source: /usr/argo/sys/netiso/RCS/tp_user.h,v $
 *	@(#)tp_user.h	7.8 (Berkeley) 6/28/90
 *
 * These are the values a real-live user ;-) needs. 
 */

#ifndef _TYPES_
#ifdef KERNEL
#include  "../sys/types.h"
#else KERNEL
#include  <sys/types.h>
#endif KERNEL
#endif

#ifndef __TP_USER__
#define __TP_USER__

struct tp_conn_param {
	/* PER CONNECTION parameters */
	short	p_Nretrans; 
	short	p_dr_ticks;

	short	p_cc_ticks;
	short	p_dt_ticks;

	short	p_x_ticks;
	short	p_cr_ticks;

	short	p_keepalive_ticks;
	short	p_sendack_ticks;

	short	p_ref_ticks;
	short	p_inact_ticks;

	short	p_unused;	/* was .. local credit fraction reported (>0) */
	short	p_winsize;

	u_char	p_tpdusize; 	/* log 2 of size */

	u_char	p_ack_strat;	/* see comments in tp_pcb.h */
	u_char	p_rx_strat;	/* see comments in tp_pcb.h */
	u_char	p_class;	 	/* class bitmask */
	u_char	p_xtd_format;
	u_char	p_xpd_service;
	u_char	p_use_checksum;
	u_char	p_use_nxpd; 	/* netwk expedited data: not implemented */
	u_char	p_use_rcc;	/* receipt confirmation: not implemented */
	u_char	p_use_efc;	/* explicit flow control: not implemented */
	u_char	p_no_disc_indications;	/* don't deliver indic on disc */
	u_char	p_dont_change_params;	/* use these params as they are */
	u_char	p_netservice;
	u_char	p_version;	/* only here for checking */
};

/*
 * These sockopt level definitions should be considered for socket.h
 */
#define	SOL_TRANSPORT	0xfffe
#define	SOL_NETWORK	0xfffd

/* get/set socket opt commands */
#define		TPACK_WINDOW	0x0 /* ack only on full window */
#define		TPACK_EACH		0x1 /* ack every packet */

#define		TPRX_USE_CW		0x8 /* use congestion window transmit */
#define		TPRX_EACH		0x4 /* retrans each packet of a set */
#define		TPRX_FASTSTART	0x1 /* don't use slow start */

#define TPOPT_INTERCEPT		0x200
#define TPOPT_FLAGS			0x300
#define TPOPT_CONN_DATA		0x400 
#define TPOPT_DISC_DATA		0x500 
#define TPOPT_CFRM_DATA		0x600 
#define TPOPT_CDDATA_CLEAR	0x700 
#define TPOPT_PERF_MEAS		0xa00
#define TPOPT_PSTATISTICS	0xb00
#define TPOPT_PARAMS		0xc00 /* to replace a bunch of the others */
#define TPOPT_MY_TSEL		0x800 
#define TPOPT_PEER_TSEL		0x900 
#define TPOPT_NGC8_ACCEPT	0xd00 /* negotiate connection requests */

/* 
 ***********************flags**********************************
 */

/* read only flags */
#define TPFLAG_DISC_DATA_OUT	(u_char)0x10 /* disc data present */
#define TPFLAG_DISC_DATA_IN		(u_char)0x20 /* disc data present */
#define TPFLAG_CONN_DATA_OUT	(u_char)0x40 /* conn data present */
#define TPFLAG_CONN_DATA_IN		(u_char)0x80 /* conn data present */
#define TPFLAG_XPD_PRESENT		(u_char)0x08 /* xpd data present */
#define TPFLAG_PEER_ON_SAMENET	(u_char)0x02
#define TPFLAG_NLQOS_PDN		(u_char)0x01
#define TPFLAG_NGC8_ACCEPT		(u_char)0x04 /* negotiate conn rq's */


/* 
 ***********************end flags******************************
 */


#endif __TP_USER__
