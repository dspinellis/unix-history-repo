/*
 * Copyright (c) University of British Columbia, 1984
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Laboratory for Computation Vision and the Computer Science Department
 * of the University of British Columbia.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)hd_var.h	7.4 (Berkeley) %G%
 */

/*
 *
 * hdlc control block
 *
 */

struct	hdtxq {
	struct	mbuf *head;
	struct	mbuf *tail;
};

struct	hdcb {
	struct	hdcb *hd_next;	/* pointer to next hdlc control block */
	char	hd_state;	/* link state */
	char	hd_vs;		/* send state variable */
	char	hd_vr;		/* receive state variable */
	char	hd_lastrxnr;	/* last received N(R) */
	char	hd_lasttxnr;	/* last transmitted N(R) */
	char	hd_condition;
#define TIMER_RECOVERY_CONDITION        0x01
#define REJ_CONDITION                   0x02
#define REMOTE_RNR_CONDITION            0X04
	char	hd_retxcnt;
	char	hd_xx;
	struct	hdtxq hd_txq;
	struct	mbuf *hd_retxq[MODULUS];
	char	hd_retxqi;
	char	hd_rrtimer;
	char	hd_timer;
#define SET_TIMER(hdp)		hdp->hd_timer = hd_t1
#define KILL_TIMER(hdp)		hdp->hd_timer = 0
	char	hd_dontcopy;	/* if-driver doesn't free I-frames */
	struct	ifnet *hd_ifp;	/* device's network visible interface */
	struct	ifaddr *hd_ifa;	/* device's X.25 network address */
	struct	x25config *hd_xcp;
	caddr_t	hd_pkp;		/* Level III junk */
	int	(*hd_output)();	/* separate entry for HDLC direct output */

	/* link statistics */

	long	hd_iframes_in;
	long	hd_iframes_out;
	long	hd_rrs_in;
	long	hd_rrs_out;
	short	hd_rejs_in;
	short	hd_rejs_out;
	long	hd_window_condition;
	short	hd_invalid_ns;
	short	hd_invalid_nr;
	short	hd_timeouts;
	short	hd_resets;
	short	hd_unknown;
	short	hd_frmrs_in;
	short	hd_frmrs_out;
	short	hd_rnrs_in;
	short	hd_rnrs_out;
};

#ifdef KERNEL
struct	hdcb *hdcbhead;		/* head of linked list of hdcb's */
struct	Frmr_frame hd_frmr;	/* rejected frame diagnostic info */
struct	ifqueue hdintrq;	/* hdlc packet input queue */

int	hd_t1;			/* timer T1 value */
int	hd_t3;			/* RR send timer */
int	hd_n2;			/* frame retransmission limit */
#endif
