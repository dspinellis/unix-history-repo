/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)bsd_audiovar.h	7.1 (Berkeley) %G%
 *
 * from: $Header: bsd_audiovar.h,v 1.4 92/07/03 23:23:12 mccanne Exp $ (LBL)
 */

#define AUCB_SIZE 4096
#define AUCB_MOD(k)	((k) & (AUCB_SIZE - 1))

#define AUCB_INIT(cb)	((cb)->cb_head = (cb)->cb_tail = (cb)->cb_drops = \
			 (cb)->cb_pdrops = 0)

#define AUCB_EMPTY(cb)	((cb)->cb_head == (cb)->cb_tail)
#define AUCB_FULL(cb)	(AUCB_MOD((cb)->cb_tail + 1) == (cb)->cb_head)
#define AUCB_LEN(cb)	(AUCB_MOD((cb)->cb_tail - (cb)->cb_head))

#define MAXBLKSIZE (AUCB_SIZE / 2)
#define DEFBLKSIZE 128

#ifndef LOCORE
/*
 * aucb's are used for communication between the trap handler and
 * the software interrupt.
 */
struct aucb {
	int	cb_head;		/* queue head */
	int	cb_tail;		/* queue tail */
	int	cb_thresh;		/* threshold for wakeup */
	u_short	cb_waking;		/* needs wakeup at softint level */
	u_short	cb_pause;		/* io paused */
	u_long	cb_drops;		/* missed samples from over/underrun */
	u_long	cb_pdrops;		/* sun compat -- paused samples */
	u_char	cb_data[AUCB_SIZE];	/* data buffer */
};

#if !defined(__STDC__) && !defined(volatile)
#define volatile
#endif

struct auio {
	volatile struct amd7930 *au_amd;/* chip registers */
	u_long	au_stamp;		/* time stamp */
	int	au_lowat;		/* xmit low water mark (for wakeup) */
	int	au_hiwat;		/* xmit high water mark (for wakeup) */
	int	au_blksize;		/* recv block (chunk) size */
	struct	aucb au_rb;		/* read (recv) buffer */
	struct	aucb au_wb;		/* write (xmit) buffer */
};
#endif
