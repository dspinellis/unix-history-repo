/*
 * Copyright (c) 1986, 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)cpu.h	7.3 (Berkeley) 6/28/90
 */

/*
 * Assorted definitions unique to TAHOE CPU support.
 */

#define	uncache(v)	mtpr(PDCS, (caddr_t)(v))
#define	movow(a,w)	_movow((u_short *)(a), (u_short)(w))
#define	movob(a,b)	_movob((u_char *)(a), (u_char)(b))

#define	resettodr()	/* no todr to set */

#define	MAXCKEY	255		/* maximal allowed code key */
#define	MAXDKEY	255		/* maximal allowed data key */
#define	NCKEY	(MAXCKEY+1)	/* # code keys, including 0 (reserved) */
#define	NDKEY	(MAXDKEY+1)	/* # data keys, including 0 (reserved) */

#ifndef LOCORE
#ifdef KERNEL
char	ckey_cache[NCKEY];	/* 1 =>'s key i may be in code cache */
short	ckey_cnt[NCKEY];	/* code key reference count */
char	dkey_cache[NDKEY];	/* 1 =>'s key i may be in data cache */
short	dkey_cnt[NDKEY];	/* data key reference count */
#endif

/*
 * Statistics maintained for code and
 * data cache key allocations algorithms.
 */
struct	keystats {
	long	ks_avail;	/* number of keys currently unallocated */
	long	ks_dirty;	/* number of keys currently waiting for purge */
	long	ks_allocs;	/* number of keys allocated */
	long	ks_allocfree;	/* key allocated from free slot */
	long	ks_norefs;	/* key marked in use, but refcnt 0 */
	long	ks_taken;	/* key taken from single process */
	long	ks_shared;	/* key taken from multiple processes */
	long	ks_inval;	/* number of times keys exchanged */
};
#endif

long	*user_psl;		/* user mode psl for ast's */
int	intenable;		/* interrupts enable startup flag */
int	clk_enable;		/* clock enable startup flag */

/*
 * Enable realtime clock.
 */
#define	enablertclock()	(clk_enable = 1)
