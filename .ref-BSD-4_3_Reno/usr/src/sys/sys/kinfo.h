/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
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
 *	@(#)kinfo.h	7.4 (Berkeley) 6/28/90
 */

/*
 * Get kernel info
 */

#define ki_op(x)		((x)&0x0000ffff)
#define ki_type(x)		((x)&0x0000ff00)

/* 
 * proc info 
 */
#define	KINFO_PROC		(0<<8)
#define KINFO_PROC_ALL	(KINFO_PROC|0)	/* everything */
#define	KINFO_PROC_PID	(KINFO_PROC|1)	/* by process id */
#define	KINFO_PROC_PGRP	(KINFO_PROC|2)	/* by process group id */
#define	KINFO_PROC_SESSION (KINFO_PROC|3)	/* by session of pid */
#define	KINFO_PROC_TTY	(KINFO_PROC|4)	/* by controlling tty */
#define	KINFO_PROC_UID	(KINFO_PROC|5)	/* by effective uid */
#define	KINFO_PROC_RUID	(KINFO_PROC|6)	/* by real uid */

/*
 * Routing table
 */
#define ki_af(x)		(((x)&0x00ff0000) >> 16)
#define KINFO_RT		(1<<8)
#define KINFO_RT_DUMP	(KINFO_RT|1)	/* dump; may limit to a.f. */
#define KINFO_RT_FLAGS	(KINFO_RT|2)	/* by flags, e.g. RESOLVING */

/*
 * vnodes
 */
#define	KINFO_VNODE		(2<<8)

/*
 * Locking and stats
 */

struct kinfo_lock {
	int	kl_lock;
	int	kl_want;
	int	kl_locked;
};

#ifdef KERNEL
extern struct kinfo_lock kinfo_lock;
#endif
