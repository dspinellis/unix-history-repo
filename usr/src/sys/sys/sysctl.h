/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)sysctl.h	7.2 (Berkeley) %G%
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
 * proc ops return arrays of augmented proc structures
 */
struct kinfo_proc {
	struct	proc kp_proc;		/* proc structure */
	struct	eproc {
		struct	proc *e_paddr;		/* address of proc */
		struct	session *e_sess;	/* session pointer */
		pid_t	e_pgid;		/* process group id */
		short	e_jobc;		/* job control counter */
		dev_t	e_tdev;		/* controlling tty */
		pid_t	e_tpgid;		/* tty process group id */
		struct	session *e_tsess;	/* tty session pointer */
#define	WMESGLEN	7
		char	e_wmesg[WMESGLEN+1];	/* wchan message */
		size_t	e_xsize;		/* text size */
		short	e_xrssize;		/* text rss */
		short	e_xccount;		/* text references */
		short	e_xswrss;
		long	e_spare[8];
	} kp_eproc;
};
/*
 * Routing table
 */
#define ki_af(x)		(((x)&0x00ff0000) >> 16)
#define KINFO_RT	(1<<8)
#define KINFO_RT_DUMP	(KINFO_RT|1)	/* dump; may limit to a.f. */
#define KINFO_RT_FLAGS	(KINFO_RT|2)	/* by flags, e.g. RESOLVING */

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
