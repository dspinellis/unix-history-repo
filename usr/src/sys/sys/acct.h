/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)acct.h	8.2 (Berkeley) %G%
 */

/*
 * Accounting structures; these use a comp_t type which is a 3 bits base 8
 * exponent, 13 bit fraction ``floating point'' number.  Units are 1/AHZ
 * seconds.
 */
typedef u_short comp_t;

struct acct {
	char	ac_comm[10];	/* command name */
	comp_t	ac_utime;	/* user time */
	comp_t	ac_stime;	/* system time */
	comp_t	ac_etime;	/* elapsed time */
	time_t	ac_btime;	/* starting time */
	uid_t	ac_uid;		/* user id */
	gid_t	ac_gid;		/* group id */
	short	ac_mem;		/* average memory usage */
	comp_t	ac_io;		/* count of IO blocks */
	dev_t	ac_tty;		/* controlling tty */
#define	AFORK	0x01			/* forked but not execed */
#define	ASU	0x02			/* used super-user permissions */
#define	ACOMPAT	0x04			/* used compatibility mode */
#define	ACORE	0x08			/* dumped core */
#define	AXSIG	0x10			/* killed by a signal */
	char	ac_flag;	/* accounting flags */
};

/*
 * 1/AHZ is the granularity of the data encoded in the comp_t fields.
 * This is not necessarily equal to hz.
 */
#define	AHZ	64

#ifdef KERNEL
struct vnode	*acctp;
#endif
