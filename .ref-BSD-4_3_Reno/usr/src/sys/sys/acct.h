/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)acct.h	7.2 (Berkeley) 5/9/89
 */

/*
 * Accounting structures;
 * these use a comp_t type which is a 3 bits base 8
 * exponent, 13 bit fraction ``floating point'' number.
 * Units are 1/AHZ seconds.
 */
typedef	u_short comp_t;

struct	acct
{
	char	ac_comm[10];		/* Accounting command name */
	comp_t	ac_utime;		/* Accounting user time */
	comp_t	ac_stime;		/* Accounting system time */
	comp_t	ac_etime;		/* Accounting elapsed time */
	time_t	ac_btime;		/* Beginning time */
	uid_t	ac_uid;			/* Accounting user ID */
	gid_t	ac_gid;			/* Accounting group ID */
	short	ac_mem;			/* average memory usage */
	comp_t	ac_io;			/* number of disk IO blocks */
	dev_t	ac_tty;			/* control typewriter */
	char	ac_flag;		/* Accounting flag */
};

#define	AFORK	0001		/* has executed fork, but no exec */
#define	ASU	0002		/* used super-user privileges */
#define	ACOMPAT	0004		/* used compatibility mode */
#define	ACORE	0010		/* dumped core */
#define	AXSIG	0020		/* killed by a signal */

/*
 * 1/AHZ is the granularity of the data encoded in the various
 * comp_t fields.  This is not necessarily equal to hz.
 */
#define AHZ 64

#ifdef KERNEL
struct	vnode	*acctp;
#endif
