/*-
 * Copyright (c) 1992 Keith Muller.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sel_subs.h	8.1 (Berkeley) %G%
 */

/*
 * data structure for storing uid/grp selects (-U, -G non standard options)
 */

#define USR_TB_SZ	317		/* user selection table size */
#define GRP_TB_SZ	317		/* user selection table size */

typedef struct usrt {
	uid_t uid;
	struct usrt *fow;		/* next uid */
} USRT;

typedef struct grpt {
	gid_t gid;
	struct grpt *fow;		/* next gid */
} GRPT;

/*
 * data structure for storing user supplied time ranges (-T option)
 */

#define ATOI2(s)	((((s)[0] - '0') * 10) + ((s)[1] - '0'))

typedef struct time_rng {
	time_t		low_time;	/* lower inclusive time limit */
	time_t		high_time;	/* higher inclusive time limit */
	int		flgs;		/* option flags */
#define	HASLOW		0x01		/* has lower time limit */
#define HASHIGH		0x02		/* has higher time limit */
#define CMPMTME		0x04		/* compare file modification time */
#define CMPCTME		0x08		/* compare inode change time */
#define CMPBOTH	(CMPMTME|CMPCTME)	/* compare inode and mod time */
	struct time_rng	*fow;		/* next pattern */
} TIME_RNG;
