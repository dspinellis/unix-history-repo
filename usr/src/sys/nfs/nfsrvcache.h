/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nfsrvcache.h	7.4 (Berkeley) %G%
 */

/*
 * Definitions for the server recent request cache
 */

#define	NFSRVCACHESIZ	256
#define	NFSRCHSZ	256

struct nfsrvcache {
	struct	nfsrvcache *rc_chain[2];	/* Hash chain links */
	struct	nfsrvcache *rc_lchain[2];	/* Lru list */
	u_long	rc_xid;				/* rpc id number */
	time_t	rc_timestamp;			/* Time stamp */
	union {
		struct mbuf *ru_repmb;		/* Reply mbuf list OR */
		int ru_repstat;			/* Reply status */
	} rc_un;
	union nethostaddr rc_haddr;		/* Host address */
	short	rc_proc;			/* rpc proc number */
	u_char	rc_state;		/* Current state of request */
	u_char	rc_flag;		/* Flag bits */
};

#define	rc_forw		rc_chain[0]
#define	rc_back		rc_chain[1]
#define	rc_next		rc_lchain[0]
#define	rc_prev		rc_lchain[1]
#define	rc_reply	rc_un.ru_repmb
#define	rc_status	rc_un.ru_repstat
#define	rc_inetaddr	rc_haddr.had_inetaddr
#define	rc_nam		rc_haddr.had_nam

/* Cache entry states */
#define	RC_UNUSED	0
#define	RC_INPROG	1
#define	RC_DONE		2

/* Return values */
#define	RC_DROPIT	0
#define	RC_REPLY	1
#define	RC_DOIT		2

/* Flag bits */
#define	RC_LOCKED	0x01
#define	RC_WANTED	0x02
#define	RC_REPSTATUS	0x04
#define	RC_REPMBUF	0x08
#define	RC_NQNFS	0x10
#define	RC_INETADDR	0x20
#define	RC_NAM		0x40

/* Delay time after completion that request is dropped */
#define	RC_DELAY	2		/* seconds */

