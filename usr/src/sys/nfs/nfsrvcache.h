/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
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
 *	@(#)nfsrvcache.h	7.2 (Berkeley) %G%
 */

/*
 * Definitions for the server recent request cache
 */

#define	NFSRVCACHESIZ	128
#define	NFSRCHSZ	32

struct nfsrvcache {
	struct	nfsrvcache *rc_chain[2];	/* Hash chain links */
	struct	nfsrvcache *rc_next;	/* Lru list */
	struct	nfsrvcache *rc_prev;
	int	rc_state;		/* Current state of request */
	int	rc_flag;		/* Flag bits */
	struct	mbuf rc_nam;		/* Sockaddr of requestor */
	u_long	rc_xid;			/* rpc id number */
	int	rc_proc;		/* rpc proc number */
	long	rc_timestamp;		/* Time stamp */
	union {
		struct mbuf *rc_repmb;	/* Reply mbuf list OR */
		int rc_repstat;		/* Reply status */
	} rc_un;
};

#define	rc_forw		rc_chain[0]
#define	rc_back		rc_chain[1]
#define	rc_status	rc_un.rc_repstat
#define	rc_reply	rc_un.rc_repmb

#define	put_at_head(rp) \
		(rp)->rc_prev->rc_next = (rp)->rc_next; \
		(rp)->rc_next->rc_prev = (rp)->rc_prev; \
		(rp)->rc_next = nfsrvcachehead.rc_next; \
		(rp)->rc_next->rc_prev = (rp); \
		nfsrvcachehead.rc_next = (rp); \
		(rp)->rc_prev = &nfsrvcachehead

/* Cache entry states */
#define	RC_UNUSED	0
#define	RC_INPROG	1
#define	RC_DONE		2

/* Return values */
#define	RC_DROPIT	0
#define	RC_REPLY	1
#define	RC_DOIT		2

/* Flag bits */
#define	RC_LOCKED	0x1
#define	RC_WANTED	0x2
#define	RC_REPSTATUS	0x4
#define	RC_REPMBUF	0x8

/* Delay time after completion that request is dropped */
#define	RC_DELAY	2		/* seconds */

