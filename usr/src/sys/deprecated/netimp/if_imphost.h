/*
 * Copyright (c) 1982,1986,1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)if_imphost.h	7.3 (Berkeley) %G%
 */

/*
 * Host structure used with IMP's.
 * Used to hold outgoing packets which
 * would exceed allowed RFNM count.
 *
 * These structures are packed into
 * mbuf's and kept as small as possible.
 */
struct host {
	struct	mbuf *h_q;		/* holding queue */
	u_short	h_timer;		/* used to stay off deletion */
	u_short	h_imp;			/* host's imp number */
	u_char	h_host;			/* host's number on imp */
	u_char	h_qcnt;          	/* size of holding q */
	u_char	h_rfnm;			/* # outstanding rfnm's */
	u_char	h_flags;		/* see below */
};

/*
 * A host structure is kept around (even when there are no
 * references to it) for a spell to avoid constant reallocation
 * and also to reflect IMP status back to sites which aren't
 * directly connected to the IMP.  When structures are marked
 * free, a timer is started; when the timer expires the structure
 * is deallocated.  A structure may be reused before the timer expires.
 * A structure holds a reference on the containing mbuf when it is marked
 * HF_INUSE or its timer is running.
 */
#define	HF_INUSE	0x1
#define	HF_DEAD		(1<<IMPTYPE_HOSTDEAD)
#define	HF_UNREACH	(1<<IMPTYPE_HOSTUNREACH)

#define	HOSTTIMER	128		/* keep structure around awhile */

/*
 * Mark a host structure free
 */
#define	hostfree(hp) { \
	(hp)->h_flags &= ~HF_INUSE; \
	(hp)->h_timer = HOSTTIMER; \
}

/*
 * Host structures, as seen inside an mbuf.
 * Hashing on the host and imp is used to
 * select an index into the first mbuf.  Collisions
 * are then resolved by searching successive
 * mbuf's at the same index.  Reclamation is done
 * automatically at the time a structure is freed.
 */
#define	HPMBUF	((MLEN - sizeof(int)) / sizeof(struct host))
#if defined(notdef) && BYTE_ORDER == BIG_ENDIAN
#define	HOSTHASH(imp, host)	(((imp)+(host)) % HPMBUF)
#else
#define	HOSTHASH(imp, host)	((ntohs(imp)+(host)) % HPMBUF)
#endif

/*
 * In-line expansions for queuing operations on
 * host message holding queue.  Queue is maintained
 * as circular list with the head pointing to the
 * last message in the queue.
 */
#define	HOST_ENQUE(hp, m) { \
	register struct mbuf *n; \
	(hp)->h_qcnt++; \
	if ((n = (hp)->h_q) == 0) \
		(hp)->h_q = (m)->m_act = (m); \
	else { \
		(m)->m_act = n->m_act; \
		(hp)->h_q = n->m_act = (m); \
	} \
}
#define	HOST_DEQUE(hp, m) { \
	if ((m) = (hp)->h_q) { \
		if ((m)->m_act == (m)) \
			(hp)->h_q = 0; \
		else { \
			(m) = (m)->m_act; \
			(hp)->h_q->m_act = (m)->m_act; \
		} \
		(hp)->h_qcnt--; \
		(m)->m_act = 0; \
	} \
}

struct hmbuf {
	int	hm_count;		/* # of struct's in use */
	struct	host hm_hosts[HPMBUF];	/* data structures proper */
};

#ifdef KERNEL
struct host *hostlookup();
struct host *hostenter();
struct mbuf *hostdeque();
#endif
