/*	if_imphost.h	4.4	82/02/21	*/

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
	struct	in_addr h_addr;		/* host's address */
	short	h_qcnt;          	/* size of holding q */
	u_char	h_rfnm;			/* # outstanding rfnm's */
	u_char	h_refcnt;		/* reference count */
};

/*
 * Host structures, as seen inside an mbuf.
 * Hashing on the host address is used to
 * select an index into the first mbuf.  Collisions
 * are then resolved by searching successive
 * mbuf's at the same index.  Reclamation is done
 * automatically at the time a structure is free'd.
 */
#define	HPMBUF	((MLEN - sizeof(int)) / sizeof(struct host))
#if vax
#define	HOSTHASH(a)	((((a).s_addr>>8)+(a).s_net) % HPMBUF)
#endif

/*
 * In-line expansions for queuing operations on
 * host message holding queue.  Queue is maintained
 * as circular list with the head pointing to the
 * last message in the queue.
 */
#define	HOST_ENQUE(hp, m) { \
	register struct mbuf *n; \
	hp->h_qcnt++; \
	if ((n = hp->h_q) == 0) \
		hp->h_q = m->m_act = m; \
	else { \
		m->m_act = n->m_act; \
		hp->h_q = n->m_act = m; \
	} \
}
#define	HOST_DEQUE(hp, m) { \
	if (m = hp->h_q) { \
		if (m->m_act == m) \
			hp->h_q = 0; \
		else { \
			m = m->m_act; \
			hp->h_q->m_act = m->m_act; \
		} \
		hp->h_qcnt--; \
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
