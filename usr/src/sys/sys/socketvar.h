/*	socketvar.h	4.8	81/11/20	*/

/*
 * Kernel structure per socket.
 * Contains send and receive buffer queues,
 * handle on protocol and pointer to protocol
 * private data and error information.
 */
struct socket {
	short	so_type;		/* generic type, see socket.h */
	short	so_options;		/* from socket call, see socket.h */
	short	so_state;		/* internal state flags SS_*, below */
	caddr_t	so_pcb;			/* protocol control block */
	struct	protosw *so_proto;	/* protocol handle */
	struct	sockbuf {
		short	sb_cc;		/* actual chars in buffer */
		short	sb_hiwat;	/* max actual char count */
		short	sb_mbcnt;	/* chars of mbufs used */
		short	sb_mbmax;	/* max chars of mbufs to use */
		short	sb_lowat;	/* low water mark (not used yet) */
		short	sb_timeo;	/* timeout (not used yet) */
		struct	mbuf *sb_mb;	/* the mbuf chain */
		struct	proc *sb_sel;	/* process selecting read/write */
		short	sb_flags;	/* flags, see below */
	} so_rcv, so_snd;
#define	SB_LOCK		0x01		/* lock on data queue (so_rcv only) */
#define	SB_WANT		0x02		/* someone is waiting to lock */
#define	SB_WAIT		0x04		/* someone is waiting for data/space */
#define	SB_SEL		0x08		/* buffer is selected */
#define	SB_COLL		0x10		/* collision selecting */
	/* need something for async wakeup */
	short	so_timeo;		/* connection timeout */
	u_short	so_error;		/* error indicator */
	struct	sockaddr so_addr;	/* socket address */
};

/*
 * Socket state bits.
 */
#define	SS_USERGONE		0x01	/* no file table ref any more */
#define	SS_ISCONNECTED		0x02	/* socket connected to a peer */
#define	SS_ISCONNECTING		0x04	/* in process of connecting to peer */
#define	SS_ISDISCONNECTING	0x08	/* in process of disconnecting */
#define	SS_CANTSENDMORE		0x10	/* can't send more data to peer */
#define	SS_CANTRCVMORE		0x20	/* can't receive more data from peer */
#define	SS_CONNAWAITING		0x40	/* connections awaiting acceptance */

/*
 * Macros for sockets and socket buffering.
 */

/* how much space is there in a socket buffer (so->so_snd or so->so_rcv) */
#define	sbspace(sb) \
    (MIN((sb)->sb_hiwat-(sb)->sb_cc, ((sb)->sb_mbmax-(sb)->sb_mbcnt)))

/* do we have to send all at once on a socket? */
#define	sosendallatonce(so) \
    (((so)->so_options & SO_NBIO) || ((so)->so_proto->pr_flags & PR_ATOMIC))

/* can we read something from so? */
#define	soreadable(so) \
    ((so)->so_rcv.sb_cc || ((so)->so_state & (SS_CANTRCVMORE|SS_CONNAWAITING)))

/* can we write something to so? */
#define	sowriteable(so) \
    (sbspace(&(so)->so_snd) > 0 || ((so)->so_state & SS_CANTSENDMORE))

/* adjust counters in sb reflecting allocation of m */
#define	sballoc(sb, m) { \
	(sb)->sb_cc += (m)->m_len; \
	(sb)->sb_mbcnt += MSIZE; \
	if ((m)->m_off > MMAXOFF) \
		(sb)->sb_mbcnt += PGSIZE; \
}

/* adjust counters in sb reflecting freeing of m */
#define	sbfree(sb, m) { \
	(sb)->sb_cc -= (m)->m_len; \
	(sb)->sb_mbcnt -= MSIZE; \
	if ((m)->m_off > MMAXOFF) \
		(sb)->sb_mbcnt -= PGSIZE; \
}

/* set lock on sockbuf sb */
#define sblock(sb) { \
	while ((sb)->sb_flags & SB_LOCK) { \
		(sb)->sb_flags |= SB_WANT; \
		sleep((caddr_t)&(sb)->sb_flags, PZERO+1); \
	} \
	(sb)->sb_flags |= SB_LOCK; \
}

/* release lock on sockbuf sb */
#define	sbunlock(sb) { \
	(sb)->sb_flags &= ~SB_LOCK; \
	if ((sb)->sb_flags & SB_WANT) { \
		(sb)->sb_flags &= ~SB_WANT; \
		wakeup((caddr_t)&(sb)->sb_flags); \
	} \
}

#define	sorwakeup(so)	sbwakeup(&(so)->so_rcv)
#define	sowwakeup(so)	sbwakeup(&(so)->so_snd)
