/* socketvar.h 4.2 81/11/08 */

/*
 * Kernel structure per socket.
 * Contains send and receive buffer queues,
 * handle on protocol and pointer to protocol
 * private data and error information.
 */
struct socket {
	short	so_type;		/* generic type, see socket.h */
	short	so_options;		/* from socket call, see socket.h */
	caddr_t	so_pcb;			/* protocol control block */
	struct	protosw *so_proto;	/* protocol handle */
	struct	sockbuf {
		short	sb_cc;		/* characters in buffer */
		short	sb_hiwat;	/* max chars for buffer */
		short	sb_mbcnt;	/* # mbufs in use */
		short	sb_mbmax;	/* max # mbufs to use */
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
};

/*
 * Option bits and socket types are defined in socket.h.
 */
