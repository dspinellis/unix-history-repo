/* socket.h 4.3 81/11/07 */

/*
 * User connection block
 */
struct ucb {
	struct	host *uc_host;		/* foreign host table entry */
	struct	proc *uc_proc;		/* user proc */
	caddr_t	uc_pcb;			/* protocol control block */
	struct	protosw *uc_proto;	/* protocol handle */
/* sbuf and rbuf should be uc_s and uc_r which are structures with */
/* fields: s_buf, s_mbmax, s_mbcnt, s_cc, s_ccmax */
	struct	mbuf *uc_sbuf;		/* user send buffer */
	u_char	uc_snd;			/* # send bufs allocated */
	u_char	uc_ssize;		/* # bufs on send buffer */
	short	uc_scc;		/* not used yet */
	short	uc_shiwat;	/* not used yet */
	struct	mbuf *uc_rbuf;		/* user receive buffer */
	u_char	uc_rcv;		/* not used now */
	u_char	uc_rsize;	/* not used now */
	short	uc_rcc;
	short	uc_rhiwat;
	short	uc_timeo;		/* open timeout */
	u_char	uc_state;		/* state of this connection */
	u_short	uc_flags;		/* misc. flags (see below) */
	struct	proc *uc_rsel;		/* read selecting proc */
};

/* uc_flags field definitions */

/* these belong within TCP */
#define	UEOL		00001		/* EOL sent */
#define	UURG		00002		/* urgent data sent */
/* end belong in TCP */
#define	UDEBUG		00004		/* turn on debugging info recording */
#define	UTCP		00020		/* SHOULD BE IMPLIED BY uc_proto */

#define	ULISTEN		00200		/* awaiting a connection */

#define	URCOLL		01000		/* someone collided on read select */
#define	URLOCK		02000		/* for uc_rbuf */
#define	URWANT		04000

/* connection state field */
#define	UCLOSED		0000		/* connection closed */
#define	UCLSERR		0001		/* error -- connection closing */
#define	UABORT		0002		/* connection aborted */
#define	UINTIMO		0004		/* open failed -- init timeout */
#define	URXTIMO		0010		/* retransmit too long timeout */
#define	URESET		0020		/* connection aborted due to reset */
#define	UOPERR		0040		/* open failed -- not enough buffers */
#define	UURGENT		0100		/* urgent data received */
#define	UNETDWN		0200		/* connection aborted due to net */

#ifdef KERNEL
struct	ucb *contab, *conNCON;
int	nnetcon;
#endif
