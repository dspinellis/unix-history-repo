/* socket.h 4.1 81/10/29 */

/*
 * User connection block
 */
struct ucb {
	struct	host *uc_host;		/* foreign host table entry */
	struct	proc *uc_proc;		/* user proc */
	union {				/* protocol control block */
		char *unull;			/* general */
		struct tcb *utcb;		/* for tcp */
	} U_cp;
#define	uc_tcb	U_cp.utcb
	struct	mbuf *uc_sbuf;		/* user send buffer */
	struct	mbuf *uc_rbuf;		/* user receive buffer */
	u_char	uc_lolink;		/* lowest link no. in range (raw) */
	u_char	uc_hilink;		/* highest link no. in range (raw) */
	u_char	uc_snd;			/* # send bufs allocated */
	u_char	uc_ssize;		/* # bufs on send buffer */
#define	uc_timeo uc_ssize		/* user timeout parameter */
	short	uc_rhiwat;
	short	uc_rcc;
	u_char	uc_state;		/* state of this connection */
	u_short	uc_flags;		/* misc. flags (see below) */
	struct	proc *uc_rsel;		/* read selecting proc */
	struct	th *uc_template;
};
struct	th *tcp_template();

/* uc_flags field definitions */
#define	UEOL		00001		/* EOL sent */
#define	UURG		00002		/* urgent data sent */
#define	UDEBUG		00004		/* turn on debugging info recording */
#define	UTCP		00020		/* this is a TCP connection */
#define	UIP		00040		/* this is a raw IP connection */
#define	URAW		00100		/* this is a raw 1822 connection */
#define	ULISTEN		00200		/* awaiting a connection */
#define	UCTL		00400		/* this is a control port only */
#define	URMSK		00560
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
