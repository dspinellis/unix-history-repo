/*	tcp_var.h	4.1	81/11/08	*/

/*
 * Kernel variables for tcp.
 */

/*
 * Tcp control block.
 */
struct tcb {
	struct tcb_hd {
		struct	th *seg_next,*seg_prev;		/* seq queue */
		struct	tcb *tcb_next,*tcb_prev;	/* other tcb's */
	} tcb_hd;
	struct	th *t_template;		/* skeletal packet for transmit */
	struct	socket *t_socket;	/* back pointer to socket */
	struct	mbuf *seg_unack;	/* unacked message queue */
	struct	host *t_host;
	short	seqcnt;
	short	xxx;
	seq_t	iss;			/* initial send seq # */
	seq_t	irs;			/* initial recv seq # */
	seq_t	rcv_urp;		/* rcv urgent pointer */
	seq_t	rcv_nxt;		/* next seq # to rcv */
	seq_t	rcv_end;		/* rcv eol pointer */
	seq_t	snd_off;		/* seq # of first datum in send buf */
	seq_t	seq_fin;		/* seq # of FIN sent */
	seq_t	snd_end;		/* send eol pointer */
	seq_t	snd_urp;		/* snd urgent pointer */
	seq_t	snd_lst;		/* seq # of last sent datum */
	seq_t	snd_nxt;		/* seq # of next datum to send */
	seq_t	snd_una;		/* seq # of first unacked datum */
	seq_t	snd_wl;			/* seq # of last sent window */
	seq_t	snd_hi;			/* highest seq # sent */
	seq_t	snd_wnd;		/* send window max */
	seq_t	t_rexmt_val;		/* val saved in rexmt timer */
	seq_t	t_rtl_val;		/* val saved in rexmt too long timer */
	seq_t	t_xmt_val;		/* seq # sent when xmt timer started */
	seq_t	rcv_adv;		/* advertised window */
	u_short	tc_flags;		/* flags and state; see below */
	u_short	t_options;
#define	TO_EOL		0x01		/* eol mode */
#define	TO_URG		0x02		/* urgent mode */
	u_short	t_lport;		/* local port */
	u_short	t_fport;		/* foreign port */
	u_char	t_state;		/* state of this connection */
	u_char	t_xmtime;		/* current rexmt time */
/* timers... must be in order */
	short	t_init;			/* initialization too long */
	short	t_rexmt;		/* retransmission */
	short	t_rexmttl;		/* retransmit too long */
	short	t_persist;		/* retransmit persistance */
	short	t_finack;		/* fin acknowledged */
	short	t_xmt;			/* round trip transmission time */
/* end timers */
};

/* tc_flags values */
#define	TC_ACK_DUE	0x0001		/* must we send ACK */
#define	TC_CANCELLED	0x0002		/* retransmit timer cancelled */
#define	TC_DROPPED_TXT	0x0004		/* dropped incoming data */
#define	TC_FIN_RCVD	0x0008		/* FIN received */
#define	TC_FORCE_ONE	0x0010		/* force sending of one byte */
#define	TC_NEW_WINDOW	0x0020		/* received new window size */
#define	TC_REXMT	0x0040		/* this msg is a retransmission */
#define	TC_SND_FIN	0x0080		/* FIN should be sent */
#define	TC_SND_RST	0x0100		/* RST should be sent */
#define	TC_SND_URG	0x0200		/* urgent data to send */
#define	TC_SYN_ACKED	0x0400		/* SYN has been ACKed */
#define	TC_SYN_RCVD	0x0800		/* SYN has been received */
#define	TC_USR_CLOSED	0x1000		/* user has closed connection */
#define	TC_WAITED_2_ML	0x2000		/* wait time for FIN ACK is up */
#define	TC_NET_KEEP	0x4000		/* don't free this net input */
#define	TC_USR_ABORT	0x8000		/* user has closed and does not expect
					   to receive any more data */
/*
 * TCP timers.
 */
#define	TINIT		0
#define	TREXMT		1
#define	TREXMTTL	2
#define	TPERSIST	3
#define	TFINACK		4
#define	TNTIMERS	5

/*
 * Tcp machine predicates
 */
#define	ack_ok(x, y) \
    (((y)->th_flags&TH_ACK)==0 || \
      ((x)->iss < (y)->t_ackno && (y)->t_ackno <= (x)->snd_hi))

#define	syn_ok(x, y) \
    ((y)->th_flags&TH_SYN)

#define	ack_fin(x, y) \
    ((x)->seq_fin > (x)->iss && (y)->t_ackno > (x)->seq_fin)

#define	rcv_empty(x) \
    (((x)->tc_flags&TC_USR_ABORT) || \
      ((x)->t_socket->so_rcv.sb_mb == NULL && \
       (x)->tcb_hd.seg_next == (x)->tcb_hd.seg_prev))

#define	ISSINCR		128		/* increment for iss each second */
#define	TCPSIZE		20		/* size of TCP leader (bytes) */

/*
 * THESE NEED TO BE JUSTIFIED!
 *
 * *2 here is because slow timeout routine called every 1/2 second.
 */
#define	T_2ML		(10*2)		/* 2*maximum packet lifetime */
#define	T_PERS		(5*2)		/* persist time */
#define	T_REXMT		(1*2)		/* base for retransmission time */
#define	T_REXMTTL	(30*2)		/* retransmit too long timeout */
#define	T_REMAX		(30*2)		/* maximum retransmission time */

#ifdef TCPDEBUG
#define	TDBSIZE		50
/*
 * Tcp debugging record.
 */
struct tcp_debug {
	long	td_tod;			/* time of day */
	struct	tcb *td_tcb;		/* -> tcb */
	char	td_old;			/* old state */
	char	td_inp;			/* input */
	char	td_tim;			/* timer id */
	char	td_new;			/* new state */
	seq_t	td_sno;			/* seq_t number */
	seq_t	td_ano;			/* acknowledgement */
	u_short	td_wno;			/* window */
	u_short	td_lno;			/* length */
	u_char	td_flg;			/* message flags */
};
#endif

#ifdef KERNEL
struct	tcb_hd tcb;			/* tcp tcb list head */
seq_t	tcp_iss;			/* tcp initial send seq # */
int	tcpconsdebug;			/* set to 1 traces on console */
#ifdef TCPDEBUG
struct	tcp_debug tcp_debug[TDBSIZE];
#endif
int	tdbx;			/* rotating index into tcp_debug */
struct	th *tcp_template();
#endif

#define	SEQ_LT(a,b)	((int)((a)-(b)) < 0)
#define	SEQ_LEQ(a,b)	((int)((a)-(b)) <= 0)
#define	SEQ_GT(a,b)	((int)((a)-(b)) > 0)
#define	SEQ_GEQ(a,b)	((int)((a)-(b)) >= 0)
struct	th *tcp_template();
