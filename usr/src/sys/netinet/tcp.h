/* tcp.h 1.10 81/10/30 */

/*
 * Tcp header.  Fits over the ip header after option removed.
 *
 * SHOULD MAKE A CLEAN HEADER FOR USE BY USERS.
 */
struct th {
	struct	th *t_next;		/* -> next tcp on rcv chain */
	struct	th *t_prev;		/* -> prev tcp on rcv chain */
	u_char	t_x1;			/* (unused) */
	u_char	t_pr;			/* protocol */
/* by rights, t_len should be a u_short, but this makes operations */
/* on it very dangerous as comparisons become unsigned and comparing */
/* against negative numbers then fails... we don't expect any > 32767 */
/* byte segments, so pragmatically delcare it to be a short */
	short	t_len;			/* seg length */
	struct	socket t_s;		/* source internet address */
	struct	socket t_d;		/* destination internet address */
	u_short	t_src;			/* source port */
	u_short	t_dst;			/* destination port */
	seq_t	t_seq;			/* sequence number */
	seq_t	t_ackno;		/* acknowledgement number */
#define	t_end(x) (x->t_seq + x->t_len - 1)
	u_char
		t_x2:4,			/* (unused) */
		t_off:4;		/* data offset */
	u_char	th_flags;
#define	TH_FIN	001
#define	TH_SYN	002
#define	TH_RST	004
#define	TH_EOL	010
#define	TH_ACK	020
#define	TH_URG	040
	u_short	t_win;			/* window */
	u_short	t_sum;			/* checksum */
	u_short	t_urp;			/* urgent pointer */
};

/*
 * Tcp control block.
 */
struct tcb {
	struct	th *t_rcv_next;		/* first el on rcv queue */
	struct	th *t_rcv_prev;		/* last el on rcv queue */
	struct	tcb *t_tcb_next;	/* next tcb */
	struct	tcb *t_tcb_prev;	/* next tcb */
	struct	ucb *t_ucb;		/* ucb */
	struct	mbuf *t_rcv_unack;	/* unacked message queue */
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

	/* various flags and state variables */

	u_short	tc_flags;
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

	u_short	t_lport;		/* local port */
	u_short	t_fport;		/* foreign port */
	u_char	t_state;		/* state of this connection */
	u_char	t_xmtime;		/* current rexmt time */

	/* timers */

	u_char	t_init;			/* initialization too long */
	u_char	t_rexmt;		/* retransmission */
	u_char	t_rexmttl;		/* retransmit too long */
	u_char	t_persist;		/* retransmit persistance */
	u_char	t_finack;		/* fin acknowledged */
	u_char	t_xmt;			/* round trip transmission time */

	seq_t	rcv_adv;		/* advertised window */
};

/*
 * TCP timers.
 */
#define	TINIT		1
#define	TREXMT		2
#define	TREXMTTL	3
#define	TPERSIST	4
#define	TFINACK		5

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
      ((x)->t_ucb->uc_rbuf == NULL && (x)->t_rcv_next == (x)->t_rcv_prev))

/*
 * THESE NEED TO BE JUSTIFIED!
 */
#define	ISSINCR		128		/* increment for iss each second */
#define	TCPROTO		6		/* TCP-4 protocol number */
#define	TCPSIZE		20		/* size of TCP leader (bytes) */
#define	T_2ML		10		/* 2*maximum packet lifetime */
#define	T_PERS		5		/* persist time */
#define	T_INIT		30		/* init too long timeout */
#define	T_REXMT		1		/* base for retransmission time */
#define	T_REXMTTL	30		/* retransmit too long timeout */
#define	T_REMAX		30		/* maximum retransmission time */
#define	ACTIVE		1		/* active open */
#define	PASSIVE		0		/* passive open */

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
struct	tcb *tcb_head, *tcb_tail;	/* tcp tcb list */
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
