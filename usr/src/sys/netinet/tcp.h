/* tcp.h 1.3 81/10/21 */

/*
 * Tcp header (fits over ip header).
 */
struct th {
	struct	th *t_next;		/* -> next tcp on rcv chain */
	struct	th *t_prev;		/* -> prev tcp on rcv chain */
	u_char	t_x1;			/* (unused) */
	u_char	t_pr;			/* protocol */
	u_short	t_len;			/* seg length */
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
	u_char
		t_fin:1,		/* fin flag */
		t_syn:1,		/* syn flag */
		t_rst:1,		/* reset flag */
		t_eol:1,		/* eol flag */
		t_ack:1,		/* ack flag */
		t_urg:1,		/* urgent flag */
		t_x3:2;			/* (unused) */
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

	u_short
		ack_due:1,		/* must we send ACK */
		cancelled:1,		/* retransmit timer cancelled */
		dropped_txt:1,		/* dropped incoming data */
		fin_rcvd:1,		/* FIN received */
		force_one:1,		/* force sending of one byte */
		new_window:1,		/* received new window size */
		rexmt:1,		/* this msg is a retransmission */
		snd_fin:1,		/* FIN should be sent */
		snd_rst:1,		/* RST should be sent */
		snd_urg:1,		/* urgent data to send */
		syn_acked:1,		/* SYN has been ACKed */
		syn_rcvd:1,		/* SYN has been received */
		usr_closed:1,		/* user has closed connection */
		waited_2_ml:1,		/* wait time for FIN ACK is up */
		net_keep:1,		/* don't free this net input */
		usr_abort:1;		/* user has closed and does not expect
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
};

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

/*
 * Tcp debugging record.
 */
struct t_debug {
	long	t_tod;			/* time of day */
	struct	tcb *t_tcb;		/* -> tcb */
	char	t_old;			/* old state */
	char	t_inp;			/* input */
	char	t_tim;			/* timer id */
	char	t_new;			/* new state */
	seq_t	t_sno;			/* seq_t number */
	seq_t	t_ano;			/* acknowledgement */
	u_short	t_wno;			/* window */
	u_short	t_lno;			/* length */
	u_char	t_flg;			/* message flags */
};

/*
 * Tcp machine predicates
 */
#define	ack_ok(x, y) \
    (!(y)->t_ack || ((x)->iss < (y)->t_ackno && (y)->t_ackno <= (x)->snd_hi))

#define	syn_ok(x, y) \
    ((y)->t_syn)

#define	ack_fin(x, y) \
    ((x)->seq_fin > (x)->iss && (y)->t_ackno > (x)->seq_fin)

#define	rcv_empty(x) \
    ((x)->usr_abort || \
      ((x)->t_ucb->uc_rbuf == NULL && (x)->t_rcv_next == (x)->t_rcv_prev))
