/*	tcp_var.h	4.7	81/11/24	*/

/*
 * Kernel variables for tcp.
 */

/*
 * Tcp+ip header, after ip options removed.
 */
struct tcpiphdr {
	struct 	ipovly ti_i;		/* overlaid ip structure */
	struct	tcphdr ti_t;		/* tcp header */
};
#define	ti_next		ti_i.ih_next
#define	ti_prev		ti_i.ih_prev
#define	ti_x1		ti_i.ih_x1
#define	ti_pr		ti_i.ih_pr
#define	ti_len		ti_i.ih_len
#define	ti_src		ti_i.ih_src
#define	ti_dst		ti_i.ih_dst
#define	ti_sport	ti_t.th_sport
#define	ti_dport	ti_t.th_dport
#define	ti_seq		ti_t.th_seq
#define	ti_ackno	ti_t.th_ackno
#define	ti_x2		ti_t.th_x2
#define	ti_off		ti_t.th_off
#define	ti_flags	ti_t.th_flags
#define	ti_win		ti_t.th_win
#define	ti_sum		ti_t.th_sum
#define	ti_urp		ti_t.th_urp

/*
 * TCP sequence numbers are 32 bit integers operated
 * on with modular arithmetic.  These macros can be
 * used to compare such integers.
 */
#define	SEQ_LT(a,b)	((int)((a)-(b)) < 0)
#define	SEQ_LEQ(a,b)	((int)((a)-(b)) <= 0)
#define	SEQ_GT(a,b)	((int)((a)-(b)) > 0)
#define	SEQ_GEQ(a,b)	((int)((a)-(b)) >= 0)

/*
 * Definitions of the TCP timers.  These timers are counted
 * down PR_SLOWHZ times a second.
 */
#define	TCPT_NTIMERS	7

#define	TCPT_INIT	0		/* initialization */
#define	TCPT_REXMT	1		/* retransmit */
#define	TCPT_REXMTTL	2		/* retransmit too long */
#define	TCPT_KEEP	3		/* keep alive */
#define	TCPT_KEEPTTL	4		/* keep alive too long */
#define	TCPT_PERSIST	5		/* retransmit persistance */
#define	TCPT_2MSL	6		/* 2*msl quiet time timer */

/*
 * Tcp control block.
 */
struct tcpcb {
	struct	tcpiphdr *seg_next,*seg_prev;	/* seq queue */
	short	seqcnt;			/* count of chars in seq queue */
	u_char	t_state;		/* state of this connection */
	short	t_timers[TCPT_NTIMERS];	/* tcp timers */
	u_char	t_options;		/* connection options: */
#define	TO_PUSH		0x01			/* push mode */
#define	TO_URG		0x02			/* urgent mode */
#define	TO_KEEP		0x04			/* use keep-alives */
	u_char	t_flags;
#define	TF_OWEACK	0x01			/* owe ack to peer */
	struct	mbuf *seg_unack;	/* unacked message queue */
	struct	tcpiphdr *t_template;	/* skeletal packet for transmit */
	struct	inpcb *t_inpcb;		/* back pointer to internet pcb */
/*
 * The following fields are used as in the protocol specification.
 * See RFC783, Dec. 1981, page 21.
 */
/* send sequence variables */
	tcp_seq	snd_una;		/* send unacknowledged */
	tcp_seq	snd_nxt;		/* send next */
	u_short	snd_wnd;		/* send window */
	tcp_seq	snd_up;			/* send urgent pointer */
	tcp_seq	snd_wl1;		/* window update seg seq number */
	tcp_seq	snd_wl2;		/* window update seg ack number */
	tcp_seq	iss;			/* initial send sequence number */
/* receive sequence variables */
	tcp_seq	rcv_nxt;		/* receive next */
	u_short	rcv_wnd;		/* receive window */
	tcp_seq	rcv_up;			/* receive urgent pointer */
	tcp_seq	irs;			/* initial receive sequence number */
/*
 * Additional variables for this implementation.
 */
/* send variables */
	tcp_seq	snd_off;	/*??*/	/* seq # of first datum in send buf */
	tcp_seq	seq_fin;	/*??*/	/* seq # of FIN sent */
	tcp_seq	snd_hi;		/*??*/	/* highest seq # sent */
	tcp_seq	snd_end;	/*??*/	/* send eol pointer */
	tcp_seq	snd_lst;	/*??*/	/* seq # of last sent datum */
	tcp_seq	snd_wl;		/*??*/	/* seq # of last sent window */
	tcp_seq	snd_wnd;	/*??*/	/* send window max */
/* retransmit variables */
	tcp_seq	t_rexmt_val;	/*??*/	/* val saved in rexmt timer */
	tcp_seq	t_rtl_val;	/*??*/	/* val saved in rexmt too long timer */
	tcp_seq	t_xmt_val;	/*??*/	/* seq # sent when xmt timer started */
	u_char	t_xmtime;	/*??*/	/* current rexmt time */
	short	t_xmt;		/*??*/	/* round trip transmission time */
/* receive variables */
	tcp_seq	rcv_end;	/*??*/	/* rcv eol pointer */
	tcp_seq	rcv_adv;	/*??*/	/* advertised window */
};

#define	intotcpcb(ip)	((struct tcpcb *)(ip)->inp_ppcb)
#define	sototcpcb(so)	(intotcpcb(sotoinpcb(so)))

#define	ISSINCR		128		/* increment for iss each second */
#define	TCPSIZE		20		/* size of TCP leader (bytes) */

#define	TCP_TTL		30		/* time to live for TCP segs: 30s */
/*
 * TCPSC constants give various timeouts in ``slow-clock'' ticks.
 */
#define	TCPSC_MSL	(TCP_TTL*PR_SLOWHZ)	/* max seg lifetime */
#define	TCPSC_REXMT	(1*PR_SLOWHZ)		/* base retransmit time */
#define	TCPSC_REXMTTL	(TCP_TTL*2*PR_SLOWHZ)	/* retransmit too long */
#define	TCPSC_KEEP	(TCP_TTL*4*PR_SLOWHZ)	/* keep alive */
#define	TCPSC_KEEPTTL	(4*TCPSC_KEEP)		/* keep alive too long */
#define	TCPSC_PERSIST	(5*PR_SLOWHZ)		/* retransmit persistance */
#define	TCPSC_2MSL	(TCP_TTL*2*PR_SLOWHZ)	/* 2*msl quiet time timer */

#define	TCPSC_REMAX	(TCP_TTL*PR_SLOWHZ)	/* maximum rexmt time */

struct	tcpstat {
	int	tcps_badsum;
	int	tcps_badoff;
	int	tcps_hdrops;
	int	tcps_badsegs;
	int	tcps_unack;
};

#ifdef KERNEL
tcp_seq	tcp_iss;		/* tcp initial send seq # */
struct	inpcb tcb;		/* head of queue of active tcpcb's */
struct	tcpstat tcpstat;	/* tcp statistics */
#endif
struct	tcpiphdr *tcp_template();
#endif

#ifdef	TCPTIMERS
char *tcptimers[] =
    { "INIT", "REXMT", "REXMTTL", "KEEP", "KEEPTTL", "PERSIST", "2MSL" };
#endif
