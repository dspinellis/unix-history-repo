/*	tcp_var.h	4.9	81/11/26	*/

/*
 * Kernel variables for tcp.
 */

/*
 * Tcp control block.
 */
struct tcpcb {
	struct	tcpiphdr *seg_next;	/* sequencing queue */
	struct	tcpiphdr *seg_prev;
	int	t_state;		/* state of this connection */
	int	t_seqcnt;		/* count of chars in seq queue */
	short	t_timer[TCPT_NTIMERS];	/* tcp timers */
	struct	mbuf *t_tcpopt;		/* tcp options */
	struct	mbuf *t_ipopt;		/* ip options */
	short	t_maxseg;		/* maximum segment size */
	u_char	t_flags;
#define	TF_ACKNOW	0x01			/* ack peer immediately */
#define	TF_DELACK	0x02			/* ack, but try to delay it */
#define	TF_PUSH		0x04			/* push mode */
#define	TF_URG		0x08			/* urgent mode */
#define	TF_KEEP		0x10			/* use keep-alives */
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
/* receive variables */
	tcp_seq	rcv_adv;		/* advertised window */
/* retransmit variables */
	tcp_seq	snd_max;		/* highest sequence number sent
					   used to recognize retransmits */
	tcp_seq	rxt_seq;
	short	rxt_time;
	short	rxt_cnt;
};

#define	tcp_finisacked(tp)	0		/* XXX */

#define	intotcpcb(ip)	((struct tcpcb *)(ip)->inp_ppcb)
#define	sototcpcb(so)	(intotcpcb(sotoinpcb(so)))

#define	TCP_ISSINCR	128		/* increment for iss each second */

struct	tcpstat {
	int	tcps_badsum;
	int	tcps_badoff;
	int	tcps_hdrops;
	int	tcps_badsegs;
	int	tcps_unack;
};

#ifdef KERNEL
struct	inpcb tcb;		/* head of queue of active tcpcb's */
struct	tcpstat tcpstat;	/* tcp statistics */
struct	tcpiphdr *tcp_template();
#endif
