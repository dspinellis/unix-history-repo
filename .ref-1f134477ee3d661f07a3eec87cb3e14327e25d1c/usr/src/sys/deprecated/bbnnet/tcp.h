#define RCSTCPHDR	"$Header: tcp.h,v 1.19 85/07/31 09:33:34 walsh Exp $"

struct th {                     /* tcp header (fits over ip header) */
	struct th	*t_next;	/* -> next tcp on rcv chain */
	struct th	*t_prev;	/* -> prev tcp on rcv chain */
	u_char		 t_x1;		/* (unused) */
	u_char		 t_pr;		/* protocol */
	u_short		 t_len;		/* seg length */
	struct in_addr	 t_s;		/* source internet address */
	struct in_addr	 t_d;		/* destination internet address */
	u_short		 t_src;		/* source port */
	u_short		 t_dst;		/* destination port */
	sequence	 t_seq;		/* sequence number */
	sequence	 t_ackno;	/* acknowledgement number */
#define t_end(x) (x->t_seq + x->t_len - 1)
	u_char
		t_x2:4,                 /* (unused) */
		t_off:4;                /* data offset */
#define TCP_OFFSHIFT 2
	u_char		 t_flags;		
#define T_FIN	0x01			/* fin flag */
#define T_SYN	0x02			/* syn flag */
#define T_RST	0x04			/* reset flag */
#define T_PUSH	0x08			/* push flag */
#define T_ACK	0x10			/* ack flag */
#define T_URG	0x20			/* urgent flag */
	u_short		 t_win;		/* window */
	u_short		 t_sum;		/* checksum */
	u_short		 t_urp;		/* urgent pointer */
};

#define TCP_END_OPT	0		/* end of option list */
#define TCP_NOP_OPT	1		/* nop option */
#define TCP_MAXSEG_OPT	2		/* maximum segment size option */
#define	TCP_MAXSEG_OPTLEN 4		/* max seg option length */
#define	TCP_MAXSEG_OPTHDR ((TCP_MAXSEG_OPT<<8)|TCP_MAXSEG_OPTLEN)

typedef u_char tcptimerval;		/* in 0.5 second units */
#define MAX_TCPTIMERVAL	255

struct tcpcb {                    /* tcp control block */

	/* various pointers */

				/* where store data until gets to socket */
	struct th *t_rcv_next;          /* -> first el on rcv queue */
	struct th *t_rcv_prev;          /* -> last el on rcv queue */
	int	   t_rcv_len;		/* length of rcv queue */

	struct inpcb *t_in_pcb;		/* -> in_pcb */
	struct mbuf *t_rcv_unack;       /* -> unacked message queue */
					/* ### how about a tail pointer */

	/* sequence number variables */

	sequence iss;                   /* initial send seq # */
	sequence irs;                   /* initial recv seq # */
	sequence rcv_urp;               /* rcv urgent pointer */
	sequence rcv_nxt;               /* next contiguous seq # to rcv */
	sequence seq_fin;               /* seq # of FIN sent */
	sequence snd_end;               /* send eol pointer. end of PUSH */
	sequence snd_urp;               /* snd urgent pointer. end of URG */
	sequence snd_lst;               /* seq # of last datum to send */
	sequence snd_nxt;               /* seq # of next datum to send */
	sequence snd_una;               /* seq # of first unacked datum */
	sequence snd_wl;                /* seq # of last sent window */
	sequence snd_hi;                /* highest seq # we sent */
	sequence t_xmt_val;             /* seq # measuring round trip time of */

	/* various flags and state variables
	 * At one time booleans were a bitfield, but since are using mbufs,
	 * have space and is quicker to test/set byte than bit.
	 */

	char	ack_due;		/* must we send ACK */
	char	cancelled;		/* retransmit timer cancelled */
	char	dropped_txt;		/* dropped incoming data */
	char	fin_rcvd;		/* FIN received */
	char	force_one;		/* force sending of one byte */
	char	new_window;		/* received new window size */
	char	rexmt;			/* this msg is a retransmission */
	char	snd_fin;		/* FIN should be sent */
	char	snd_rst;		/* RST should be sent */
	char	snd_urg;		/* urgent data to send */
	char	syn_acked;		/* SYN has been ACKed */
	char	syn_rcvd;		/* SYN has been received */
	char	usr_closed;		/* user has closed connection */
	char	waited_2_ml;		/* wait time for FIN ACK is up */
	char	usr_abort;		/* user has closed and does not expect
				           to receive any more data */
	char	sent_zero;		/* sent zero window */
	char	force_ack;		/* force sending of ack */
	char	t_push;
	char	t_urg;
	char	t_noactprobe;		/* see tcp_newtcpcb() */
	char	t_noactsig;
	    /* end booleans */

	u_short		snd_wnd;	/* window he advertised */
	short		t_maxseg;	/* max seg size peer can handle */
	u_short		t_maxfrag;	/* max IP frag size received */
	u_short		t_olddata;	/* useless rexmts received */
	u_short		t_preproc;	/* #segs out of window rcvd */
	u_short		t_rxtct;	/* # of retransmissions */
	u_char		t_state;	/* state of this connection */

	tcptimerval	t_srtt;		/* smoothed round trip time */
	/*
	 * Not used to limit t_srtt, but to estimate limits/values for the
	 * timers given the rxmitime = 1.5 srtt, and rxmitime doubles for
	 * each retransmission.
	 * This is the srtt on our slowest network connection.
	 */
#define TCP_tvMAXSRTT	20		    /* 10 seconds */

	tcptimerval	t_rxmitime;	/* current rexmt time */
	/*
	 * Allow some slop for the maximum in case the network experiences
	 * a temporary peak loading
	 */
#define TCP_tvRXMIN  4
#define TCP_tvRXMAX ((3 * TCP_tvMAXSRTT) / 2)

	tcptimerval	t_itimeo;	/* init timeout value */
	/* by default, try 3+ syns to get to the other side */
#define TCP_tvINIT  (TCP_tvMAXSRTT + 3 * TCP_tvRXMAX)

	tcptimerval	t_rttltimeo;	/* rxmit took too long timeout value */
	/* by default, try 4+ retransmissions before warn user */
#define TCP_tvRTTL  (TCP_tvMAXSRTT + 4 * TCP_tvRXMAX)

	tcptimerval	t_noact;	/* no activity timeout value (mins.) */
#define TCP_tvNOACT 10			    /* internal no activity timeout (min) */

	tcptimerval	t_timers[NTIMERS];/* the timers */ 
#define TCP_tvMINPERSIST 10
#define TCP_tvMAXPERSIST 90
#define TCP_tv2ML	 40                 /* 2*maximum packet lifetime */

	struct mbuf    *oob_data;	/* for 4.2 implementation of urgent */
	sequence	rcv_urpend;	/* (out-of-band) data */

	short 		sws_qff;	/* silly window syndrome and icmp
					 * source quench fudge factor */

	short		ack_skipped;
	sequence	lastack;	/* with force_ack, for TDELACK */
	u_short		rcv_wnd;	/* window we advertised */

	struct th      *t_template;	/* for send_pkt() */
};          

#if TCP_tvRXMAX > MAX_TCPTIMERVAL
	whoops
#endif
#if TCP_tvINIT  > MAX_TCPTIMERVAL
	whoops
#endif
#if TCP_tvRTTL  > MAX_TCPTIMERVAL
	whoops
#endif

struct t_debug {                /* tcp debugging record */
	u_long t_iptime;
	char t_oldstate;		/* old state */
	char t_input;			/* input */
	char t_timer;			/* timer id */ 
	char t_newstate;		/* new state */

	struct tcpcb	t_tcb;		/* -> tcb */
	struct th	t_hdr;		/* valid iff input is INRECV */
};

#define DB_PER_CHUNK(x)  (((x)/sizeof(struct t_debug)) * sizeof(struct t_debug))
#define TDBLEN	DB_PER_CHUNK(MLEN)
#define TCDBLEN DB_PER_CHUNK(CLBYTES)

/*
 * tcp statistics
 */

struct tcp_stat {
    struct in_stat t_in;
#define t_total		t_in.in_total
#define t_badsum	t_in.in_badsum
#define t_tooshort	t_in.in_tooshort
    int t_badsegs;		/* #bad tcp segments (to which we send RST) */
    int t_unack;		/* #tcp segs placed on rcv_unack */
    int t_retransmit;		/* #retransmissions we sent */
    int t_ackonly;		/* #send_pkt just to send ack, no data */
};


/* size of TCP leader (bytes) */
#define TCPSIZE (sizeof(struct th)-sizeof(struct ip))                      
/*
 * max size of TCP/IP leader.  If start using options on tcp connections,
 * increase TCPIPMAX accordingly.
 */
#define TCPIPMAX sizeof(struct th)
/* initial maximum segment size */
#define TCPMAXSND (IPMAX - TCPIPMAX)

/* get the tcpcb from the inpcb */
#define inptotcpcb(inp)	((struct tcpcb *)((inp)->inp_ppcb))
#define sototcpcb(so)	(inptotcpcb((struct inpcb *)((so)->so_pcb)))
#define tcpcbtoso(tp)	((tp)->t_in_pcb->inp_socket)

#define TCP_CTL	1			/* send/receive control call */
#define TCP_DATA 0			/* send/receive data call */

#define T_LINGERTIME 120		/* two minutes of linger */

/* tcp machine predicates */

/*
 * Is there unacked data on this TCP connection?
 */
#define is_unacked(t) (SEQ_LT((t)->snd_una, (t)->snd_hi))

/* ACK of local FIN */
#define ack_fin(x, y) (SEQ_GT((x)->seq_fin, (x)->iss) && \
			SEQ_GT((y)->t_ackno, (x)->seq_fin))

/* receive buffer empty */
#define rcv_empty(x) ((x)->usr_abort || \
		((x)->t_in_pcb->inp_socket->so_rcv.sb_cc == 0 && \
		 (x)->t_rcv_next == (struct th *)(x)))

#define t_cancel(tp, timer) ((tp)->t_timers[timer] = 0)	

sequence firstempty();
char *tcp_conn_used();	/* see note about return value */
struct th *tcp_template();

/*
 * If we have many incarnations of a connection in a time period, do not
 * want the sequence number space of them to overlap and have packets for
 * one be mistaken as from another.
 * Assume max throughput of 1Mbit/sec == 125kbyte/sec for TCP
 */
#define ISSINCR	((125*1024) / PR_SLOWHZ)

/*
 * 512 is arbitrary.
 */
#define MAX_TCPOOB	512

/*
 * TCP port allocation
 */

#define TCP_RESERVED		1023	/* <= for root only */
#define TCP_USERRESERVED	5000	/* reserved for applications */
#define TCP_MAXPORT		0xffff

#ifdef KERNEL
extern struct tcp_stat tcpstat;
#endif
