/* tcp.h 1.21 82/01/18 */

typedef	u_long	tcp_seq;
/*
 * TCP header.
 * Per RFC 793, September, 1981.
 */
struct tcphdr {
	u_short	th_sport;		/* source port */
	u_short	th_dport;		/* destination port */
	tcp_seq	th_seq;			/* sequence number */
	tcp_seq	th_ack;			/* acknowledgement number */
	u_char
		th_x2:4,		/* (unused) */
		th_off:4;		/* data offset */
	u_char	th_flags;
#define	TH_FIN	0x01
#define	TH_SYN	0x02
#define	TH_RST	0x04
#define	TH_PUSH	0x08
#define	TH_ACK	0x10
#define	TH_URG	0x20
	u_short	th_win;			/* window */
	u_short	th_sum;			/* checksum */
	u_short	th_urp;			/* urgent pointer */
};

#define	TCPOPT_EOL	0
#define	TCPOPT_NOP	1
#define	TCPOPT_MAXSEG	2

#ifdef TCPTRUEOOB
/*
 * True out-of-band as value added option.
 * Advertise willingness with TCPOPT_WILOOB in
 * initial segment.  If peer is willing, will receive
 * such also.  Then can send TCPOPT_OOBDATA whenever oob data
 * exists; peer should ack with TCPOPT_OOBACK in segment.
 */
#define	TCPOPT_WILLOOB	64		/* bytes: 64, 2 */
#define	TCPOPT_OOBDATA	65		/* bytes: 65, 8, seq#, data, markseq */
#define	TCPOPT_OOBACK	66		/* bytes: 66, 3, ack# */
#endif
