/* tcp.h 1.16 81/11/15 */

/*
 * TCP header.
 */
struct tcphdr {
	u_short	th_sport;		/* source port */
	u_short	th_dport;		/* destination port */
	seq_t	th_seq;			/* sequence number */
	seq_t	th_ackno;		/* acknowledgement number */
	u_char
		th_x2:4,		/* (unused) */
		th_off:4;		/* data offset */
	u_char	th_flags;
#define	TH_FIN	001
#define	TH_SYN	002
#define	TH_RST	004
#define	TH_EOL	010
#define	TH_ACK	020
#define	TH_URG	040
	u_short	th_win;			/* window */
	u_short	th_sum;			/* checksum */
	u_short	th_urp;			/* urgent pointer */
};
