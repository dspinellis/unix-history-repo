/* tcp.h 1.15 81/11/14 */

/*
 * Tcp header.  Fits over the ip header after option removed.
 *
 * SHOULD MAKE A CLEAN HEADER FOR USE BY USERS.
 */
struct tcpiphdr {
	struct	tcpiphdr *t_next;		/* -> next tcp on rcv chain */
	struct	tcpiphdr *t_prev;		/* -> prev tcp on rcv chain */
	u_char	t_x1;			/* (unused) */
	u_char	t_pr;			/* protocol */
/* by rights, t_len should be a u_short, but this makes operations */
/* on it very dangerous as comparisons become unsigned and comparing */
/* against negative numbers then fails... we don't expect any > 32767 */
/* byte segments, so pragmatically delcare it to be a short */
	short	t_len;			/* seg length */
	struct	ip_addr t_s;		/* source internet address */
	struct	ip_addr t_d;		/* destination internet address */
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
