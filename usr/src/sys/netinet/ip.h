/* ip.h 1.4 81/10/28 */

struct ip {
	u_char	ip_hl:4,		/* header length */
		ip_v:4;			/* version */
	u_char	ip_tos;			/* type of service */
#define	ip_mff	ip_tos			/* more fragments flag (input) */
/* by rights, ip_len should be a u_short, but this makes operations */
/* on it very dangerous as comparisons become unsigned and comparing */
/* against negative numbers then fails... we don't expect any > 32767 */
/* byte packets, so pragmatically delcare it to be a short */
	short	ip_len;			/* total length */
	u_short	ip_id;			/* identification */
/* ip_off should also, by rights, be u_short, ala ip_len */
	short	ip_off;			/* fragment offset field */
#define	ip_df 0x4000			/* dont fragment flag */
#define	ip_mf 0x2000			/* more fragments flag (output) */
	u_char	ip_ttl;			/* time to live */
	u_char	ip_p;			/* protocol */
	u_short	ip_sum;			/* checksum */
#define	ip_end	ip_sum			/* fragment end */
	union {
		struct socket ip_s;	/* source address */
		struct ip *ip_nxt;	/* next fragment */
	} I_sun;
#define	ip_src	I_sun.ip_s
#define	ip_next	I_sun.ip_nxt
	union {
		struct socket ip_d;	/* destination address */
		struct ip *ip_prv;	/* prev fragment */
	} I_dun;
#define	ip_dst	I_dun.ip_d
#define	ip_prev I_dun.ip_prv
};

/*
 * Ip reassembly queue.
 */
struct ipq {
	struct	ip iqx;		/* dummy ip element for top of list */
	struct	ipq *iq_next;	/* -> next chain on q */
	struct	ipq *iq_prev;	/* -> prev chain on q */
	struct	ip iqh;		/* fragment header */
};

#define	IPVERSION	4		/* internet protocol version number */
#define	IPLOLINK	155		/* internet link numbers */
#define	IPHILINK	158
#define	IPLINK		IPLOLINK
#define	MAXTTL		255		/* maximum time to live (seconds) */

#define	ip_bswap(p) { \
	p->ip_len = ntohs(p->ip_len); \
	p->ip_id = ntohs(p->ip_id); \
	p->ip_off = ntohs(p->ip_off); }

