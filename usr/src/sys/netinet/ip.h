/* ip.h 1.1 81/10/14 */
struct ip {                     /* ip leader */
	unsigned char ip_hl:4,          /* header length */
		ip_v:4;                 /* version */
	unsigned char ip_tos;           /* type of service */
#define ip_mff ip_tos                   /* more fragments flag (input) */
	unsigned short ip_len;          /* total length */
	unsigned short ip_id;           /* identification */
	unsigned short ip_off;          /* fragment offset field */
#define ip_df 0x4000                    /* dont fragment flag */
#define ip_mf 0x2000                    /* more fragments flag (output) */
	unsigned char ip_ttl;           /* time to live */
	unsigned char ip_p;             /* protocol */
	unsigned short ip_sum;          /* checksum */
#define ip_end ip_sum                   /* fragment end */
	union {
		struct socket ip_s;     /* source address */
		struct ip *ip_nxt;      /* ->next fragment */      
	} I_sun;
#define ip_src  I_sun.ip_s
#define ip_next I_sun.ip_nxt
	union {
		struct socket ip_d;     /* destination address */
		struct ip *ip_prv;      /* ->prev fragment */
	} I_dun;
#define ip_dst  I_dun.ip_d
#define ip_prev I_dun.ip_prv
};

struct ipq {                    /* ip reass.q header */
	struct ip iqx;                  /* dummy ip element for top of list */
	struct ipq *iq_next;            /* -> next chain on q */
	struct ipq *iq_prev;            /* -> prev chain on q */
	struct ip iqh;                  /* fragment header */
};

#define IPVERSION 4             /* internet protocol version number */
#define IPLOLINK 155            /* internet link numbers */
#define IPHILINK 158
#define IPLINK IPLOLINK
#define MAXTTL 255              /* maximum time to live (seconds) */

