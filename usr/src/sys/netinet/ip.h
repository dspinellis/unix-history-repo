/* ip.h 1.6 81/11/08 */

/*
 * Definitions for internet protocol version 4.
 * Per RFC 791, September 1981.
 */
#define	IPVERSION	4

/*
 * Structure of an internet header, naked of options.
 *
 * SHOULD MAKE A VERSION OF THIS FOR KERNEL SO USER
 * VERSION CAN BE union FREE AND INITIALIZABLE.
 */
struct ip {
	u_char	ip_hl:4,		/* header length */
		ip_v:4;			/* version */
	u_char	ip_tos;			/* type of service */
/* we copy the IP_MF to ip_tos on input */
#define	ip_mff	ip_tos			/* more fragments flag */
/* by rights, ip_len should be a u_short, but this makes operations */
/* on it very dangerous as comparisons become unsigned and comparing */
/* against negative numbers then fails... we don't expect any > 32767 */
/* byte packets, so pragmatically delcare it to be a short */
	short	ip_len;			/* total length */
	u_short	ip_id;			/* identification */
/* ip_off should also, by rights, be u_short, ala ip_len */
	short	ip_off;			/* fragment offset field */
#define	IP_DF 0x4000			/* dont fragment flag */
#define	IP_MF 0x2000			/* more fragments flag */
	u_char	ip_ttl;			/* time to live */
	u_char	ip_p;			/* protocol */
	u_short	ip_sum;			/* checksum */
	union {
		struct ip_addr ip_s;	/* source address */
		struct ip *ip_nxt;	/* next fragment */
	} I_sun;
#define	ip_src	I_sun.ip_s
#define	ip_next	I_sun.ip_nxt
	union {
		struct ip_addr ip_d;	/* destination address */
		struct ip *ip_prv;	/* prev fragment */
	} I_dun;
#define	ip_dst	I_dun.ip_d
#define	ip_prev I_dun.ip_prv
};

/*
 * Definitions for options.
 */
#define	IPOPT_COPIED(o)		((o)&0x80)
#define	IPOPT_CLASS(o)		((o)&0x40)
#define	IPOPT_NUMBER(o)		((o)&0x3f)

#define	IPOPT_CONTROL		0x00
#define	IPOPT_RESERVED1		0x10
#define	IPOPT_DEBMEAS		0x20
#define	IPOPT_RESERVED2		0x30

#define	IPOPT_EOL		0		/* end of option list */
#define	IPOPT_NOP		1		/* no operation */

#define	IPOPT_RR		7		/* record packet route */
#define	IPOPT_TS		68		/* timestamp */
#define	IPOPT_SECURITY		130		/* provide s,c,h,tcc */
#define	IPOPT_LSRR		131		/* loose source route */
#define	IPOPT_SATID		136		/* satnet id */
#define	IPOPT_SSRR		137		/* strict source route */

/*
 * Time stamp option structure.
 */
struct	ip_timestamp {
	u_char	ipt_code;		/* IPOPT_TS */
	u_char	ipt_len;		/* size of structure (variable) */
	u_char	ipt_ptr;		/* index of current entry */
	u_char	ipt_flg:4,		/* flags, see below */
		ipt_oflw:4;		/* overflow counter */
	union {
		n_long	ipt_time[1];
		struct	ipt_ta {
			struct ip_addr ipt_addr;
			n_long ipt_time;
		} ipt_ta[1];
	}
};

/* flag bits for ipt_flg */
#define	IPOPT_TS_TSONLY		0		/* timestamps only */
#define	IPOPT_TS_TSANDADDR	1		/* timestamps and addresses */
#define	IPOPT_TS_PRESPEC	2		/* specified modules only */

/* bits for security (not byte swapped) */
#define	IPOPT_SECUR_UNCLASS	0x0000
#define	IPOPT_SECUR_CONFID	0xf135
#define	IPOPT_SECUR_EFTO	0x789a
#define	IPOPT_SECUR_MMMM	0xbc4d
#define	IPOPT_SECUR_RESTR	0xaf13
#define	IPOPT_SECUR_SECRET	0xd788
#define	IPOPT_SECUR_TOPSECRET	0x6bc5

/*
 * Ip reassembly queue structure.  Each fragment
 * being reassambled is attached to one of these structures.
 * They are timed out after ipq_ttl drops to 0, and may also
 * be reclaimed if memory becomes tight.
 */
struct ipq {
	struct	ipq *next,*prev;	/* to other reass headers */
	u_char	ipq_ttl;		/* time for reass q to live */
	u_char	ipq_p;			/* protocol of this fragment */
	u_short	ipq_id;			/* sequence id for reassembly */
	struct	ip *ipq_next,*ipq_prev;	/* to ip headers of fragments */
	struct	ip_addr ipq_src,ipq_dst;
};

/*
 * Internet implementation parameters.
 */
#define	MAXTTL		255		/* maximum time to live (seconds) */
#define	IPFRAGTTL	15		/* time to live for frag chains */

#ifdef KERNEL
struct	ipq	ipq;			/* ip reass. queue */
struct	ipq	*ip_freef();
u_short	ip_id;				/* ip packet ctr, for ids */
#endif
