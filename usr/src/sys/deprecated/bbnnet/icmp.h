#define RCSICMPHDR	"$Header: icmp.h,v 1.6 85/07/31 09:29:40 walsh Exp $"


struct icmp {				/* icmp header */
	u_char ic_type;				/* icmp message type */
	u_char ic_code;				/* icmp message sub-type */
	u_short ic_sum;	 			/* checksum */
	union {
		u_char I_off;			/* parameter error offset */
		struct in_addr I_gaddr;		/* redirect gateway addr */
		struct {
			u_short I_id;		/* echo/timestamp id */
			u_short I_seq;		/* echo/timestamp sequence */
		} I_iseq;
		long I_misc;			/* the whole field */
	} I_un1;
#define ic_off		I_un1.I_off
#define ic_gaddr 	I_un1.I_gaddr
#define ic_id 		I_un1.I_iseq.I_id
#define ic_seq 		I_un1.I_iseq.I_seq
#define ic_misc 	I_un1.I_misc
	union {
		struct ip I_iphdr;		/* ip/{proto} hdr of original
						 * IP datagram.  Has >= 
						 * ICMP_ERRLEN bytes  of
						 * protocol header
						 */
		struct {
			long I_torg;		/* originate timestamp */
			long I_trcv;		/* receive timestamp */
			long I_txmt;		/* transmit timestamp */
		} I_time;
		char I_data[1];			/* echo data */
	} I_un2;
};
#define ic_iphdr	I_un2.I_iphdr
#define ic_torg		I_un2.I_time.I_torg
#define ic_trcv		I_un2.I_time.I_trcv
#define ic_txmt		I_un2.I_time.I_txmt
#define ic_data		I_un2.I_data

					/* icmp message types */
#define ICMP_ECHOR	0			/* echo reply */
#define ICMP_UNRCH	3			/* destination unreachable */
#define		ICMP_UNRCH_NET		0		/* net unreachable */
#define		ICMP_UNRCH_HOST		1		/* host unreachable */
#define		ICMP_UNRCH_PR		2		/* protocol unrch */
#define		ICMP_UNRCH_PORT		3		/* port unreachable */
#define 	ICMP_UNRCH_FRAG		4		/* DF on fragment */
#define		ICMP_UNRCH_SRC		5		/* bad source route */
#define		ICMP_UNRCH_NUM		6
#define	ICMP_SRCQ	4			/* source quench */
#define ICMP_REDIR	5			/* redirect */
#define 	ICMP_REDIR_NET		0		/* network */
#define		ICMP_REDIR_HOST		1		/* host */
#define		ICMP_REDIR_TNET		2		/* TOS & network */
#define		ICMP_REDIR_THOST	3		/* TOS & host */
#define ICMP_ECHO	8			/* echo */
#define ICMP_TIMEX	11			/* time exceeded */
#define 	ICMP_TIMEX_XMT		0		/* in transit */
#define		ICMP_TIMEX_REASS	1		/* reassembly */
#define ICMP_PARM	12			/* parameter problem */
#define ICMP_TIMES	13			/* timestamp */
#define ICMP_TIMESR	14			/* timestamp reply */
#define ICMP_INFO	15			/* information request */
#define ICMP_INFOR	16			/* information reply */

#define ICMPSIZE 8	/* size of min. echo packet */
#define PINGTIME 8	/* ping interval (in sec/2) */
#define MAXPING 5	/* no. of pings before decide gway is down */

#define RT_REINSTATE 2	/* number of ping intervals a gateway stays down.
			 * See ip_gdown() and check_ping().
			 */

#define ICMP_ERRLEN 8	 /* 64 bits  == 8 bytes */

#define MY_ECHO_ID	0xffff

/*
 * icmp statistics
 */

struct icmp_stat {
    struct in_stat ic_in;
#define ic_total	ic_in.in_total
#define ic_badsum	ic_in.in_badsum
#define ic_tooshort	ic_in.in_tooshort
#define ic_drops	ic_in.in_drops
    int ic_broadcast;		/* #icmp pkts rcv that were broadcast (ign) */
    int ic_quenches;		/* #icmp source quenches received */
    int ic_redirects;		/* #icmp redirects received */
    int ic_echoes;		/* #icmp echo requests respond to */
    int ic_svpings;		/* #pings saved by -1 hack */
    int ic_pings;		/* #pings actually sent */
    int ic_timex;		/* #icmp time exceeded messages received */
    int ic_parm;		/* #icmp parameter problem received */
};

#ifdef KERNEL
extern struct icmp_stat icmpstat;
#endif
