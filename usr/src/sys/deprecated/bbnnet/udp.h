#define RCSUDPHDR "$Header: udp.h,v 1.1 84/04/05 15:23:56 walsh Exp $"

struct udp {			/* user dgram proto leader (fits over ip hdr) */
	char		u_x[8];		/* unused fields from ip */
	u_char		u_x1;		/* unused */
	u_char		u_pr;		/* protocol number */
	u_short		u_ilen;		/* pseudo header length == UDP length */
	struct in_addr	u_s;		/* source internet address */
	struct in_addr	u_d;		/* destination internet address */
	u_short		u_src;		/* source port */
	u_short		u_dst;		/* destination port */
	u_short		u_len;		/* length */
	u_short		u_sum;		/* checksum */
};

#define UDPSIZE 8			/* UDP header only */
#define UDPCKSIZE 12			/* UDP pseudo header */

struct udp_stat {
    struct in_stat u_in;
#define u_total		u_in.in_total
#define u_badsum	u_in.in_badsum
#define u_tooshort	u_in.in_tooshort
#define u_drops		u_in.in_drops
    int u_sonospace;		/* #udp pkts user socket rcv buf full (drop) */
    int u_nobuf;		/* #udp pkts can't m_get for socket code */
};

/*
 * UDP port information
 */

#define UDP_RESERVED		1023
#define UDP_USERRESERVED	5000
#define UDP_MAXPORT		0xffff

#ifdef KERNEL
extern struct udp_stat udpstat;
char *udp_conn_used();
#endif
