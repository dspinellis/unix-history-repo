/* in.h 4.9 82/02/27 */

/*
 * Constants and structures defined by the internet system,
 * Per RFC 790, September 1981.
 */

/*
 * Protocols
 */
#define	IPPROTO_ICMP		1		/* control message protocol */
#define	IPPROTO_GGP		2		/* gateway^2 (deprecated) */
#define	IPPROTO_TCP		6		/* tcp */
#define	IPPROTO_PUP		12		/* pup */
#define	IPPROTO_UDP		17		/* user datagram protocol */

#define	IPPROTO_RAW		255		/* raw IP packet */
#define	IPPROTO_MAX		256

/*
 * Port/socket numbers: network standard functions
 */
#define	IPPORT_ECHO		7
#define	IPPORT_DISCARD		9
#define	IPPORT_SYSTAT		11
#define	IPPORT_DAYTIME		13
#define	IPPORT_NETSTAT		15
#define	IPPORT_FTP		21
#define	IPPORT_TELNET		23
#define	IPPORT_SMTP		25
#define	IPPORT_TIMESERVER	37
#define	IPPORT_NAMESERVER	42
#define	IPPORT_WHOIS		43
#define	IPPORT_MTP		57

/*
 * Port/socket numbers: host specific functions
 */
#define	IPPORT_TFTP		69
#define	IPPORT_RJE		77
#define	IPPORT_FINGER		79
#define	IPPORT_TTYLINK		87
#define	IPPORT_SUPDUP		95

#define	IPPORT_RESERVED		1024

/*
 * Link numbers
 */
#define	IMPLINK_IP		155
#define	IMPLINK_LOWEXPER	156
#define	IMPLINK_HIGHEXPER	158

/*
 * Internet address (old style... should be updated)
 */
struct in_addr {
	union {
		struct { u_char s_b1,s_b2,s_b3,s_b4; } S_un_b;
		struct { u_short s_w1,s_w2; } S_un_w;
		u_long S_addr;
	} S_un;
#define	s_addr	S_un.S_addr	/* can be used for most tcp & ip code */
#ifdef vax
#define	s_host	S_un.S_un_b.s_b2	/* host on imp */
#define	s_net	S_un.S_un_b.s_b1	/* network */
#define	s_imp	S_un.S_un_w.s_w2	/* imp */
#endif
};

#define	INADDR_ANY	0x00000000

/*
 * Socket address, internet style.
 */
struct sockaddr_in {
	short	sin_family;
	u_short	sin_port;
	struct	in_addr sin_addr;
	char	sin_zero[8];
};
