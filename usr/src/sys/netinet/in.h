/* in.h 4.12 82/06/05 */

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

/*
 * UNIX TCP sockets
 */
#define	IPPORT_EXECSERVER	512
#define	IPPORT_LOGINSERVER	513
#define	IPPORT_CMDSERVER	514

/*
 * UNIX UDP sockets
 */
#define	IPPORT_BIFFUDP		512
#define	IPPORT_WHOSERVER	513

/*
 * Ports < IPPORT_RESERVED are reserved for
 * privileged processes (e.g. root).
 */
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
#define	s_impno	S_un.S_un_b.s_b4	/* imp # */
#define	s_lh	S_un.S_un_b.s_b3	/* logical host */
#endif
};

/*
 * Macros for dealing with Class A/B/C network
 * numbers.  High 3 bits of uppermost byte indicates
 * how to interpret the remainder of the 32-bit
 * Internet address.
 */
#ifdef vax || pdp11
#define	IN_CLASSA_NET	0x000000ff	/* 8 bits of net # */
#define	IN_CLASSA_LNA	0xffffff00
#define	IN_CLASSB	0x00000008
#define	IN_CLASSB_NET	0x0000ffff	/* 16 bits of net # */
#define	IN_CLASSB_LNA	0xffff0000
#define	IN_CLASSC	0x0000000c
#define	IN_CLASSC_NET	0x00ffffff	/* 24 bits of net # */
#define	IN_CLASSC_LNA	0xff000000
#endif

#define	inetpart(in) \
	((((in).s_addr&IN_CLASSC)==IN_CLASSC)?((in).s_addr&IN_CLASSC_NET):\
	 ((((in).s_addr&IN_CLASSB)==IN_CLASSB)?((in).s_addr&IN_CLASSB_NET):\
	  ((in).s_addr&IN_CLASSA_NET)))
#define	lnapart(in) \
	((((in).s_addr&IN_CLASSC)==IN_CLASSC)?((in).s_addr&IN_CLASSC_LNA) : \
	 ((((in).s_addr&IN_CLASSB)==IN_CLASSB)?((in).s_addr&IN_CLASSB_LNA) : \
	  ((in).s_addr&IN_CLASSA_LNA)))

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
