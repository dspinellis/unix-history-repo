/*
 * Constants and structures defined by the internet system,
 * Per RFC 790, September 1981.
 */

/*
 * Protocols
 */
#define	IPPROTO_IP		0		/* dummy for IP */
#define	IPPROTO_ICMP		1		/* control message protocol */
#define	IPPROTO_GGP		2		/* gateway^2 (deprecated) */
#define	IPPROTO_TCP		6		/* tcp */
#define	IPPROTO_EGP		8		/* exterior gateway protocol */
#define	IPPROTO_PUP		12		/* pup */
#define	IPPROTO_UDP		17		/* user datagram protocol */
#define IPPROTO_HMP		20		/* host monitoring protocol */
#define	IPPROTO_IDP		22		/* xns idp */
#define IPPROTO_RDP		27		/* reliabe datagram protocol */

#define	IPPROTO_MAX		256


/*
 * historical and inaccurate.  See protocol .h files if you care
 * about reserved ports.
 */
#define	IPPORT_RESERVED		1024

/*
 * Link numbers
 */
#define	IMPLINK_IP		155
#define	IMPLINK_LOWEXPER	156
#define	IMPLINK_HIGHEXPER	158

#ifdef when_convince_berk
/*
 * Internet layers for getsockopt()/setsockopt()
 * (gaps left in case we forgot something)
 */

#define SOL_INPROTO		2	/* tcp/udp/hmp/rdp */
#define SOL_INRAW		4	/* ip */
#define SOL_INETHER		6	/* ARP, etc. */
#define SOL_INIFADDR		8	/* interface addrs */
#endif

/*
 * Internet address (a structure for historical reasons)
 */
struct in_addr {
	u_long s_addr;
};

#define	INADDR_ANY	  ((u_long) 0x00000000)
#define	INADDR_BROADCAST  ((u_long) 0xffffffff)		/* must be masked */

/*
 * Socket address, internet style.
 */
struct sockaddr_in {
	short	sin_family;
	u_short	sin_port;
	struct	in_addr sin_addr;
	char	sin_zero[8];
};

/*
 * Options for use with [gs]etsockopt at the IP level.
 */
#define	IP_OPTIONS	1		/* set/get IP per-packet options */

#if !defined(vax)
/*
 * Macros for number representation conversion.
 */
#define	ntohl(x)	(x)
#define	ntohs(x)	(x)
#define	htonl(x)	(x)
#define	htons(x)	(x)
#endif

#ifdef KERNEL
extern	struct domain inetdomain;
extern	struct protosw inetsw[];
struct	in_addr in_makeaddr();
/*
 * Treat a sockaddr as a sockaddr_in, and retrieve the IP address
 * associated with it.
 */
#define satoipa(x) (((struct sockaddr_in *) (x)) ->sin_addr)

#endif
