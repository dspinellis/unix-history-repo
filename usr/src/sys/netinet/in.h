/*
 * Copyright (c) 1982, 1986, 1990 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)in.h	7.13 (Berkeley) %G%
 */

/*
 * Constants and structures defined by the internet system,
 * Per RFC 790, September 1981.
 */

/*
 * Protocols
 */
#define	IPPROTO_IP		0		/* dummy for IP */
#define	IPPROTO_ICMP		1		/* control message protocol */
#define	IPPROTO_IGMP		2		/* group mgmt protocol */
#define	IPPROTO_GGP		3		/* gateway^2 (deprecated) */
#define	IPPROTO_TCP		6		/* tcp */
#define	IPPROTO_EGP		8		/* exterior gateway protocol */
#define	IPPROTO_PUP		12		/* pup */
#define	IPPROTO_UDP		17		/* user datagram protocol */
#define	IPPROTO_IDP		22		/* xns idp */
#define	IPPROTO_TP		29 		/* tp-4 w/ class negotiation */
#define	IPPROTO_EON		80		/* ISO cnlp */
#define	IPPROTO_ENCAP		98		/* encapsulation header */

#define	IPPROTO_RAW		255		/* raw IP packet */
#define	IPPROTO_MAX		256


/*
 * Local port number conventions:
 * Ports < IPPORT_RESERVED are reserved for
 * privileged processes (e.g. root).
 * Ports > IPPORT_USERRESERVED are reserved
 * for servers, not necessarily privileged.
 */
#define	IPPORT_RESERVED		1024
#define	IPPORT_USERRESERVED	5000

/*
 * Internet address (a structure for historical reasons)
 */
struct in_addr {
	u_long s_addr;
};

/*
 * Definitions of bits in internet address integers.
 * On subnets, the decomposition of addresses to host and net parts
 * is done according to subnet mask, not the masks here.
 */
#define	IN_CLASSA(i)		(((long)(i) & 0x80000000) == 0)
#define	IN_CLASSA_NET		0xff000000
#define	IN_CLASSA_NSHIFT	24
#define	IN_CLASSA_HOST		0x00ffffff
#define	IN_CLASSA_MAX		128

#define	IN_CLASSB(i)		(((long)(i) & 0xc0000000) == 0x80000000)
#define	IN_CLASSB_NET		0xffff0000
#define	IN_CLASSB_NSHIFT	16
#define	IN_CLASSB_HOST		0x0000ffff
#define	IN_CLASSB_MAX		65536

#define	IN_CLASSC(i)		(((long)(i) & 0xe0000000) == 0xc0000000)
#define	IN_CLASSC_NET		0xffffff00
#define	IN_CLASSC_NSHIFT	8
#define	IN_CLASSC_HOST		0x000000ff

#define	IN_CLASSD(i)		(((long)(i) & 0xf0000000) == 0xe0000000)
#define	IN_CLASSD_NET		0xf0000000	/* These ones aren't really */
#define	IN_CLASSD_NSHIFT	28		/* net and host fields, but */
#define	IN_CLASSD_HOST		0x0fffffff	/* routing needn't know.    */
#define	IN_MULTICAST(i)		IN_CLASSD(i)

#define	IN_EXPERIMENTAL(i)	(((long)(i) & 0xf0000000) == 0xf0000000)
#define	IN_BADCLASS(i)		(((long)(i) & 0xf0000000) == 0xf0000000)

#define	INADDR_ANY		(u_long)0x00000000
#define	INADDR_BROADCAST	(u_long)0xffffffff	/* must be masked */
#ifndef KERNEL
#define	INADDR_NONE		0xffffffff		/* -1 return */
#endif

#define INADDR_UNSPEC_GROUP	(u_long)0xe0000000	/* 224.0.0.0   */
#define INADDR_ALLHOSTS_GROUP	(u_long)0xe0000001	/* 224.0.0.1   */
#define INADDR_MAX_LOCAL_GROUP	(u_long)0xe00000ff	/* 224.0.0.255 */

#define	IN_LOOPBACKNET		127			/* official! */

/*
 * Socket address, internet style.
 */
struct sockaddr_in {
	u_char	sin_len;
	u_char	sin_family;
	u_short	sin_port;
	struct	in_addr sin_addr;
	char	sin_zero[8];
};

/*
 * Structure used to describe IP options.
 * Used to store options internally, to pass them to a process,
 * or to restore options retrieved earlier.
 * The ip_dst is used for the first-hop gateway when using a source route
 * (this gets put into the header proper).
 */
struct ip_opts {
	struct	in_addr ip_dst;		/* first hop, 0 w/o src rt */
	char	ip_opts[40];		/* actually variable in size */
};

/*
 * Options for use with [gs]etsockopt at the IP level.
 * First word of comment is data type; bool is stored in int.
 */
#define	IP_OPTIONS	1	/* buf/ip_opts; set/get IP per-packet options */

#define	IP_MULTICAST_IF		2 /* in_addr; set/get IP multicast interface */
#define	IP_MULTICAST_TTL	3 /* u_char; set/get IP multicast timetolive */
#define	IP_MULTICAST_LOOP	4 /* u_char; set/get IP multicast loopback */
#define	IP_ADD_MEMBERSHIP	5 /* ip_mreq; add an IP group membership */
#define	IP_DROP_MEMBERSHIP	6 /* ip_mreq; drop an IP group membership */


#define	IP_HDRINCL	7	/* int; header is included with data (raw) */
#define	IP_TOS		8	/* int; IP type of service and precedence */
#define	IP_TTL		9	/* int; IP time to live */
#define	IP_RECVOPTS	10	/* bool; receive all IP options w/datagram */
#define	IP_RECVRETOPTS	11	/* bool; receive IP options for response */
#define	IP_RECVDSTADDR	12	/* bool; receive IP dst addr w/datagram */
#define	IP_RETOPTS	13	/* ip_opts; set/get IP per-packet options */

#define	IP_DEFAULT_MULTICAST_TTL 1	/* normally limit m'casts to 1 hop */
#define	IP_DEFAULT_MULTICAST_LOOP 1	/* normally hear sends if a member */
#define	IP_MAX_MEMBERSHIPS	20	/* per socket */

/*
 * Argument structure for IP_ADD_MEMBERSHIP and IP_DROP_MEMBERSHIP.
 */
struct ip_mreq {
	struct in_addr	imr_multiaddr;	/* IP multicast address of group */
	struct in_addr	imr_interface;	/* local IP address of interface */
};

#ifdef KERNEL
struct	in_addr in_makeaddr();
u_long	in_netof(), in_lnaof();
#endif
