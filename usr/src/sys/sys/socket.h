/*	socket.h	4.7	81/11/21	*/

/*
 * Externally visible attributes of sockets.
 */

/*
 * Socket types.
 *
 * The kernel implement these abstract (session-layer) socket
 * services, with extra protocol on top of network services
 * if necessary.
 */
#define	SOCK_STREAM	1		/* stream socket */
#define	SOCK_DGRAM	2		/* datagram socket */
#define	SOCK_RAW	3		/* raw-protocol interface */
#define	SOCK_RDM	4		/* reliably-delivered message */

/*
 * Option flags per-socket.
 */
#define	SO_DEBUG	0x01		/* turn on debugging info recording */
#define	SO_ACCEPTCONN	0x02		/* willing to accept connection */
#define	SO_NBIO		0x04		/* don't block on this socket */
#define	SO_INTNOTIFY	0x08		/* interrupt when data available */

/*
 * Generic socket protocol format.
 *
 * Each process is normally operating in a protocol family,
 * whose protocols are used unless the process specifies otherwise.
 * Most families supply protocols to the basic socket types.  When
 * protocols are not present in the family, the higher level (roughly
 * ISO session layer) code in the system layers on the protocols
 * to support the socket types.
 */
struct sockproto {
	short	sp_family;		/* protocol family */
	short	sp_protocol;		/* protocol within family */
};

#define	PF_UNSPEC	0		/* unspecified */
#define	PF_LOCAL	1		/* local to host (pipes, portals) */
#define	PF_INET		2		/* internetwork: UDP, TCP, etc. */
#define	PF_PUP		3		/* pup protocols: e.g. BSP */
#define	PF_CHAOS	4		/* mit CHAOS protocols */
#define	PF_OISCP	5		/* ois communication protocols */
#define	PF_NBS		6		/* nbs protocols */
#define	PF_ECMA		7		/* european computer manufacturers */
#define	PF_DATAKIT	8		/* datakit protocols */
#define	PF_CCITT	9		/* CCITT protocols, X.25 etc */

/*
 * Generic socket address format.
 *
 * Each process is also operating in an address family, whose
 * addresses are assigned unless otherwise requested.  The address
 * family used affects address properties: whether addresses are
 * externalized or internalized, location dependent or independent, etc.
 * The address can be defined directly if it fits in 14 bytes, or
 * a pointer and length can be given to variable length data.
 * We give these as two different structures to allow initialization.
 */
struct sockaddr {
	short	sa_family;		/* address family */
	char	sa_data[14];		/* up to 14 bytes of direct address */
};

/*
 * The first few address families correspond to protocol
 * families.  Address families unrelated to protocol families
 * are also possible.
 */
#define	AF_UNSPEC	0		/* unspecified */
#define	AF_LOCAL	1		/* local to host (pipes, portals) */
#define	AF_INET		2		/* internetwork: UDP, TCP, etc. */
#define	AF_PUP		3		/* pup protocols: e.g. BSP */
#define	AF_CHAOS	4		/* mit CHAOS protocols */
#define	AF_OISCP	5		/* ois communication protocols */
#define	AF_NBS		6		/* nbs protocols */
#define	AF_ECMA		7		/* european computer manufacturers */
#define	AF_DATAKIT	8		/* datakit protocols */
#define	AF_CCITT	9		/* CCITT protocols, X.25 etc */
