/*
 * Copyright (c) 1984, 1985, 1986, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ns_if.h	7.6 (Berkeley) %G%
 */

/*
 * Interface address, xerox version.  One of these structures
 * is allocated for each interface with an internet address.
 * The ifaddr structure contains the protocol-independent part
 * of the structure and is assumed to be first.
 */

struct ns_ifaddr {
	struct	ifaddr ia_ifa;		/* protocol-independent info */
#define	ia_ifp		ia_ifa.ifa_ifp
#define	ia_flags	ia_ifa.ifa_flags
/*	union	ns_net	ia_net;		/* network number of interface */
#define ia_net		ia_addr.sns_addr.x_net
	struct	ns_ifaddr *ia_next;	/* next in list of xerox addresses */
	struct	sockaddr_ns ia_addr;	/* reserve space for my address */
	struct	sockaddr_ns ia_dstaddr;	/* space for my broadcast address */
#define ia_broadaddr	ia_dstaddr
	struct	sockaddr_ns ia_netmask;	/* space for my network mask */
};

struct	ns_aliasreq {
	char	ifra_name[IFNAMSIZ];		/* if name, e.g. "en0" */
	struct	sockaddr_ns ifra_addr;
	struct	sockaddr_ns ifra_broadaddr;
#define ifra_dstaddr ifra_broadaddr
};
/*
 * Given a pointer to an ns_ifaddr (ifaddr),
 * return a pointer to the addr as a sockadd_ns.
 */

#define	IA_SNS(ia) (&(((struct ns_ifaddr *)(ia))->ia_addr))

/* This is not the right place for this but where is? */
#define	ETHERTYPE_NS	0x0600

#ifdef	NSIP
struct nsip_req {
	struct sockaddr rq_ns;	/* must be ns format destination */
	struct sockaddr rq_ip;	/* must be ip format gateway */
	short rq_flags;
};
#endif

#ifdef	KERNEL
struct	ns_ifaddr *ns_ifaddr;
struct	ns_ifaddr *ns_iaonnetof();
struct	ifqueue	nsintrq;	/* XNS input packet queue */
#endif
