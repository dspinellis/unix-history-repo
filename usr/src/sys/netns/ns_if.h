/*
 * Copyright (c) 1984, 1985, 1986, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *      @(#)ns_if.h	7.2 (Berkeley) %G%
 */

/*
 * Interface address, xerox version.  One of these structures
 * is allocated for each interface with an internet address.
 * The ifaddr structure contains the protocol-independent part
 * of the structure and is assumed to be first.
 */

struct ns_ifaddr {
	struct	ifaddr ia_ifa;		/* protocol-independent info */
#define	ia_addr	ia_ifa.ifa_addr
#define	ia_broadaddr	ia_ifa.ifa_broadaddr
#define	ia_dstaddr	ia_ifa.ifa_dstaddr
#define	ia_ifp		ia_ifa.ifa_ifp
	union	ns_net	ia_net;		/* network number of interface */
	int	ia_flags;
	struct	ns_ifaddr *ia_next;	/* next in list of internet addresses */
};

/*
 * Given a pointer to an ns_ifaddr (ifaddr),
 * return a pointer to the addr as a sockadd_ns.
 */

#define	IA_SNS(ia) ((struct sockaddr_ns *)(&((struct ns_ifaddr *)ia)->ia_addr))
/*
 * ia_flags
 */
#define	IFA_ROUTE	0x01		/* routing entry installed */

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
