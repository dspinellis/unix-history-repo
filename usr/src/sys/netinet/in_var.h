/*
 * Copyright (c) 1985, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)in_var.h	7.1 (Berkeley) 6/5/86
 */

/*
 * Interface address, Internet version.  One of these structures
 * is allocated for each interface with an Internet address.
 * The ifaddr structure contains the protocol-independent part
 * of the structure and is assumed to be first.
 */
struct in_ifaddr {
	struct	ifaddr ia_ifa;		/* protocol-independent info */
#define	ia_addr	ia_ifa.ifa_addr
#define	ia_broadaddr	ia_ifa.ifa_broadaddr
#define	ia_dstaddr	ia_ifa.ifa_dstaddr
#define	ia_ifp		ia_ifa.ifa_ifp
	u_long	ia_net;			/* network number of interface */
	u_long	ia_netmask;		/* mask of net part */
	u_long	ia_subnet;		/* subnet number, including net */
	u_long	ia_subnetmask;		/* mask of net + subnet */
	struct	in_addr ia_netbroadcast; /* broadcast addr for (logical) net */
	int	ia_flags;
	struct	in_ifaddr *ia_next;	/* next in list of internet addresses */
};
/*
 * Given a pointer to an in_ifaddr (ifaddr),
 * return a pointer to the addr as a sockadd_in.
 */
#define	IA_SIN(ia) ((struct sockaddr_in *)(&((struct in_ifaddr *)ia)->ia_addr))
/*
 * ia_flags
 */
#define	IFA_ROUTE	0x01		/* routing entry installed */

#ifdef	KERNEL
struct	in_ifaddr *in_ifaddr;
struct	in_ifaddr *in_iaonnetof();
struct	ifqueue	ipintrq;		/* ip packet input queue */
#endif
