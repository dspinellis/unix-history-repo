/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)in_var.h	6.2 (Berkeley) 6/8/85
 */

/*
 * Interface address, internet version.  One of these structures
 * is allocated for each interface with an internet address.
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
	int	ia_flags;
	struct	in_ifaddr *ia_next;	/* next in list of internet addresses */
};

/*
 * Given a pointer to an in_ifaddr (ifaddr),
 * return a pointer to the addr as a sockadd_in.
 */
#define	IA_SIN(ia) ((struct sockaddr_in *)(&((struct in_ifaddr *)ia)->ia_addr))

#define IA_B_SIN(ia) ((struct sockaddr_in *)(&((struct in_ifaddr *)ia)->ia_broadaddr))

/*
 * in_ifaddr to in_addr
 */
#define IA_INADDR(ia) (IA_SIN(ia)->sin_addr)

#define IA_B_INADDR(ia) (IA_B_SIN(ia)->sin_addr)

/*
 * ia_flags
 */
#define	IFA_ROUTE	0x01		/* routing entry installed */

/*
 * protocol switch table built by IP at initialization time
 */

struct ipswitch {
    struct protosw *ipsw_user;		/* for normal user packets passed up */
    struct protosw *ipsw_raw;		/* for raw packets passed up */
    int		    ipsw_hlen;
};
extern struct ipswitch ipsw[];

extern u_char inetctlerrmap[];

/*
 * Internet protocol statistics structure (should be at the head
 * of protocol dependent structures).
 */

struct in_stat {
    int in_total;		/* total packets seen */
    int in_badsum;		/* packets with bad cksums */
    int in_tooshort;		/* bad sizes */
    int in_drops;		/* packets no one (except raw) wanted */
};


#ifdef	KERNEL
extern	struct in_ifaddr *in_ifaddr;

extern  struct in_ifaddr *in_iawithaddr();
extern  struct in_ifaddr *in_iawithnet();
extern  struct in_ifaddr *in_iafromif();
#endif
