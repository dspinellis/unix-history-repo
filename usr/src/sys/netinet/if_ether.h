/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_ether.h	7.7 (Berkeley) %G%
 */

/*
 * Structure of a 10Mb/s Ethernet header.
 */
struct	ether_header {
	u_char	ether_dhost[6];
	u_char	ether_shost[6];
	u_short	ether_type;
};

#define	ETHERTYPE_PUP	0x0200		/* PUP protocol */
#define	ETHERTYPE_IP	0x0800		/* IP protocol */
#define ETHERTYPE_ARP	0x0806		/* Addr. resolution protocol */

/*
 * The ETHERTYPE_NTRAILER packet types starting at ETHERTYPE_TRAIL have
 * (type-ETHERTYPE_TRAIL)*512 bytes of data followed
 * by an ETHER type (as given above) and then the (variable-length) header.
 */
#define	ETHERTYPE_TRAIL		0x1000		/* Trailer packet */
#define	ETHERTYPE_NTRAILER	16

#define	ETHERMTU	1500
#define	ETHERMIN	(60-14)

/*
 * Ethernet Address Resolution Protocol.
 *
 * See RFC 826 for protocol description.  Structure below is adapted
 * to resolving internet addresses.  Field names used correspond to 
 * RFC 826.
 */
struct	ether_arp {
	struct	arphdr ea_hdr;	/* fixed-size header */
	u_char	arp_sha[6];	/* sender hardware address */
	u_char	arp_spa[4];	/* sender protocol address */
	u_char	arp_tha[6];	/* target hardware address */
	u_char	arp_tpa[4];	/* target protocol address */
};
#define	arp_hrd	ea_hdr.ar_hrd
#define	arp_pro	ea_hdr.ar_pro
#define	arp_hln	ea_hdr.ar_hln
#define	arp_pln	ea_hdr.ar_pln
#define	arp_op	ea_hdr.ar_op


/*
 * Structure shared between the ethernet driver modules and
 * the address resolution code.  For example, each ec_softc or il_softc
 * begins with this structure.
 */
struct	arpcom {
	struct 	ifnet ac_if;		/* network-visible interface */
	u_char	ac_enaddr[6];		/* ethernet hardware address */
	struct in_addr ac_ipaddr;	/* copy of ip address- XXX */
};

/*
 * Internet to ethernet address resolution table.
 */
struct	arptab {
	struct	in_addr at_iaddr;	/* internet address */
	u_char	at_enaddr[6];		/* ethernet address */
	u_char	at_timer;		/* minutes since last reference */
	u_char	at_flags;		/* flags */
	struct	mbuf *at_hold;		/* last packet until resolved/timeout */
};

struct llinfo_arp {				
	struct	llinfo_arp *la_next;
	struct	llinfo_arp *la_prev;
	struct	rtentry *la_rt;
	struct	mbuf *la_hold;		/* last packet until resolved/timeout */
	long	la_asked;		/* last time we QUERIED for this addr */
#define la_timer la_rt->rt_rmx.rmx_expire /* deletion time in seconds */
};

struct sockaddr_inarp {
	u_char	sin_len;
	u_char	sin_family;
	u_short sin_port;
	struct	in_addr sin_addr;
	struct	in_addr sin_srcaddr;
	u_short	sin_tos;
	u_short	sin_other;
#define SIN_PROXY 1
};

#ifdef	KERNEL
u_char	etherbroadcastaddr[6];
struct	llinfo_arp *arptnew __P((struct in_addr *));
struct	llinfo_arp llinfo_arp;		/* head of the llinfo queue */
int	ether_output __P((struct ifnet *, struct mbuf *, struct sockaddr *,
			  struct rtentry *));
int	ether_input __P((struct ifnet *, struct ether_header *, struct mbuf *));
char	*ether_sprintf __P((u_char *));
int	arp_rtrequest __P((int, struct rtentry *, struct sockaddr *));
struct	ifqueue arpintrq;

/* XXX These probably belong elsewhere */
void	in_arpinput __P((struct mbuf *));
void	arpwhohas __P((struct arpcom *, struct in_addr *));
#endif
