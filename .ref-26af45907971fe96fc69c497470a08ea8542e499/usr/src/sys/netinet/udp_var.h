/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)udp_var.h	7.12 (Berkeley) %G%
 */

/*
 * UDP kernel structures and variables.
 */
struct	udpiphdr {
	struct 	ipovly ui_i;		/* overlaid ip structure */
	struct	udphdr ui_u;		/* udp header */
};
#define	ui_next		ui_i.ih_next
#define	ui_prev		ui_i.ih_prev
#define	ui_x1		ui_i.ih_x1
#define	ui_pr		ui_i.ih_pr
#define	ui_len		ui_i.ih_len
#define	ui_src		ui_i.ih_src
#define	ui_dst		ui_i.ih_dst
#define	ui_sport	ui_u.uh_sport
#define	ui_dport	ui_u.uh_dport
#define	ui_ulen		ui_u.uh_ulen
#define	ui_sum		ui_u.uh_sum

struct	udpstat {
				/* input statistics: */
	u_long	udps_ipackets;		/* total input packets */
	u_long	udps_hdrops;		/* packet shorter than header */
	u_long	udps_badsum;		/* checksum error */
	u_long	udps_badlen;		/* data length larger than packet */
	u_long	udps_noport;		/* no socket on port */
	u_long	udps_noportbcast;	/* of above, arrived as broadcast */
	u_long	udps_fullsock;		/* not delivered, input socket full */
	u_long	udpps_pcbcachemiss;	/* input packets missing pcb cache */
				/* output statistics: */
	u_long	udps_opackets;		/* total output packets */
};

/*
 * Names for UDP sysctl objects
 */
#define	UDPCTL_CHECKSUM		1	/* checksum UDP packets */
#define UDPCTL_MAXID		2

#define UDPCTL_NAMES { \
	{ 0, 0 }, \
	{ "checksum", CTLTYPE_INT }, \
}

#ifdef KERNEL
struct	inpcb udb;
struct	udpstat udpstat;

void	 udp_ctlinput __P((int, struct sockaddr *, struct ip *));
void	 udp_init __P((void));
void	 udp_input __P((struct mbuf *, int));
int	 udp_output __P((struct inpcb *,
	    struct mbuf *, struct mbuf *, struct mbuf *));
int	 udp_sysctl __P((int *, u_int, void *, size_t *, void *, size_t));
int	 udp_usrreq __P((struct socket *,
	    int, struct mbuf *, struct mbuf *, struct mbuf *));
#endif
