/*	if.h	4.1	81/11/18	*/

/*
 * Structure defining a network interface.
 *
 * (Would like to call this struct ``if'', but C isn't PL/1.)
 */
struct ifnet {
	short	if_unit;		/* sub-unit for lower level driver */
	short	if_flags;		/* state flags */
	short	if_mtu;			/* maximum transmission unit */
	short	if_net;			/* network number of interface */
	struct	in_addr if_addr;	/* internet address of interface */
	struct ifbuf {
		short	ib_mbcnt;	/* mbufs on chain */
		short	ib_mbhiwat;	/* mbufs permitted on chain */
		struct	mbuf *ib_hd;	/* head of chain */
		struct	mbuf *ib_tl;	/* tail of chain */
	} if_snd, if_rcv;
	struct ifsw {
		int	(*if_output)();		/* output routine */
		int	(*if_ubareset)();	/* uba reset routine */
	} *if_sw;
};

/* bits in if_flags */
#define	IF_OACTIVE	1		/* output in progress */

#ifdef KERNEL
struct	ifnet *ifnet;
struct	ifnet *if_ifwithaddr(), *if_ifonnetof();
#endif
