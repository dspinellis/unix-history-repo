/*	if.h	4.4	81/11/29	*/

/*
 * Structures defining a network interface, providing a packet
 * transport mechanism (ala level 0 of the PUP protocols).
 *
 * Each interface accepts output datagrams of a specified maximum
 * length, and provides higher level routines with input datagrams
 * received from its medium.
 *
 * Output occurs when the routine if_output is called, with three parameters:
 *	(*ifp->if_output)(ifp, m, pf)
 * Here m is the mbuf chain to be sent and pf is the protocol family
 * of the internetwork datagram format in which the data is wrapped
 * (e.g. PF_PUP or PF_INET).  The output routine encapsulates the
 * supplied datagram if necessary, and then transmits it on its medium.
 *
 * On input, each interface unwraps the data received by it, and either
 * places it on the input queue of a internetwork datagram routine
 * and posts the associated software interrupt, or passes the datagram to a raw
 * packet input routine.
 *
 * Routines exist for locating interfaces by their internet addresses
 * or for locating a interface on a certain network, as well as more general
 * routing and gateway routines maintaining information used to locate
 * interfaces.  These routines live in the files if.c and ip_ggp.c.
 */

/*
 * Structure defining a queue for a network interface.
 *
 * (Would like to call this struct ``if'', but C isn't PL/1.)
 */
struct ifnet {
	short	if_unit;		/* sub-unit for lower level driver */
	short	if_mtu;			/* maximum transmission unit */
	short	if_net;			/* network number of interface */
	int	if_host[2];		/* local net host number */
	struct	in_addr if_addr;	/* internet address of interface */
	struct	ifqueue {
		struct	mbuf *ifq_head;
		struct	mbuf *ifq_tail;
	} if_snd;			/* output queue */
/* procedure handles */
	int	(*if_init)();		/* init routine */
	int	(*if_output)();		/* output routine */
	int	(*if_ubareset)();	/* uba reset routine */
/* generic interface statistics */
	int	if_collisions;		/* collisions on csma interfaces */
	int	if_ierrors;		/* input errors */
	int	if_oerrors;		/* output errors */
/* end statistics */
	struct	ifnet *if_next;
};

/*
 * Output queues (ifp->if_snd) and internetwork datagram level (pup level 1)
 * input routines have queues of messages stored on ifqueue structures
 * (defined above).  Entries are added to and deleted from these structures
 * by these macros, which should be called with ipl raised to splimp().
 */
#define	IF_ENQUEUE(ifq, m) { \
	(m)->m_act = 0; \
	if ((ifq)->ifq_tail == 0) \
		(ifq)->ifq_head = (ifq)->ifq_tail =  m; \
	else \
		(ifq)->ifq_tail->m_act = m; \
}
#define	IF_DEQUEUE(ifq, m) { \
	(m) = (ifq)->ifq_head; \
	if (m) { \
		if (((ifq)->ifq_head = (m)->m_act) == 0) \
			(ifq)->ifq_tail = 0; \
		(m)->m_act = 0; \
	} \
}

#ifdef KERNEL
#ifdef INET
struct	ifqueue	ipintrq;		/* ip packet input queue */
#endif
struct	ifqueue rawintrq;		/* raw packet input queue */
struct	ifnet *ifnet;
struct	ifnet *if_ifwithaddr(), *if_ifonnetof(), *if_gatewayfor();
struct	in_addr if_makeaddr();
#endif
