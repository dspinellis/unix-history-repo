/*	if.h	4.3	81/11/26	*/

/*
 * Definitions for network interfaces.
 */
struct ifqueue {
	struct	mbuf *ifq_head;
	struct	mbuf *ifq_tail;
};

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
	struct	ifqueue if_snd;		/* output queue */
	int	(*if_output)();		/* output routine */
	int	(*if_ubareset)();	/* uba reset routine */
	int	if_collisions;
	int	if_ierrors;
	int	if_oerrors;
	struct	ifnet *if_next;
};

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
struct	ifnet *ifnet;
struct	ifnet *if_ifwithaddr(), *if_ifonnetof(), *if_gatewayfor();
#endif
