/*	route.h	4.8	82/06/12	*/

/*
 * Kernel resident routing tables.
 * 
 * The routing tables are initialized at boot time by
 * making entries for all directly connected interfaces.
 * Routing daemons can thereafter update the routing tables.
 *
 * TODO:
 *	keep statistics
 */

/*
 * A route consists of a destination address and a reference
 * to a routing entry.  These are often held by protocols
 * in their control blocks, e.g. inpcb.
 */
struct route {
	struct	rtentry *ro_rt;
	struct	sockaddr ro_dst;
#ifdef notdef
	caddr_t	ro_pcb;			/* not used yet */
#endif
};
#ifdef KERNEL
/*
 * The route ``routetoif'' is a special atom passed to the output routines
 * to implement the SO_DONTROUTE option.
 */
struct	route routetoif;
#endif

/*
 * We distinguish between routes to hosts and routes to networks,
 * preferring the former if available.  For each route we infer
 * the interface to use from the gateway address supplied when
 * the route was entered.  Routes that forward packets through
 * gateways are marked so that the output routines know to address the
 * gateway rather than the ultimate destination.
 */
struct rtentry {
	u_long	rt_hash;		/* to speed lookups */
	struct	sockaddr rt_dst;	/* key */
	struct	sockaddr rt_gateway;	/* value */
	short	rt_flags;		/* up/down?, host/net */
	short	rt_refcnt;		/* # held references */
	u_long	rt_use;			/* raw # packets forwarded */
	struct	ifnet *rt_ifp;		/* the answer: interface to use */
};
#ifdef KERNEL
#define	RTHASHSIZ	7
struct	mbuf *rthost[RTHASHSIZ];
struct	mbuf *rtnet[RTHASHSIZ];
#endif

#define	RTF_UP		0x1		/* route useable */
#define	RTF_GATEWAY	0x2		/* destination is a gateway */
#define	RTF_HOST	0x4		/* host entry (net otherwise) */

#define	RTFREE(rt) \
	if ((rt)->rt_refcnt == 1) \
		rtfree(rt); \
	else \
		(rt)->rt_refcnt--;
