/*	route.h	4.5	82/03/30	*/

/*
 * Structure of kernel resident routing
 * data base.  Assumption is user routing
 * daemon maintains this data base based
 * on routing information it gleans from
 * gateway protocols it listens to (e.g. GGP).
 *
 * TO ADD:
 *	more statistics -- smooth usage figures
 */
struct rtentry {
	u_long	rt_hash[2];		/* [0] for net, [1] for host */
	struct	sockaddr rt_dst;	/* match value */
	struct	sockaddr rt_gateway;	/* who to forward to */
	short	rt_flags;		/* see below */
	short	rt_refcnt;		/* # held references */
	u_long	rt_use;			/* raw # packets forwarded */
	struct	ifnet *rt_ifp;		/* interface to use */
};

struct route {
	struct	rtentry *ro_rt;
	struct	sockaddr ro_dst;
	caddr_t	ro_pcb;			/* back pointer? */
};

/*
 * Flags and host/network status.
 */
#define	RTF_UP		0x1		/* route useable */
#define	RTF_DIRECT	0x2		/* destination is a neighbor */

#ifdef KERNEL
/*
 * Lookup are hashed by a key.  Each hash bucket
 * consists of a linked list of mbuf's
 * containing routing entries.  Dead entries are
 * reclaimed along with mbufs.
 */
#define	RTHASHSIZ	16
struct	mbuf *routehash[RTHASHSIZ];
#endif
