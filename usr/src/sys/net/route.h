/*	route.h	4.7	82/03/31	*/

/*
 * Kernel resident routing tables.
 * 
 * Each interface makes an entry at boot time so that
 * correspondents directly addressible can be found.
 * User programs can update this data base from information
 * stored in the file system or information gleaned from
 * routing protocol interactions with gateways.
 *
 * TODO:
 *	keep statistics
 *	smooth usage figures
 */
struct rtentry {
	u_long	rt_hash;		/* for net or for host */
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
#define	RTF_HOST	0x4		/* host entry (net otherwise) */

#define	RTFREE(rt) \
	if ((rt)->rt_refcnt == 1) \
		rtfree(rt); \
	else \
		(rt)->rt_refcnt--;

#ifdef KERNEL
/*
 * Lookup are hashed by a key.  Each hash bucket
 * consists of a linked list of mbuf's
 * containing routing entries.  Dead entries are
 * reclaimed along with mbufs.
 */
#define	RTHASHSIZ	7
struct	mbuf *rthost[RTHASHSIZ];
struct	mbuf *rtnet[RTHASHSIZ];
#endif
