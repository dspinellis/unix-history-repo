/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)table.h	5.1 (Berkeley) 6/4/85 (routed/table.h)
 *
 *	@(#)table.h	5.3 (Berkeley) %G%
 */

/*
 * Routing table management daemon.
 */

/*
 * Routing table structure; differs a bit from kernel tables.
 *
 * Note: the union below must agree in the first 4 members
 * so the ioctl's will work.
 */
struct rthash {
	struct	rt_entry *rt_forw;
	struct	rt_entry *rt_back;
};

#ifdef RTM_ADD
#define rtentry ortentry
#endif

struct rt_entry {
	struct	rt_entry *rt_forw;
	struct	rt_entry *rt_back;
	union {
		struct	rtentry rtu_rt;
		struct {
			u_long	rtu_hash;
			struct	sockaddr rtu_dst;
			struct	sockaddr rtu_router;
			short	rtu_flags;
			short	rtu_state;
			int	rtu_timer;
			int	rtu_metric;
			struct	interface *rtu_ifp;
		} rtu_entry;
	} rt_rtu;
};

#define	rt_rt		rt_rtu.rtu_rt			/* pass to ioctl */
#define	rt_hash		rt_rtu.rtu_entry.rtu_hash	/* for net or host */
#define	rt_dst		rt_rtu.rtu_entry.rtu_dst	/* match value */
#define	rt_router	rt_rtu.rtu_entry.rtu_router	/* who to forward to */
#define	rt_flags	rt_rtu.rtu_entry.rtu_flags	/* kernel flags */
#define	rt_timer	rt_rtu.rtu_entry.rtu_timer	/* for invalidation */
#define	rt_state	rt_rtu.rtu_entry.rtu_state	/* see below */
#define	rt_metric	rt_rtu.rtu_entry.rtu_metric	/* cost of route */
#define	rt_ifp		rt_rtu.rtu_entry.rtu_ifp	/* interface to take */

#define	ROUTEHASHSIZ	32		/* must be a power of 2 */
#define	ROUTEHASHMASK	(ROUTEHASHSIZ - 1)

/*
 * "State" of routing table entry.
 */
#define	RTS_CHANGED	0x1		/* route has been altered recently */
#define	RTS_PASSIVE	IFF_PASSIVE	/* don't time out route */
#define	RTS_INTERFACE	IFF_INTERFACE	/* route is for network interface */
#define	RTS_REMOTE	IFF_REMOTE	/* route is for ``remote'' entity */

struct	rthash nethash[ROUTEHASHSIZ];
struct	rthash hosthash[ROUTEHASHSIZ];
struct	rt_entry *rtlookup();
struct	rt_entry *rtfind();
