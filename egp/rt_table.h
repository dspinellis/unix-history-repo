/*	rt_table.h
 */

/* EGP User Process, ISI 23-Jun-84 */

/*
 * Modified from Routing table management daemon. table.h
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
#define	rt_router	rt_rtu.rtu_entry.rtu_router	/* who to forward to*/
#define	rt_flags	rt_rtu.rtu_entry.rtu_flags	/* kernel flags */
#define	rt_timer	rt_rtu.rtu_entry.rtu_timer	/* for invalidation */
#define	rt_state	rt_rtu.rtu_entry.rtu_state	/* see below */
#define	rt_metric	rt_rtu.rtu_entry.rtu_metric	/* cost of route */
#define	rt_ifp		rt_rtu.rtu_entry.rtu_ifp	/* interface to take*/

#define	ROUTEHASHSIZ	19

/*
 * "State" of routing table entry.
 */
#define	RTS_CHANGED	0x1  		/* route has been altered recently */
#define	RTS_PASSIVE	0x20		/* don't time out route */
#define	RTS_INTERFACE	0x40		/* route is for network interface */
#define	RTS_REMOTE	0x80 /*not used	/* route is for ``remote'' entity */
#define RTS_NOTINSTALL  0x100		/* don't install this route in the
					 * kernel as an interior route is
					 * available and preferred
					 */

#define RTS_NOTADVISENR 0x200 		/* This route notr to be advised in 
					 * EGP Net Reachable messages
					 */

#define HOPCNT_INFINITY	255		/* unreachable net distance */
#define INTERIOR	0		/* interior routing table */
#define EXTERIOR	1		/* exterior routing table */
#define DEFAULTNET	0		/* net # for default route */
#define RT_MINAGE	240  /*4 /*240	/* minimum time in seconds before
					   route is deleted when not updated*/
#define RT_NPOLLAGE	3		/* minimum number of poll intervals
					   before a route is deleted when
					   not updated */
#define RT_TIMERRATE	60  /*2 /*60	/* minimum time in seconds between
					route age increments. Actually use
					multiple of EGP hello interval as this
					is timer interrupt period */
#define INSTALLED	1		/* status of default route */
#define NOTINSTALLED	2
