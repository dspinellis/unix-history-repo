/*	defs.h	4.2	82/05/25	*/

/*
 * Internal data structure definitions for
 * user routing process.  Based on Xerox NS
 * protocol specs with mods relevant to more
 * general addressing scheme.
 */
#include <net/route.h>

/*
 * Internal routing table structure.
 * Differs a bit from kernel tables.
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
			struct	sockaddr rtu_gateway;
			short	rtu_flags;
			short	rtu_retry;
			int	rtu_timer;
			int	rtu_metric;
			struct	ifnet *rtu_ifp;
		} rtu_entry;
	} rt_rtu;
};

#define	rt_rt		rt_rtu.rtu_rt			/* pass to ioctl */
#define	rt_hash		rt_rtu.rtu_entry.rtu_hash	/* for net or host */
#define	rt_dst		rt_rtu.rtu_entry.rtu_dst	/* match value */
#define	rt_gateway	rt_rtu.rtu_entry.rtu_gateway	/* who to forward to */
#define	rt_flags	rt_rtu.rtu_entry.rtu_flags	/* see below */
#define	rt_retry	rt_rtu.rtu_entry.rtu_retry	/* retries of ioctl */
#define	rt_timer	rt_rtu.rtu_entry.rtu_timer	/* for invalidation */
#define	rt_metric	rt_rtu.rtu_entry.rtu_metric	/* cost of route */
#define	rt_ifp		rt_rtu.rtu_entry.rtu_ifp	/* interface to take */

#define	ROUTEHASHSIZ	19

/*
 * Flags used by routing process are not
 * interpreted by kernel.
 */
#define	RTF_DELRT	0x8		/* delete pending */
#define	RTF_CHGRT	0x10		/* change command pending */
#define	RTF_ADDRT	0x20		/* add command pending */
#define	RTF_SILENT	0x40		/* don't send to router */

struct	rthash nethash[ROUTEHASHSIZ], hosthash[ROUTEHASHSIZ];
struct	rt_entry *rtlookup();

/*
 * Per address family routines.  Hash returns hash key based
 * on address; netmatch verifies net # matching, output interprets
 * an address in preparation for sending; portmatch interprets
 * an address in verifying incoming packets were sent from the
 * appropriate port; checkhost is used to decide whether an
 * address is for a host, or for a network (e.g. broadcast);
 * canon purges any extraneous stuff from a sender's address
 * before pattern matching is performed (e.g. Internet ports).
 */
struct afswitch {
	int	(*af_hash)();
	int	(*af_netmatch)();
	int	(*af_output)();
	int	(*af_portmatch)();
	int	(*af_checkhost)();
	int	(*af_canon)();
};

struct afhash {
	u_int	afh_hosthash;
	u_int	afh_nethash;
};

struct	afswitch afswitch[AF_MAX];
