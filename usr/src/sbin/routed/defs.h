/*	defs.h	4.1	82/05/22	*/

/*
 * Internal data structure definitions for
 * user routing process.  Based on Xerox NS
 * protocol specs with mods relevant to more
 * general addressing scheme.
 */

/*
 * Internal routing table structure.
 * Differs a bit from kernel tables.
 */
struct rt_hash {
	struct	rt_entry *rt_forw;
	struct	rt_entry *rt_back;
};

struct rt_entry {
	struct	rt_entry *rt_forw;
	struct	rt_entry *rt_back;
	u_long	rt_hash;		/* for net or for host */
	struct	sockaddr rt_dst;	/* match value */
	struct	sockaddr rt_gateway;	/* who to forward to */
	short	rt_flags;		/* see below */
	short	rt_retry;		/* # ioctl retries */
	int	rt_timer;		/* for invalidation */
	int	rt_metric;		/* hop count of route */
	struct	ifnet *rt_ifp;		/* corresponding interface */
};

#define	ROUTEHASHSIZ	19

/*
 * Flags used by routing process are not
 * interpreted by kernel.
 */
#define	RTF_DELRT	0x8		/* delete pending */
#define	RTF_CHGRT	0x10		/* change command pending */
#define	RTF_ADDRT	0x20		/* add command pending */
#define	RTF_SILENT	0x40		/* don't send to router */

struct	rt_hash nethash[ROUTEHASHSIZ], hosthash[ROUTEHASHSIZ];
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
