/*	routed.h	82/05/26	4.3	*/
/*
 * Routing Information Protocol
 *
 * Derived from Xerox NS Routing Information Protocol
 * by changing 32-bit net numbers to sockaddr's and
 * padding stuff to 32-bit boundaries.
 */
struct netinfo {
	struct	sockaddr rip_dst;	/* destination net/host */
	int	rip_metric;		/* cost of route */
};

struct rip {
	u_char	rip_cmd;		/* request/response */
	u_char	rip_res1[3];		/* pad to 32-bit boundary */
	struct	netinfo rip_nets[1];	/* variable length... */
};
 
#define	RIPCMD_REQUEST		0x1	/* want info */
#define	RIPCMD_RESPONSE		0x2	/* responding to request */

#define IPPORT_ROUTESERVER 	520	/* well-known port */
#define	HOPCNT_INFINITY		16	/* per Xerox NS */
#define	MAXPACKETSIZE		512	/* max broadcast size */

/*
 * Timer values used in managing the routing table.
 * Every update forces an entry's timer to be reset.  After
 * EXPIRE_TIME without updates, the entry is marked invalid,
 * but held onto until GARBAGE_TIME so that others may
 * see it "be deleted".  If there's a reason to look for network
 * interface transitions do so every CHECK_INTERVAL seconds.
 */
#define	TIMER_RATE		30	/* alarm clocks every 30 seconds */
#define	GARBAGE_TIME		210	/* time to garbage collect */
#define	EXPIRE_TIME		180	/* time to mark entry invalid */
#define	SUPPLY_INTERVAL		30	/* time to supply tables */
#define	CHECK_INTERVAL		300	/* time to check for new interfaces */
