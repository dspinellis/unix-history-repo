/*
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This file includes significant work done at Cornell University by
 * Bill Nesheim.  That work included by permission.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)protocol.h	8.1 (Berkeley) %G%
 */

/*
 * Xerox Routing Information Protocol
 *
 */

struct netinfo {
	union ns_net	rip_dst;		/* destination net */
	u_short		rip_metric;		/* cost of route */
};

struct rip {
	u_short	rip_cmd;		/* request/response */
	struct netinfo rip_nets[1];	/* variable length */
};
 
/*
 * Packet types.
 */
#define	RIPCMD_REQUEST		1	/* want info */
#define	RIPCMD_RESPONSE		2	/* responding to request */

#define	RIPCMD_MAX		3
#ifdef RIPCMDS
char *ripcmds[RIPCMD_MAX] =
  { "#0", "REQUEST", "RESPONSE" };
#endif

#define	HOPCNT_INFINITY		16		/* per Xerox NS */
#define	DSTNETS_ALL		0xffffffff	/* per Xerox NS */
#define	MAXPACKETSIZE		512		/* max broadcast size */

extern union ns_net ns_anynet;
extern union ns_net ns_zeronet;

/*
 * Timer values used in managing the routing table.
 * Every update forces an entry's timer to be reset.  After
 * EXPIRE_TIME without updates, the entry is marked invalid,
 * but held onto until GARBAGE_TIME so that others may
 * see it "be deleted".
 */
#define	TIMER_RATE		30	/* alarm clocks every 30 seconds */

#define	SUPPLY_INTERVAL		30	/* time to supply tables */

#define	EXPIRE_TIME		180	/* time to mark entry invalid */
#define	GARBAGE_TIME		240	/* time to garbage collect */
