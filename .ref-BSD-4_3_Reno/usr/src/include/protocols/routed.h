/*-
 * Copyright (c) 1983, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)routed.h	5.2 (Berkeley) 6/25/90
 */

/*
 * Routing Information Protocol
 *
 * Derived from Xerox NS Routing Information Protocol
 * by changing 32-bit net numbers to sockaddr's and
 * padding stuff to 32-bit boundaries.
 */
#define	RIPVERSION	1

struct netinfo {
	struct	sockaddr rip_dst;	/* destination net/host */
	int	rip_metric;		/* cost of route */
};

struct rip {
	u_char	rip_cmd;		/* request/response */
	u_char	rip_vers;		/* protocol version # */
	u_char	rip_res1[2];		/* pad to 32-bit boundary */
	union {
		struct	netinfo ru_nets[1];	/* variable length... */
		char	ru_tracefile[1];	/* ditto ... */
	} ripun;
#define	rip_nets	ripun.ru_nets
#define	rip_tracefile	ripun.ru_tracefile
};
 
/*
 * Packet types.
 */
#define	RIPCMD_REQUEST		1	/* want info */
#define	RIPCMD_RESPONSE		2	/* responding to request */
#define	RIPCMD_TRACEON		3	/* turn tracing on */
#define	RIPCMD_TRACEOFF		4	/* turn it off */

#define	RIPCMD_MAX		5
#ifdef RIPCMDS
char *ripcmds[RIPCMD_MAX] =
  { "#0", "REQUEST", "RESPONSE", "TRACEON", "TRACEOFF" };
#endif

#define	HOPCNT_INFINITY		16	/* per Xerox NS */
#define	MAXPACKETSIZE		512	/* max broadcast size */

/*
 * Timer values used in managing the routing table.
 * Complete tables are broadcast every SUPPLY_INTERVAL seconds.
 * If changes occur between updates, dynamic updates containing only changes
 * may be sent.  When these are sent, a timer is set for a random value
 * between MIN_WAITTIME and MAX_WAITTIME, and no additional dynamic updates
 * are sent until the timer expires.
 *
 * Every update of a routing entry forces an entry's timer to be reset.
 * After EXPIRE_TIME without updates, the entry is marked invalid,
 * but held onto until GARBAGE_TIME so that others may
 * see it "be deleted".
 */
#define	TIMER_RATE		30	/* alarm clocks every 30 seconds */

#define	SUPPLY_INTERVAL		30	/* time to supply tables */
#define	MIN_WAITTIME		2	/* min. interval to broadcast changes */
#define	MAX_WAITTIME		5	/* max. time to delay changes */

#define	EXPIRE_TIME		180	/* time to mark entry invalid */
#define	GARBAGE_TIME		240	/* time to garbage collect */
