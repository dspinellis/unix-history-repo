/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nfsrtt.h	7.2 (Berkeley) %G%
 */

/*
 * Definitions for performance monitor.
 * The client and server logging are turned on by setting the global
 * constant "nfsrtton" to 1.
 */
#define	NFSRTTLOGSIZ	128

/*
 * Circular log of client side rpc activity. Each log entry is for one
 * rpc filled in upon completion. (ie. in order of completion)
 * The "pos" is the table index for the "next" entry, therefore the
 * list goes from nfsrtt.rttl[pos] --> nfsrtt.rttl[pos - 1] in
 * chronological order of completion.
 */
struct nfsrtt {
	int pos;			/* Position in array for next entry */
	struct rttl {
		int	proc;		/* NFS procedure number */
		int	rtt;		/* Measured round trip time */
		int	rto;		/* Round Trip Timeout */
		int	sent;		/* # rpcs in progress */
		int	cwnd;		/* Send window */
		int	srtt;		/* Ave Round Trip Time */
		int	sdrtt;		/* Ave mean deviation of RTT */
		fsid_t	fsid;		/* Fsid for mount point */
		struct timeval tstamp;	/* Timestamp of log entry */
	} rttl[NFSRTTLOGSIZ];
};

/*
 * And definitions for server side performance monitor.
 * The log organization is the same as above except it is filled in at the
 * time the server sends the rpc reply.
 */

/*
 * Bits for the flags field.
 */
#define	DRT_NQNFS	0x01	/* Rpc used Nqnfs protocol */
#define	DRT_TCP		0x02	/* Client used TCP transport */
#define	DRT_CACHEREPLY	0x04	/* Reply was from recent request cache */
#define	DRT_CACHEDROP	0x08	/* Rpc request dropped, due to recent reply */

/*
 * Server log structure
 * NB: ipadr == INADDR_ANY indicates a client using a non IP protocol.
 *	(ISO perhaps?)
 */
struct nfsdrt {
	int pos;			/* Position of next log entry */
	struct drt {
		int	flag;		/* Bits as defined above */
		int	proc;		/* NFS procedure number */
		u_long	ipadr;		/* IP address of client */
		int	resptime;	/* Response time (usec) */
		struct timeval tstamp;	/* Timestamp of log entry */
	} drt[NFSRTTLOGSIZ];
};
