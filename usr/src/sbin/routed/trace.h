/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)trace.h	5.8 (Berkeley) %G%
 */

/*
 * Routing table management daemon.
 */

/*
 * Trace record format.
 */
struct	iftrace {
	struct	timeval ift_stamp;	/* time stamp */
	struct	sockaddr ift_who;	/* from/to */
	char	*ift_packet;		/* pointer to packet */
	short	ift_size;		/* size of packet */
	short	ift_metric;		/* metric on associated metric */
};

/*
 * Per interface packet tracing buffers.  An incoming and
 * outgoing circular buffer of packets is maintained, per
 * interface, for debugging.  Buffers are dumped whenever
 * an interface is marked down.
 */
struct	ifdebug {
	struct	iftrace *ifd_records;	/* array of trace records */
	struct	iftrace *ifd_front;	/* next empty trace record */
	int	ifd_count;		/* number of unprinted records */
	struct	interface *ifd_if;	/* for locating stuff */
};

/*
 * Packet tracing stuff.
 */
int	tracepackets;		/* watch packets as they go by */
int	tracecontents;		/* watch packet contents as they go by */
int	traceactions;		/* on/off */
int	tracehistory;		/* on/off */
FILE	*ftrace;		/* output trace file */

#define	TRACE_ACTION(action, route) { \
	  if (traceactions) \
		traceaction(ftrace, action, route); \
	}
#define	TRACE_NEWMETRIC(route, newmetric) { \
	  if (traceactions) \
		tracenewmetric(ftrace, route, newmetric); \
	}
#define	TRACE_INPUT(ifp, src, pack, size) { \
	  if (tracehistory) { \
		ifp = if_iflookup(src); \
		if (ifp) \
			trace(&ifp->int_input, src, pack, size, \
				ntohl(ifp->int_metric)); \
	  } \
	  if (tracepackets) \
		dumppacket(ftrace, "from", src, pack, size, &now); \
	}
#define	TRACE_OUTPUT(ifp, dst, size) { \
	  if (tracehistory && ifp) \
		trace(&ifp->int_output, dst, packet, size, ifp->int_metric); \
	  if (tracepackets) \
		dumppacket(ftrace, "to", dst, packet, size, &now); \
	}
