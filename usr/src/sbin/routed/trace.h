/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)trace.h	5.1 (Berkeley) %G%
 */

/*
 * Routing table management daemon.
 */

/*
 * Trace record format.
 */
struct	iftrace {
	time_t	ift_stamp;		/* time stamp */
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
int	tracing;		/* on/off */
FILE	*ftrace;		/* output trace file */

#define	TRACE_ACTION(action, route) { \
	  if (tracing) \
		traceaction(ftrace, "action", route); \
	}
#define	TRACE_INPUT(ifp, src, size) { \
	  if (tracing) { \
		ifp = if_iflookup(src); \
		if (ifp) \
			trace(&ifp->int_input, src, packet, size, \
				ntohl(ifp->int_metric)); \
	  } \
	  if (tracepackets) \
		dumppacket(stdout, "from", src, packet, size); \
	}
#define	TRACE_OUTPUT(ifp, dst, size) { \
	  if (tracing) \
		trace(&ifp->int_output, dst, packet, size, ifp->int_metric); \
	  if (tracepackets) \
		dumppacket(stdout, "to", dst, packet, size); \
	}
