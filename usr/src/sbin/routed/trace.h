/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)trace.h	5.5 (Berkeley) %G%
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
int	traceactions;		/* on/off */
int	tracehistory;		/* on/off */
FILE	*ftrace;		/* output trace file */
char	*curtime;		/* current timestamp string */

#define	TRACE_ACTION(action, route) { \
	  if (traceactions) \
		traceaction(ftrace, action, route); \
	}
#define	TRACE_NEWMETRIC(route, newmetric) { \
	  if (traceactions) \
		tracenewmetric(ftrace, route, newmetric); \
	}
#define	TRACE_INPUT(ifp, src, size) { \
	  if (tracehistory) { \
		ifp = if_iflookup(src); \
		if (ifp) \
			trace(&ifp->int_input, src, packet, size, \
				ntohl(ifp->int_metric)); \
	  } \
	  if (tracepackets) { \
		time_t t; \
		t = time(0); \
		dumppacket(stdout, "from", src, packet, size, &t); \
	  } \
	}
#define	TRACE_OUTPUT(ifp, dst, size) { \
	  if (tracehistory && ifp) \
		trace(&ifp->int_output, dst, packet, size, ifp->int_metric); \
	  if (tracepackets) { \
		time_t t; \
		t = time(0); \
		dumppacket(stdout, "to", dst, packet, size, &t); \
	  } \
	}
