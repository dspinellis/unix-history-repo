/*	trace.h	4.1	83/01/11	*/

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
	struct	interface *ifd_if;	/* for locating stuff */
};

/*
 * Packet tracing stuff.
 */
int	tracing;		/* on/off */
FILE	*ftrace;		/* output trace file */
#define	TRACE_ACTION(action, route) { \
	  if (tracing) \
		traceaction(ftrace, "action", route); \
	}
#define	TRACE_INPUT(ifp, from, size) { \
	  if (tracing) { \
		ifp = if_iflookup(from); \
		if (ifp) \
			trace(&ifp->int_input, from, packet, size, \
				ifp->int_metric); \
	  } \
	}
#define	TRACE_OUTPUT(ifp, to, size) { \
	  if (tracing) \
		trace(&ifp->int_output, to, packet, size, ifp->int_metric); \
	}
