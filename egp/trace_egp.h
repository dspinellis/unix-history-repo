/*	trace_egp.h	*/

/* EGP User Process, ISI 23-Jun-84 */

			/* tracing levels */
#define	TR_INT	0x1	/* internal errors */
#define TR_EXT	0x2	/* external errors and state changes resulting from
			   egp */
#define TR_RT	0x4	/* routing changes */
#define TR_PKT	0x8	/* all egp packets sent and received */

#define TRACE_INT  if( tracing & TR_INT) printf
#define TRACE_EXT  if( tracing & TR_EXT) printf
#define TRACE_RT   if( tracing & TR_RT) printf

#define	TRACE_ACTION(action, route) { \
	    if (tracing & TR_RT) \
		traceaction( stdout, "action", route); \
	}

#define TRACE_EGPPKT(comment, src, dst, egp, length) { \
	    if( tracing & TR_PKT) \
		traceegp( "comment", src, dst, egp, length); \
	}
