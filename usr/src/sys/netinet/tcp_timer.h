/*	tcp_timer.h	4.4	81/12/21	*/

/*
 * Definitions of the TCP timers.  These timers are counted
 * down PR_SLOWHZ times a second.
 */
#define	TCPT_NTIMERS	4

#define	TCPT_REXMT	0		/* retransmit */
#define	TCPT_PERSIST	1		/* retransmit persistance */
#define	TCPT_KEEP	2		/* keep alive */
#define	TCPT_2MSL	3		/* 2*msl quiet time timer */

/*
 * The TCPT_REXMT timer is used to force retransmissions.
 * The TCP has the TCPT_REXMT timer set whenever segments
 * have been sent for which ACKs are expected but not yet
 * received.  If an ACK is received which advances tp->snd_una,
 * then the retransmit timer is cleared (if there are no more
 * outstanding segments) or reset to the base value (if there
 * are more ACKs expected).  Whenever the retransmit timer goes off,
 * we retransmit all unacknowledged segments, and do an exponential
 * backoff on the retransmit timer.
 *
 * The TCPT_PERSIST timer is used to keep window size information
 * flowing even if the window goes shut.  If an output is attempted when there
 * is data ready to transmit, but nothing gets sent because the window
 * is shut, then we start the TCPT_PERSIST timer, and at intervals
 * send a single byte into the peers window to force him to update
 * our window information.  We do this at most as often as TCPT_PERSMIN
 * time intervals, but no more frequently than the current estimate of
 * round-trip packet time.  The TCPT_PERSIST timer is cleared whenever
 * we receive a window update from the peer.
 *
 * The TCPT_KEEP timer is used to keep connections alive.  If an
 * connection is idle (no segments received) for TCPTV_KEEP amount of time,
 * but not yet established, then we drop the connection.  If the connection
 * is established, then we force the peer to send us a segment by sending:
 *	<SEQ=SND.UNA-1><ACK=RCV.NXT><CTL=ACK>
 * This segment is (deliberately) outside the window, and should elicit
 * an ack segment in response from the peer.  If, despite the TCPT_KEEP
 * initiated segments we cannot elicit a response from a peer in TCPT_MAXIDLE
 * amount of time, then we drop the connection.
 */

#define	TCP_TTL		15		/* time to live for TCP segs */
/*
 * Time constants.
 */
#define	TCPTV_MSL	( 30*PR_SLOWHZ)		/* max seg lifetime */
#define	TCPTV_SRTTBASE	(  1*PR_SLOWHZ)		/* base roundtrip time */
#define	TCPTV_KEEP	( 60*PR_SLOWHZ)		/* keep alive - 1 min */
#define	TCPTV_PERSMIN	(  5*PR_SLOWHZ)		/* retransmit persistance */

#define	TCPTV_MAXIDLE	(  4*TCPTV_KEEP)	/* maximum allowable idle
						   time before drop conn */

#define	TCPTV_MIN	(  1*PR_SLOWHZ)		/* minimum allowable value */
#define	TCPTV_MAX	(120*PR_SLOWHZ)		/* maximum allowable value */

#ifdef	TCPTIMERS
char *tcptimers[] =
    { "REXMT", "PERSIST", "KEEP", "2MSL" };
#endif

/*
 * Retransmission smoothing constants.
 * Smoothed round trip time is updated by
 *    tp->t_srtt = (tcp_alpha * tp->t_srtt) + ((1 - tcp_alpha) * tp->t_rtt)
 * each time a new value of tp->t_rtt is available.  The initial
 * retransmit timeout is then based on
 *    tp->t_timer[TCPT_REXMT] = tcp_beta * tp->t_srtt;
 * limited, however to be at least TCPTV_REXMTLO and at most TCPTV_REXMTHI.
 */
float	tcp_alpha, tcp_beta;

/*
 * Initial values of tcp_alpha and tcp_beta.
 * These are conservative: averaging over a long
 * period of time, and allowing for large individual deviations from
 * tp->t_srtt.
 */
#define	TCP_ALPHA	0.9
#define	TCP_BETA	2.0

/*
 * Force a time value to be in a certain range.
 */
#define	TCPT_RANGESET(tv, value, tvmin, tvmax) { \
	(tv) = (value); \
	if ((tv) < (tvmin)) \
		(tv) = (tvmin); \
	if ((tv) > (tvmax)) \
		(tv) = (tvmax); \
}
