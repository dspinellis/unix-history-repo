/*	tcp_timer.h	4.1	81/11/29	*/

/*
 * Definitions of the TCP timers.  These timers are counted
 * down PR_SLOWHZ times a second.
 */
#define	TCPT_NTIMERS	4

#define	TCPT_REXMT	0		/* retransmit */
#define	TCPT_2MSL	1		/* 2*msl quiet time timer */
#define	TCPT_PERSIST	2		/* retransmit persistance */
#define	TCPT_KEEP	3		/* keep alive */

#define	TCP_TTL		60		/* time to live for TCP segs */
/*
 * TCPSC constants give various timeouts in ``slow-clock'' ticks.
 */
#define	TCPSC_MSL	(120*PR_SLOWHZ)		/* max seg lifetime */
#define	TCPSC_REXMT	(  1*PR_SLOWHZ)		/* base retransmit time */
#define	TCPSC_KEEP	(240*PR_SLOWHZ)		/* keep alive */
#define	TCPSC_PERSIST	(  5*PR_SLOWHZ)		/* retransmit persistance */

#define	TCPSC_KEEPTTL	(  4*TCPSC_KEEP)	/* keep alive too long */
#define	TCPSC_2MSL	(  2*TCPSC_MSL)		/* 2*msl quiet time timer */

#define	TCPSC_TOOLONG	(480*PR_SLOWHZ)

#ifdef	TCPTIMERS
char *tcptimers[] =
    { "INIT", "REXMT", "REXMTTL", "KEEP", "KEEPTTL", "PERSIST", "2MSL" };
#endif
