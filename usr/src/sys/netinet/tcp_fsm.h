/*	tcp_fsm.h	4.8	81/11/25	*/

/*
 * TCP FSM state definitions.
 * Per RFC793, September, 1981.
 */

#define	TCP_NSTATES	11

#define	TCPS_CLOSED		0	/* closed */
#define	TCPS_LISTEN		1	/* listening for connection */
#define	TCPS_SYN_SENT		2	/* active, have sent syn */
#define	TCPS_SYN_RCVD		3	/* have send and received syn */
/* states < TCPS_ESTABLISHED are those where connections not established */
#define	TCPS_ESTABLISHED	4	/* established */
#define	TCPS_CLOSE_WAIT		8	/* rcvd fin, waiting for close */
/* states > TCPS_CLOSE_WAIT are those where user has closed */
#define	TCPS_FIN_WAIT_1		5	/* have closed, sent fin */
#define	TCPS_FIN_WAIT_2		6	/* have closed, fin is acked */
#define	TCPS_TIME_WAIT		7	/* in 2*msl quiet wait after close */
#define	TCPS_CLOSING		9	/* closed xchd FIN; await FIN ACK */
#define	TCPS_LAST_ACK		10	/* had fin and close; await FIN ACK */

#define	TCPS_HAVERCVDSYN(s)	((s) >= TCPS_SYN_RCVD)
#define	TCPS_HAVERCVDFIN(s)	((s) >= TCPS_TIME_WAIT)

#ifdef KPROF
int	tcp_acounts[TCP_NSTATES][PRU_NREQ];
#endif

#ifdef TCPSTATES
char *tcpstates[] = {
	"CLOSED",	"LISTEN",	"SYN_SENT",	"SYN_RCVD",
	"ESTABLISHED",	"FIN_WAIT1",	"FIN_WAIT2",	"TIME_WAIT",
	"CLOSE_WAIT",	"CLOSING",	"LAST_ACK",
};
#endif
