/* $Header: fsm.h,v 1.3 85/04/08 13:02:06 walsh Exp $ */


#define EFAILEC         (-1)
#define SAME            0
#define LISTEN          1
#define SYN_SENT        2
#define SYN_RCVD        3
#define L_SYN_RCVD      4
#define ESTAB           5
#define FIN_W1          6
#define FIN_W2          7
#define TIME_WAIT       8
#define CLOSE_WAIT      9
#define CLOSING1        10
#define CLOSING2        11
#define RCV_WAIT        12
#define CLOSED		13
#define TCP_NSTATES	14

#define IUOPENA 1
#define INRECV  2
#define IUOPENR 3 
#define IUCLOSE 4
#define ISTIMER 5
#define IURECV  6
#define IUSEND  7
#define IUABORT 8 
#define INCLEAR 9 
#define INOP    10	/* number of inputs to TCP fsm */

#define TXMT	0	/* measure time from sent sequence t_xmt_val to ack */
#define TINIT   1	/* timeout on connetion creation */
#define TREXMT  2	/* resend data when goes off */
#define TREXMTTL 3	/* tell user retransmissions failing when goes off */
#define TPERSIST 4	/* probe closed windows to ensure opening */
#define TFINACK 5
#define TDELACK 6	/* used to reduce ack-only traffic + incr thruput */
#define TNOACT	7	/* no activity, or close took too long */

#define NTIMERS 8	/* number of tcp timers */

