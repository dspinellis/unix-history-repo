/* tcp_timer.c 4.9 81/12/20 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/protosw.h"
#include "../net/in.h"
#include "../net/in_pcb.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../net/tcp.h"
#include "../net/tcp_fsm.h"
#include "../net/tcp_seq.h"
#include "../net/tcp_timer.h"
#include "../net/tcp_var.h"
#include "../net/tcpip.h"
#include "../errno.h"

/*
 * Fast timeout routine for processing delayed acks
 */
tcp_fasttimo()
{

COUNT(TCP_FASTTIMO);
}

/*
 * Tcp protocol timeout routine called every 500 ms.
 * Updates the timers in all active tcb's and
 * causes finite state machine actions if timers expire.
 */
tcp_slowtimo()
{
	register struct inpcb *ip;
	register struct tcpcb *tp;
	int s = splnet();
	register int i;
COUNT(TCP_SLOWTIMO);

	/*
	 * Search through tcb's and update active timers.
	 */
	ip = tcb.inp_next;
	if (ip == 0) {
		splx(s);
		return;
	}
	for (; ip != &tcb; ip = ip->inp_next) {
		tp = intotcpcb(ip);
		if (tp == 0)
			continue;
		for (i = 0; i < TCPT_NTIMERS; i++) {
			if (tp->t_timer[i] && --tp->t_timer[i] == 0)
				(void) tcp_usrreq(tp->t_inpcb->inp_socket,
				    PRU_SLOWTIMO, (struct mbuf *)0,
				    (caddr_t)i);
		}
		tp->t_idle++;
		if (tp->t_rtt)
			tp->t_rtt++;
	}
	tcp_iss += TCP_ISSINCR/PR_SLOWHZ;		/* increment iss */
	splx(s);
}

/*
 * Cancel all timers for TCP tp.
 */
tcp_canceltimers(tp)
	struct tcpcb *tp;
{
	register int i;

COUNT(TCP_CANCELTIMERS);
	for (i = 0; i < TCPT_NTIMERS; i++)
		tp->t_timer[i] = 0;
}

/*
 * TCP timer processing.
 */
tcp_timers(tp, timer)
	register struct tcpcb *tp;
	int timer;
{

COUNT(TCP_TIMERS);
	switch (timer) {

	/*
	 * 2 MSL timeout in shutdown went off.  Delete connection
	 * control block.
	 */
	case TCPT_2MSL:
		tcp_close(tp);
		return;

	/*
	 * Retransmission timer went off.  Message has not
	 * been acked within retransmit interval.  Back off
	 * to a longer retransmit interval and retransmit all
	 * unacknowledged messages in the window.
	 */
	case TCPT_REXMT:
		tp->t_rxtshift++;
		TCPT_RANGESET(tp->t_timer[TCPT_REXMT],
		    ((int)(2 * tp->t_srtt)) << tp->t_rxtshift,
		    TCPTV_MIN, TCPTV_MAX);
printf("rexmt set to %d\n", tp->t_timer[TCPT_REXMT]);
		tp->snd_nxt = tp->snd_una;
		/* this only transmits one segment! */
		(void) tcp_output(tp);
		return;

	/*
	 * Persistance timer into zero window.
	 * Force a byte to be output, if possible.
	 */
	case TCPT_PERSIST:
		tp->t_force = 1;
		(void) tcp_output(tp);
		tp->t_force = 0;
		TCPT_RANGESET(tp->t_timer[TCPT_PERSIST],
		    2 * tp->t_srtt, TCPTV_PERSMIN, TCPTV_MAX);
		return;

	/*
	 * Keep-alive timer went off; send something
	 * or drop connection if idle for too long.
	 */
	case TCPT_KEEP:
		if (tp->t_state < TCPS_ESTABLISHED ||
		    tp->t_idle >= TCPTV_MAXIDLE) {
			tcp_drop(tp, ETIMEDOUT);
			return;
		}
		tcp_respond(tp->t_template, tp->rcv_nxt, tp->snd_una-1, 0);
		tp->t_timer[TCPT_KEEP] = TCPTV_KEEP;
		return;
	}
}
