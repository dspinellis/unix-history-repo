/* tcp_timer.c 4.1 81/11/24 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/protosw.h"
#include "../net/inet.h"
#include "../net/inet_pcb.h"
#include "../net/inet_systm.h"
#include "../net/if.h"
#include "../net/imp.h"
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../net/tcp.h"
#include "../net/tcp_fsm.h"
#include "../net/tcp_var.h"
#include "/usr/include/errno.h"

/*
 * Fast timeout routine for processing delayed acks
 */
tcp_fasttimo()
{

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
	register short *tmp;
	register int i;
COUNT(TCP_TIMEO);

	/*
	 * Search through tcb's and update active timers.
	 */
	for (ip = tcb.inp_next; ip != &tcb; ip = ip->inp_next) {
		tp = intotcpcb(ip);
		tmp = &tp->t_init;
		for (i = 0; i < TNTIMERS; i++) {
			if (*tmp && --*tmp == 0)
				(void) tcp_usrreq(tp->t_inpcb->inp_socket,
				    PRU_SLOWTIMO, (struct mbuf *)0,
				    (caddr_t)i);
			tmp++;
		}
		tp->t_xmt++;
	}
	tcp_iss += ISSINCR/2;		/* increment iss */
	splx(s);
}

/*
 * Cancel all timers for tcp tp.
 */
tcp_tcancel(tp)
	struct tcpcb *tp;
{
	register short *tmp = &tp->t_init;
	register int i;

	for (i = 0; i < TNTIMERS; i++)
		*tmp++ = 0;
}

/*
 * TCP timer went off processing.
 */
tcp_timers(tp, timertype)
	register struct tcpcb *tp;
	int timertype;
{

COUNT(TCP_TIMERS);
	switch (timertype) {

	case TFINACK:		/* fin-ack timer */
		switch (tp->t_state) {

		case TIME_WAIT:
			/*
			 * We can be sure our ACK of foreign FIN was rcvd,
			 * and can close if no data left for user.
			 */
			if (rcv_empty(tp)) {
				tcp_disconnect(tp);
				return (CLOSED);
			}
			return (RCV_WAIT);			/* 17 */

		case CLOSING:
			tp->tc_flags |= TC_WAITED_2_ML;
			return (SAME);

		default:
			return (SAME);
		}

	case TREXMT:		/* retransmission timer */
		if (tp->t_rexmt_val > tp->snd_una) {	 	/* 34 */
			/*
			 * Set so for a retransmission, increase rexmt time
			 * in case of multiple retransmissions.
			 */
			tp->snd_nxt = tp->snd_una;
			tp->tc_flags |= TC_REXMT;
			tp->t_xmtime = tp->t_xmtime << 1;
			if (tp->t_xmtime > T_REMAX)
				tp->t_xmtime = T_REMAX;
			(void) tcp_send(tp);
		}
		return (SAME);

	case TREXMTTL:		/* retransmit too long */
		if (tp->t_rtl_val > tp->snd_una) {		/* 36 */
			tcp_error(tp, ETIMEDOUT);
			return (CLOSED);
		}
		return (SAME);

	case TPERSIST:		/* persist timer */
		/*
		 * Force a byte send through closed window.
		 */
		tp->tc_flags |= TC_FORCE_ONE;
		(void) tcp_send(tp);
		return (SAME);
	}
	panic("tcp_timers");
	/*NOTREACHED*/
}
