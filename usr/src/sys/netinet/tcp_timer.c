/* tcp_timer.c 4.18 82/03/24 */

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

int	tcpnodelack = 0;
/*
 * Fast timeout routine for processing delayed acks
 */
tcp_fasttimo()
{
	register struct inpcb *inp;
	register struct tcpcb *tp;
	int s = splnet();
COUNT(TCP_FASTTIMO);

	inp = tcb.inp_next;
	if (inp)
	for (; inp != &tcb; inp = inp->inp_next)
		if ((tp = (struct tcpcb *)inp->inp_ppcb) &&
		    (tp->t_flags & TF_DELACK)) {
			tp->t_flags &= ~TF_DELACK;
			tp->t_flags |= TF_ACKNOW;
			(void) tcp_output(tp);
		}
	splx(s);
}

/*
 * Tcp protocol timeout routine called every 500 ms.
 * Updates the timers in all active tcb's and
 * causes finite state machine actions if timers expire.
 */
tcp_slowtimo()
{
	register struct inpcb *ip, *ipnxt;
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
	while (ip != &tcb) {
		tp = intotcpcb(ip);
		if (tp == 0)
			continue;
		ipnxt = ip->inp_next;
		for (i = 0; i < TCPT_NTIMERS; i++) {
			if (tp->t_timer[i] && --tp->t_timer[i] == 0) {
				(void) tcp_usrreq(tp->t_inpcb->inp_socket,
				    PRU_SLOWTIMO, (struct mbuf *)0,
				    (caddr_t)i);
				if (ipnxt->inp_prev != ip)
					goto tpgone;
			}
		}
		tp->t_idle++;
		if (tp->t_rtt)
			tp->t_rtt++;
tpgone:
		ip = ipnxt;
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

int	tcprexmtprint;
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
		if (tp->t_rxtshift > TCP_MAXRXTSHIFT) {
			tcp_drop(tp, ETIMEDOUT);
			return;
		}
		TCPT_RANGESET(tp->t_timer[TCPT_REXMT],
		    (int)tp->t_srtt, TCPTV_MIN, TCPTV_MAX);
		TCPT_RANGESET(tp->t_timer[TCPT_REXMT],
		    tp->t_timer[TCPT_REXMT] << tp->t_rxtshift,
		    TCPTV_MIN, TCPTV_MAX);
if (tcprexmtprint)
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
		    tcp_beta * tp->t_srtt, TCPTV_PERSMIN, TCPTV_MAX);
		return;

	/*
	 * Keep-alive timer went off; send something
	 * or drop connection if idle for too long.
	 */
	case TCPT_KEEP:
		if (tp->t_state < TCPS_ESTABLISHED)
			goto dropit;
		if (tp->t_inpcb->inp_socket->so_options & SO_KEEPALIVE) {
		    	if (tp->t_idle >= TCPTV_MAXIDLE)
				goto dropit;
			/*
			 * Saying tp->rcv_nxt-1 lies about what
			 * we have received, and by the protocol spec
			 * requires the correspondent TCP to respond.
			 * Saying tp->snd_una-1 causes the transmitted
			 * byte to lie outside the receive window; this
			 * is important because we don't necessarily
			 * have a byte in the window to send (consider
			 * a one-way stream!)
			 */
			tcp_respond(tp,
			    tp->t_template, tp->rcv_nxt-1, tp->snd_una-1, 0);
		} else
			tp->t_idle = 0;
		tp->t_timer[TCPT_KEEP] = TCPTV_KEEP;
		return;
	dropit:
		tcp_drop(tp, ETIMEDOUT);
		return;

#ifdef TCPTRUEOOB
	/*
	 * Out-of-band data retransmit timer.
	 */
	case TCPT_OOBREXMT:
		if (tp->t_flags & TF_NOOPT)
			return;
		(void) tcp_output(tp);
		TCPT_RANGESET(tp->t_timer[TCPT_OOBREXMT],
		    2 * tp->t_srtt, TCPTV_MIN, TCPTV_MAX);
		return;
#endif
	}
}
