/* tcp_timer.c 4.3 81/11/26 */

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
#include "/usr/include/errno.h"

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
	register short *tmp;
	register int i;
COUNT(TCP_SLOWTIMO);

	/*
	 * Search through tcb's and update active timers.
	 */
	for (ip = tcb.inp_next; ip != &tcb; ip = ip->inp_next) {
		tp = intotcpcb(ip);
		for (i = 0; i < TCPT_NTIMERS; i++) {
			if (tp->t_timer[i] && --tp->t_timer[i] == 0)
				(void) tcp_usrreq(tp->t_inpcb->inp_socket,
				    PRU_SLOWTIMO, (struct mbuf *)0,
				    (caddr_t)i);
			tmp++;
		}
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
 * TCP timer went off processing.
 */
tcp_timers(tp, timer)
	register struct tcpcb *tp;
	int timer;
{

COUNT(TCP_TIMERS);
	switch (timer) {

	case TCPT_2MSL:
		tcp_close(tp);
		return;

	case TCPT_REXMT:
#if 0
		tp->t_xmtime <<= 1;
		if (tp->t_xmtime > TCPSC_TOOLONG) {
			tcp_drop(tp, ETIMEDOUT);
			return;
		}
#endif
		tcp_output(tp);
		return;

	case TCPT_PERSIST:
		if (tcp_output(tp) == 0)
			tp->snd_wnd++, (void) tcp_output(tp), tp->snd_wnd--;
		/* reset? */
		return;

	case TCPT_KEEP:
		/* reset? */
		return;
	}
}
