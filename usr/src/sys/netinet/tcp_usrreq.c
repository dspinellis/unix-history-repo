/* tcp_usrreq.c 1.41 81/12/12 */

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

extern char *tcpstates[];
struct	tcpcb *tcp_newtcpcb();
/*
 * Process a TCP user request for tcp tb.  If this is a send request
 * then m is the mbuf chain of send data.  If this is a timer expiration
 * (called from the software clock routine), then timertype tells which timer.
 */
tcp_usrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	caddr_t addr;
{
	register struct inpcb *inp = sotoinpcb(so);
	register struct tcpcb *tp;
	int s = splnet();
	int error = 0;
COUNT(TCP_USRREQ);

	/*
	 * Make sure attached.  If not,
	 * only PRU_ATTACH is valid.
	 */
	if (inp == 0 && req != PRU_ATTACH) {
		splx(s);
		return (EINVAL);
	}
	if (inp) {
		tp = intotcpcb(inp);
#ifdef KPROF
		tcp_acounts[tp->t_state][req]++;
#endif
	}
	switch (req) {

	case PRU_ATTACH:
		if (inp) {
			error = EISCONN;
			break;
		}
		error = in_pcbattach(so, &tcb, 2048, 2048, (struct sockaddr_in *)addr);
		if (error)
			break;
		inp = (struct inpcb *)so->so_pcb;
		tp = tcp_newtcpcb(inp);
		if (so->so_options & SO_ACCEPTCONN) {
			if (tp == 0) {
				in_pcbdetach(inp);
				error = ENOBUFS;
				break;
			}
			tp->t_state = TCPS_LISTEN;
		} else
			tp->t_state = TCPS_CLOSED;
		break;

	case PRU_DETACH:
		in_pcbdetach(inp);
		break;

	case PRU_CONNECT:
		error = in_pcbconnect(inp, (struct sockaddr_in *)addr);
		if (error)
			break;
		tp = tcp_newtcpcb(inp);
		if (tp == 0)
			goto badcon;
		tp->t_template = tcp_template(tp);
		if (tp->t_template == 0)
			goto badcon2;
		tp->t_inpcb = inp;
		inp->inp_ppcb = (caddr_t)tp;
		soisconnecting(so);
		tp->t_state = TCPS_SYN_SENT;
		tp->t_timer[TCPT_KEEP] = TCPTV_KEEP;
		tp->iss = tcp_iss; tcp_iss += TCP_ISSINCR/2;
		tcp_sendseqinit(tp);
		(void) tcp_output(tp);
		break;

badcon2:
		(void) m_free(dtom(tp));
badcon:
		in_pcbdisconnect(inp);
		error = ENOBUFS;
		break;

	case PRU_ACCEPT:
		soisconnected(so);
		break;

	case PRU_DISCONNECT:
		if (tp->t_state < TCPS_ESTABLISHED)
			tcp_close(tp);
		else {
			soisdisconnecting(so);
			tcp_usrclosed(tp);
			(void) tcp_output(tp);
		}
		break;

	case PRU_SHUTDOWN:
		socantsendmore(so);
		tcp_usrclosed(tp);
		(void) tcp_output(tp);
		break;

	case PRU_RCVD:
		(void) tcp_output(tp);
		break;

	case PRU_SEND:
		sbappend(&so->so_snd, m);
/*
		if (tp->t_flags & TF_PUSH)
			tp->snd_end = tp->snd_una + so->so_snd.sb_cc;
 */
		if (tp->t_flags & TF_URG)
			tp->snd_up = tp->snd_una + so->so_snd.sb_cc + 1;
		(void) tcp_output(tp);
		break;

	case PRU_ABORT:
		tcp_drop(tp, ECONNABORTED);
		break;

	case PRU_CONTROL:
		error = EOPNOTSUPP;
		break;

	case PRU_SENSE:
		error = EOPNOTSUPP;
		break;

	case PRU_RCVOOB:
		error = EOPNOTSUPP;
		break;

	case PRU_SENDOOB:
		error = EOPNOTSUPP;
		break;

	case PRU_SLOWTIMO:
		tcp_timers(tp, (int)addr);
		break;

	default:
		panic("tcp_usrreq");
	}
	splx(s);
	return (error);
}

tcp_usrclosed(tp)
	struct tcpcb *tp;
{

	switch (tp->t_state) {

	case TCPS_LISTEN:
	case TCPS_SYN_SENT:
		tp->t_state = TCPS_CLOSED;
		tcp_close(tp);
		break;

	case TCPS_SYN_RECEIVED:
	case TCPS_ESTABLISHED:
		tp->t_state = TCPS_FIN_WAIT_1;
		break;

	case TCPS_CLOSE_WAIT:
		tp->t_state = TCPS_LAST_ACK;
		break;
	}
}
