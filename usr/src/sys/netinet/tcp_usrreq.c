/* tcp_usrreq.c 1.34 81/11/24 */

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
	register int nstate;
	int error = 0;
COUNT(TCP_USRREQ);

	/*
	 * Make sure attached.  If not,
	 * only PRU_ATTACH is valid.
	 */
	if (inp == 0) {
		if (req != PRU_ATTACH) {
			splx(s);
			return (EINVAL);
		}
	} else {
		tp = intotcpcb(inp);
		nstate = tp->t_state;
#ifdef KPROF
		tcp_acounts[nstate][req]++;
#endif
	}

	switch (req) {

	case PRU_ATTACH:
		if (inp) {
			error = EISCONN;
			break;
		}
		error = in_pcballoc(so, &tcb, 2048, 2048, (struct sockaddr_in *)addr);
		if (error) {
			(void) m_free(dtom(tp));
			break;
		}
		inp = (struct inpcb *)so->so_pcb;
		if (so->so_options & SO_ACCEPTCONN) {
			tp = tcp_newtcpcb(inp);
			if (tp == 0) {
				error = ENOBUFS;
				break;
			}
			nstate = LISTEN;
		} else
			nstate = CLOSED;
		break;

	case PRU_DETACH:
		break;

	case PRU_CONNECT:
		error = in_pcbsetpeer(inp, (struct sockaddr_in *)addr);
		if (error)
			break;
		tp = tcp_newtcpcb(inp);
		if (tp == 0) {
			inp->inp_faddr.s_addr = 0;
			error = ENOBUFS;
			break;
		}
		tp->t_inpcb = inp;
		inp->inp_ppcb = (caddr_t)tp;
		(void) tcp_sndctl(tp);
		nstate = SYN_SENT;
		soisconnecting(so);
		break;

	case PRU_ACCEPT:
		soisconnected(so);
		break;

	case PRU_DISCONNECT:
		if (nstate < ESTAB)
			tcp_disconnect(tp);
		else {
			tp->tc_flags |= TC_SND_FIN;
			(void) tcp_sndctl(tp);
			soisdisconnecting(so);
		}
		break;

	case PRU_SHUTDOWN:
		switch (nstate) {

		case TCPS_LISTEN:
		case TCPS_SYN_SENT:
			nstate = TCPS_CLOSED;
			break;

		case TCPS_SYN_RCVD:
		case TCPS_ESTABLISHED:
		case TCPS_CLOSE_WAIT:
			tp->tc_flags |= TC_SND_FIN;
			(void) tcp_sndctl(tp);
			nstate = nstate != CLOSE_WAIT ? FIN_W1 : LAST_ACK;
			break;
			
		case TCPS_FIN_W1:
		case TCPS_FIN_W2:
		case TCPS_TIME_WAIT:
		case TCPS_CLOSING:
		case TCPS_LAST_ACK:
		case TCPS_RCV_WAIT:
			break;

		default:
			goto bad;
		}
		break;

	case PRU_RCVD:
		if (nstate < TCPS_ESTAB)
			goto bad;
		tcp_sndwin(tp);
		if (nstate == TCPS_RCV_WAIT && rcv_empty(tp))
			nstate = TCPS_CLOSED;
		break;

	case PRU_SEND:
		switch (nstate) {

		case ESTAB:
		case CLOSE_WAIT:
			tcp_usrsend(tp, m);
			break;
		
		default:
			if (nstate < ESTAB)
				goto bad;
			m_freem(m);
			error = ENOTCONN;
			break;
		}
		break;

	case PRU_ABORT:
		tcp_abort(tp);
		nstate = CLOSED;
		break;

	case PRU_CONTROL:
		error = EOPNOTSUPP;
		break;

	case PRU_SLOWTIMO:
		switch (nstate) {

		case 0:
		case CLOSED:
		case LISTEN:
			goto bad;

		default:
			nstate = tcp_timers(tp, (int)addr);
		}
		break;

	default:
		panic("tcp_usrreq");
	bad:
		printf("tcp: bad state: tcb=%x state=%d input=%d\n",
		    tp, tp->t_state, req);
		nstate = EFAILEC;
		break;
	}
	switch (nstate) {

	case CLOSED:
	case SAME:
		break;

	case EFAILEC:
		if (m)
			m_freem(dtom(m));
		break;

	default:
		tp->t_state = nstate;
		break;
	}
	splx(s);
	return (error);
}

struct tcpcb *
tcp_newtcpcb(inp)
	struct inpcb *inp;
{
	struct mbuf *m = m_getclr(0);
	register struct tcpcb *tp;
COUNT(TCP_NEWTCPCB);

	if (m == 0)
		return (0);
	tp = mtod(m, struct tcpcb *);

	/*
	 * Make empty reassembly queue.
	 */
	tp->seg_next = tp->seg_prev = (struct tcpiphdr *)tp;

	/*
	 * Initialize sequence numbers and round trip retransmit timer.
	 */
	tp->t_xmtime = T_REXMT;
	tp->snd_end = tp->seq_fin = tp->snd_nxt = tp->snd_hi = tp->snd_una =
	    tp->iss = tcp_iss;
	tp->snd_off = tp->iss + 1;
	tcp_iss += (ISSINCR >> 1) + 1;

	/*
	 * Hook to inpcb.
	 */
	tp->t_inpcb = inp;
	inp->inp_ppcb = (caddr_t)tp;
	return (tp);
}

tcp_disconnect(tp)
	register struct tcpcb *tp;
{
	register struct tcpiphdr *t;

COUNT(TCP_DISCONNECT);
	tcp_tcancel(tp);
	t = tp->seg_next;
	for (; t != (struct tcpiphdr *)tp; t = (struct tcpiphdr *)t->ti_next)
		m_freem(dtom(t));
	tcp_drainunack(tp);
	if (tp->t_template) {
		(void) m_free(dtom(tp->t_template));
		tp->t_template = 0;
	}
	in_pcbfree(tp->t_inpcb);
	(void) m_free(dtom(tp));
}

tcp_abort(tp)
	register struct tcpcb *tp;
{

COUNT(TCP_ABORT);
	switch (tp->t_state) {

	case SYN_RCVD:
	case ESTAB:
	case FIN_W1:
	case FIN_W2:
	case CLOSE_WAIT:
		tp->tc_flags |= TC_SND_RST;
		tcp_sndnull(tp);
	}
	soisdisconnected(tp->t_inpcb->inp_socket);
	tcp_disconnect(tp);
}

/*
 * Send data queue headed by m0 into the protocol.
 */
tcp_usrsend(tp, m0)
	register struct tcpcb *tp;
	struct mbuf *m0;
{
	register struct socket *so = tp->t_inpcb->inp_socket;
COUNT(TCP_USRSEND);

	sbappend(&so->so_snd, m0);
	if (tp->t_options & TO_EOL)
		tp->snd_end = tp->snd_off + so->so_snd.sb_cc;
	if (tp->t_options & TO_URG) {
		tp->snd_urp = tp->snd_off + so->so_snd.sb_cc + 1;
		tp->tc_flags |= TC_SND_URG;
	}
	(void) tcp_send(tp);
}

/*ARGSUSED*/
tcp_sense(m)
	struct mbuf *m;
{

COUNT(TCP_SENSE);
	return (EOPNOTSUPP);
}

tcp_drop(tp, errno)
	struct tcpcb *tp;
	int errno;
{
	struct socket *so = tp->t_inpcb->inp_socket;

COUNT(TCP_ERROR);
	so->so_error = errno;
	sorwakeup(so);
	sowwakeup(so);
	tcp_disconnect(tp);
}

tcp_drain()
{
	register struct inpcb *ip;

COUNT(TCP_DRAIN);
	for (ip = tcb.inp_next; ip != &tcb; ip = ip->inp_next)
		tcp_drainunack(intotcpcb(ip));
}

tcp_drainunack(tp)
	register struct tcpcb *tp;
{
	register struct mbuf *m;

COUNT(TCP_DRAINUNACK);
	for (m = tp->seg_unack; m; m = m->m_act)
		m_freem(m);
	tp->seg_unack = 0;
}
	
tcp_ctlinput(m)
	struct mbuf *m;
{

COUNT(TCP_CTLINPUT);
	m_freem(m);
}
