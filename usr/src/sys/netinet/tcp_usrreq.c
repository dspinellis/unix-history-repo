/* tcp_usrreq.c 1.32 81/11/20 */

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
#define TCPFSTAB
#ifdef TCPDEBUG
#define TCPSTATES
#endif
#include "../net/tcp_fsm.h"
#include "../net/tcp_var.h"
#include "/usr/include/errno.h"

/*
 * Tcp initialization
 */
tcp_init()
{

	tcp_iss = 1;		/* wrong */
	tcb.inp_next = tcb.inp_prev = &tcb;
}

/*
 * Tcp finite state machine entries for timer and user generated
 * requests.  These routines raise the ipl to that of the network
 * to prevent reentry.  In particluar, this requires that the software
 * clock interrupt have lower priority than the network so that
 * we can enter the network from timeout routines without improperly
 * nesting the interrupt stack.
 */

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
#ifdef TCPDEBUG
	struct tcp_debug tdb;
#endif
	int error = 0;
COUNT(TCP_USRREQ);

	/*
	 * Make sure attached.  If not,
	 * only PRU_ATTACH is valid.
	 */
#ifdef TCPDEBUG
	tdb.td_tod = 0;
#endif
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
#ifdef TCPDEBUG
		if (((tp->t_socket->so_options & SO_DEBUG) || tcpconsdebug)) {
			tdb_setup(tp, (struct tcpiphdr *)0, req, &tdb);
			tdb.td_tim = timertype;
		}
#endif
		tp->tc_flags &= ~TC_NET_KEEP;
	}

	switch (req) {

	case PRU_ATTACH:
		if (inp) {
			error = EISCONN;
			break;
		}
		tp = tcp_newtcpcb();
		if (tp == 0) {
			error = ENOBUFS;
			break;
		}
		error = in_pcballoc(so, &tcb, 2048, 2048, (struct sockaddr_in *)addr);
		if (error) {
			(void) m_free(dtom(tp));
			break;
		}
		inp = (struct inpcb *)so->so_pcb;
		tp->t_inpcb = inp;
		inp->inp_ppcb = (caddr_t)tp;
		if (so->so_options & SO_ACCEPTCONN)
			nstate = LISTEN;
		else
			nstate = CLOSED;
		break;

	case PRU_DETACH:
		tcp_detach(tp);
		break;

	case PRU_CONNECT:
		if (tp->t_state != 0 && tp->t_state != CLOSED)
			goto bad;
		error = in_pcbsetpeer(inp, (struct sockaddr_in *)addr);
		if (error)
			break;
		(void) tcp_sndctl(tp);
		nstate = SYN_SENT;
		soisconnecting(so);
		break;

	case PRU_ACCEPT:
		soisconnected(so);
		break;

	case PRU_DISCONNECT:
		if ((tp->tc_flags & TC_FIN_RCVD) == 0)
			goto abort;
		if (nstate < ESTAB)
			tcp_disconnect(tp);
		else {
			tp->tc_flags |= TC_SND_FIN;
			(void) tcp_sndctl(tp);
			tp->tc_flags |= TC_USR_CLOSED;
			soisdisconnecting(so);
		}
		break;

	case PRU_SHUTDOWN:
		switch (nstate) {

		case LISTEN:
		case SYN_SENT:
			nstate = CLOSED;
			break;

		case SYN_RCVD:
		case L_SYN_RCVD:
		case ESTAB:	
		case CLOSE_WAIT:
			tp->tc_flags |= TC_SND_FIN;
			(void) tcp_sndctl(tp);
			tp->tc_flags |= TC_USR_CLOSED;
			nstate = nstate != CLOSE_WAIT ? FIN_W1 : LAST_ACK;
			break;
			
		case FIN_W1:
		case FIN_W2:
		case TIME_WAIT:
		case CLOSING:
		case LAST_ACK:
		case RCV_WAIT:
			break;

		default:
			goto bad;
		}
		break;

	case PRU_RCVD:
		if (nstate < ESTAB || nstate == CLOSED)
			goto bad;
		tcp_sndwin(tp);
		if ((tp->tc_flags&TC_FIN_RCVD) &&
		    (tp->tc_flags&TC_USR_CLOSED) == 0 &&
		    rcv_empty(tp))
			error = ESHUTDOWN;
		if (nstate == RCV_WAIT && rcv_empty(tp))
			nstate = CLOSED;
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

abort:
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
#ifdef TCPDEBUG
	if (tdb.td_tod)
		tdb_stuff(&tdb, nstate);
#endif
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
tcp_newtcpcb()
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
	return (tp);
}

tcp_detach(tp)
	struct tcpcb *tp;
{
COUNT(TCP_DETACH);

	in_pcbfree(tp->t_inpcb);
	(void) m_free(dtom(tp));
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
		if (tp->t_rtl_val > tp->snd_una)		/* 36 */
			tcp_error(tp, EIO);		/* URXTIMO !?! */
		/*
		 * If user has already closed, abort the connection.
		 */
		if (tp->tc_flags & TC_USR_CLOSED) {
			tcp_abort(tp);
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

/*ARGSUSED*/
tcp_sense(m)
	struct mbuf *m;
{

COUNT(TCP_SENSE);
	return (EOPNOTSUPP);
}

tcp_error(tp, errno)
	struct tcpcb *tp;
	int errno;
{
	struct socket *so = tp->t_inpcb->inp_socket;

COUNT(TCP_ERROR);
	so->so_error = errno;
	sorwakeup(so);
	sowwakeup(so);
}

#ifdef TCPDEBUG
/*
 * TCP debugging utility subroutines.
 * THE NAMES OF THE FIELDS USED BY THESE ROUTINES ARE STUPID.
 */
tdb_setup(tp, n, input, tdp)
	struct tcpcb *tp;
	register struct tcpiphdr *n;
	int input;
	register struct tcp_debug *tdp;
{

COUNT(TDB_SETUP);
	tdp->td_tod = time;
	tdp->td_tcb = tp;
	tdp->td_old = tp->t_state;
	tdp->td_inp = input;
	tdp->td_tim = 0;
	tdp->td_new = -1;
	if (n) {
		tdp->td_sno = n->ti_seq;
		tdp->td_ano = n->ti_ackno;
		tdp->td_wno = n->t_win;
		tdp->td_lno = n->ti_len;
		tdp->td_flg = n->ti_flags;
	} else
		tdp->td_sno = tdp->td_ano = tdp->td_wno = tdp->td_lno =
		    tdp->td_flg = 0;
}

tdb_stuff(tdp, nstate)
	struct tcp_debug *tdp;
	int nstate;
{
COUNT(TDB_STUFF);

	tdp->td_new = nstate;
	tcp_debug[tdbx++ % TDBSIZE] = *tdp;
	if (tcpconsdebug & 2)
		tcp_prt(tdp);
}

tcp_prt(tdp)
	register struct tcp_debug *tdp;
{
COUNT(TCP_PRT);

	printf("%x ", ((int)tdp->td_tcb)&0xffffff);
	if (tdp->td_inp == INSEND) {
		printf("SEND #%x", tdp->td_sno);
		tdp->td_lno = ntohs(tdp->td_lno);
		tdp->td_wno = ntohs(tdp->td_wno);
	} else {
		if (tdp->td_inp == INRECV)
			printf("RCV #%x ", tdp->td_sno);
		printf("%s.%s",
		    tcpstates[tdp->td_old], tcpinputs[tdp->td_inp]);
		if (tdp->td_inp == ISTIMER)
			printf("(%s)", tcptimers[tdp->td_tim]);
		printf(" -> %s",
		    tcpstates[(tdp->td_new > 0) ? tdp->td_new : tdp->td_old]);
		if (tdp->td_new == -1)
			printf(" (FAILED)");
	}
	/* GROSS... DEPENDS ON SIGN EXTENSION OF CHARACTERS */
	if (tdp->td_lno)
		printf(" len=%d", tdp->td_lno);
	if (tdp->td_wno)
		printf(" win=%d", tdp->td_wno);
	if (tdp->td_flg & TH_FIN) printf(" FIN");
	if (tdp->td_flg & TH_SYN) printf(" SYN");
	if (tdp->td_flg & TH_RST) printf(" RST");
	if (tdp->td_flg & TH_EOL) printf(" EOL");
	if (tdp->td_flg & TH_ACK)  printf(" ACK %x", tdp->td_ano);
	if (tdp->td_flg & TH_URG) printf(" URG");
	printf("\n");
}
#endif
