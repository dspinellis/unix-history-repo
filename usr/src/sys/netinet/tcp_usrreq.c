/* tcp_usrreq.c 1.15 81/10/30 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../inet/inet.h"
#include "../inet/inet_systm.h"
#include "../inet/imp.h"
#include "../inet/ip.h"
#include "../inet/tcp.h"
#define TCPFSTAB
#ifdef TCPDEBUG
#define TCPSTATES
#endif
#include "../inet/tcp_fsm.h"

tcp_timeo()
{
	register struct tcb *tp;
	int s = splnet();
COUNT(TCP_TIMEO);

	/*
	 * Search through tcb's and update active timers.
	 */
	for (tp = tcb_head; tp != NULL; tp = tp->t_tcb_next) {
		if (tp->t_init != 0 && --tp->t_init == 0)
			tcp_usrreq(ISTIMER, TINIT, tp, 0);
		if (tp->t_rexmt != 0 && --tp->t_rexmt == 0)
			tcp_usrreq(ISTIMER, TREXMT, tp, 0);
		if (tp->t_rexmttl != 0 && --tp->t_rexmttl == 0)
			tcp_usrreq(ISTIMER, TREXMTTL, tp, 0);
		if (tp->t_persist != 0 && --tp->t_persist == 0)
			tcp_usrreq(ISTIMER, TPERSIST, tp, 0);
		if (tp->t_finack != 0 && --tp->t_finack == 0)
			tcp_usrreq(ISTIMER, TFINACK, tp, 0);
		tp->t_xmt++;
	}
	tcp_iss += ISSINCR;		/* increment iss */
	timeout(tcp_timeo, 0, hz);      /* reschedule every second */
	splx(s);
}

tcp_usrreq(input, timertype, tp, mp)
	int input, timertype;
	register struct tcb *tp;
	struct mbuf *mp;
{
	int s = splnet();
	register int nstate;
#ifdef TCPDEBUG
	struct tcp_debug tdb;
#endif
COUNT(TCP_USRREQ);

	nstate = tp->t_state;
	tp->tc_flags &= ~TC_NET_KEEP;
	acounts[nstate][input]++;
#ifdef TCPDEBUG
	if ((tp->t_ucb->uc_flags & UDEBUG) || tcpconsdebug) {
		tdb_setup(tp, (struct th *)0, input, &tdb);
		tdb.td_tim = timertype;
	} else
		tdb.td_tod = 0;
#endif
	switch (tcp_fstab[nstate][input]) {

	default:
		printf("tcp: bad state: tcb=%x state=%d input=%d\n",
		    tp, tp->t_state, input);
		nstate = EFAILEC;
		break;

	case LIS_CLS:				/* 1 */
		t_open(tp, PASSIVE);
		nstate = LISTEN;
		break;

	case SYS_CLS:				/* 2 */
		t_open(tp, ACTIVE);
		send_ctl(tp);
		nstate = SYN_SENT;
		break;

	case CLS_OPN:				/* 10 */
		t_close(tp, UCLOSED);
		nstate = CLOSED;
		break;

	case CL2_CLW:				/* 10 */
		tp->tc_flags |= TC_SND_FIN;
		send_ctl(tp);
		tp->tc_flags |= TC_USR_CLOSED;
		nstate = CLOSING2;
		break;

	case TIMERS:				/* 14,17,34,35,36,37,38 */
		nstate = tcp_timers(tp, timertype);
		break;

	case CLS_RWT:				/* 20 */
		present_data(tp);
		if (rcv_empty(tp)) {
			t_close(tp, UCLOSED);
			nstate = CLOSED;
		} else
			nstate = RCV_WAIT;
		break;

	case FW1_SYR:				/* 24,25 */
		tp->tc_flags |= TC_SND_FIN;
		send_ctl(tp);
		tp->tc_flags |= TC_USR_CLOSED;
		nstate = FIN_W1;
		break;

	case SSS_SND:				/* 40,41 */
		nstate = sss_send(tp, mp);
		break;

	case SSS_RCV:				/* 42 */
		send_ctl(tp);		/* send new window */
		present_data(tp);
		break;

	case CLS_NSY:				/* 44 */
		t_close(tp, UABORT);
		nstate = CLOSED;
		break;

	case CLS_SYN:				/* 45 */
		tp->tc_flags |= TC_SND_RST;
		send_null(tp);
		t_close(tp, UABORT);
		nstate = CLOSED;
		break;

	case CLS_ACT:				/* 47 */
		t_close(tp, UNETDWN);
		nstate = CLOSED;
		break;

	case NOP:
		break;

	case CLS_ERR:
		to_user(tp->t_ucb, UCLSERR);
		break;
	}
#ifdef TCPDEBUG
	if (tdb.td_tod)
		tdb_stuff(&tdb, nstate);
#endif
	/* YECH */
	switch (nstate) {

	case CLOSED:
	case SAME:
		break;

	case EFAILEC:
		if (mp)
			m_freem(dtom(mp));
		break;

	default:
		tp->t_state = nstate;
		break;
	}
	splx(s);
}

t_open(tp, mode)                /* set up a tcb for a connection */
	register struct tcb *tp;
	int mode;
{
	register struct ucb *up;
COUNT(T_OPEN);

	/* enqueue the tcb */

	if (tcb_head == NULL) {
		tcb_head = tp;
		tcb_tail = tp;
	} else {
		tp->t_tcb_next = tcb_head;
		tcb_head->t_tcb_prev = tp;
		tcb_head = tp;
	}

	/* initialize non-zero tcb fields */

	tp->t_rcv_next = (struct th *)tp;
	tp->t_rcv_prev = (struct th *)tp;
	tp->t_xmtime = T_REXMT;
	tp->snd_end = tp->seq_fin = tp->snd_nxt = tp->snd_hi =
	              tp->snd_una = tp->iss = tcp_iss;
	tp->snd_off = tp->iss + 1;
	tcp_iss += (ISSINCR >> 1) + 1;

	/* set timeout for open */

	up = tp->t_ucb;
	tp->t_init = (up->uc_timeo != 0 ? up->uc_timeo :
					(mode == ACTIVE ? T_INIT : 0));
	up->uc_timeo = 0;       /* overlays uc_ssize */
}

t_close(tp, state)
	register struct tcb *tp;
	short state;
{
	register struct ucb *up;
	register struct th *t;
	register struct mbuf *m;
COUNT(T_CLOSE);

	up = tp->t_ucb;

	tp->t_init = tp->t_rexmt = tp->t_rexmttl = tp->t_persist = 
	    tp->t_finack = 0;

	/* delete tcb */

	if (tp->t_tcb_prev == NULL)
		tcb_head = tp->t_tcb_next;
	else
		tp->t_tcb_prev->t_tcb_next = tp->t_tcb_next;
	if (tp->t_tcb_next == NULL)
		tcb_tail = tp->t_tcb_prev;
	else
		tp->t_tcb_next->t_tcb_prev = tp->t_tcb_prev;

	/* free all data on receive and send buffers */

	for (t = tp->t_rcv_next; t != (struct th *)tp; t = t->t_next)
		m_freem(dtom(t));

	if (up->uc_rbuf != NULL) {
		m_freem(up->uc_rbuf);
		up->uc_rbuf = NULL;
	}
	up->uc_rcc = 0;
	if (up->uc_sbuf != NULL) {
		m_freem(up->uc_sbuf);
		up->uc_sbuf = NULL;
	}
	up->uc_ssize = 0;
	for (m = tp->t_rcv_unack; m != NULL; m = m->m_act) {
		m_freem(m);
		tp->t_rcv_unack = NULL;
	}
	if (up->uc_template) {
		m_free(dtom(up->uc_template));
		up->uc_template = 0;
	}
	wmemfree((caddr_t)tp, 1024);
	up->uc_tcb = NULL;

	/* lower buffer allocation and decrement host entry */

	mbstat.m_lowat -= up->uc_snd + (up->uc_rhiwat/MSIZE) + 2;
	mbstat.m_hiwat = 2 * mbstat.m_lowat;
	if (up->uc_host != NULL) {
		h_free(up->uc_host);
		up->uc_host = NULL;
	}

	/* if user has initiated close (via close call), delete ucb
	   entry, otherwise just wakeup so user can issue close call */

	if (tp->tc_flags&TC_USR_ABORT)
        	up->uc_proc = NULL;
	else
        	to_user(up, state);
}

sss_send(tp, m0)
	register struct tcb *tp;
	struct mbuf *m0;
{
	register struct mbuf *m, *n;
	register struct ucb *up = tp->t_ucb;
	register off;
	seq_t last;
COUNT(SSS_SEND);

	last = tp->snd_off;
	for (m = n = m0; m != NULL; m = m->m_next) {
		up->uc_ssize++;
		if (m->m_off > MMAXOFF)
			up->uc_ssize += NMBPG;
		last += m->m_len;
	}
	if ((m = up->uc_sbuf) == NULL)
		up->uc_sbuf = n;
	else {
		while (m->m_next != NULL) {
			m = m->m_next;
			last += m->m_len;
		}
		if (m->m_off <= MMAXOFF) {
			last += m->m_len;
			off = m->m_off + m->m_len;
			while (n && n->m_off <= MMAXOFF &&
			    (MMAXOFF - off) >= n->m_len) {
				bcopy((caddr_t)((int)n + n->m_off),
				      (caddr_t)((int)m + off), n->m_len);
				m->m_len += n->m_len;
				off += n->m_len;
				up->uc_ssize--;
				n = m_free(n);
			}
		}
		m->m_next = n;
	}
	if (up->uc_flags & UEOL)
		tp->snd_end = last;
	if (up->uc_flags & UURG) {
		tp->snd_urp = last+1;
		tp->tc_flags |= TC_SND_URG;
	}
	send(tp);
	return (SAME);
}

tcp_timers(tp, timertype)
	register struct tcb *tp;
	int timertype;
{

COUNT(TCP_TIMERS);
	switch (timertype) {

	case TINIT:		/* initialization timer */
		if ((tp->tc_flags&TC_SYN_ACKED) == 0) {		/* 35 */
			t_close(tp, UINTIMO);
			return (CLOSED);
		}
		return (SAME);

	case TFINACK:		/* fin-ack timer */
		switch (tp->t_state) {

		case TIME_WAIT:
			/*
			 * We can be sure our ACK of foreign FIN was rcvd,
			 * and can close if no data left for user.
			 */
			if (rcv_empty(tp)) {
				t_close(tp, UCLOSED);		/* 14 */
				return (CLOSED);
			}
			return (RCV_WAIT);			/* 17 */

		case CLOSING1:
			tp->tc_flags |= TC_WAITED_2_ML;
			return (SAME);

		default:
			return (SAME);
		}

	case TREXMT:		/* retransmission timer */
		if (tp->t_rexmt_val > tp->snd_una) {	 	/* 34 */
			/*
			 * Set up for a retransmission, increase rexmt time
			 * in case of multiple retransmissions.
			 */
			tp->snd_nxt = tp->snd_una;
			tp->tc_flags |= TC_REXMT;
			tp->t_xmtime = tp->t_xmtime << 1;
			if (tp->t_xmtime > T_REMAX)
				tp->t_xmtime = T_REMAX;
			send(tp);
		}
		return (SAME);

	case TREXMTTL:		/* retransmit too long */
		if (tp->t_rtl_val > tp->snd_una)		/* 36 */
			to_user(tp->t_ucb, URXTIMO);
		/*
		 * If user has already closed, abort the connection.
		 */
		if (tp->tc_flags & TC_USR_CLOSED) {
			t_close(tp, URXTIMO);
			return (CLOSED);
		}
		return (SAME);

	case TPERSIST:		/* persist timer */
		/*
		 * Force a byte send through closed window.
		 */
		tp->tc_flags |= TC_FORCE_ONE;
		send(tp);
		return (SAME);
	}
	panic("tcp_timers");
}

/* THIS ROUTINE IS A CROCK */
to_user(up, state)
	register struct ucb *up;
	register short state;
{
COUNT(TO_USER);

	up->uc_state |= state;
	netwakeup(up);
  	if (state == UURGENT)
		psignal(up->uc_proc, SIGURG);
}

#ifdef TCPDEBUG
tcp_prt(tdp)
	register struct tcp_debug *tdp;
{
COUNT(TCP_PRT);

	printf("TCP(%x) %s x %s",
	    tdp->td_tcb, tcpstates[tdp->td_old], tcpinputs[tdp->td_inp]);
	if (tdp->td_inp == ISTIMER)
		printf("(%s)", tcptimers[tdp->td_tim]);
	printf(" --> %s",
	    tcpstates[(tdp->td_new > 0) ? tdp->td_new : tdp->td_old]);
	/* GROSS... DEPENDS ON SIGN EXTENSION OF CHARACTERS */
	if (tdp->td_new < 0)
		printf(" (FAILED)");
	if (tdp->td_sno) {
		printf(" sno %x ano %x win %d len %d flags %x",
		    tdp->td_sno, tdp->td_ano, tdp->td_wno, tdp->td_lno, tdp->td_flg);
	}
	printf("\n");
}
#endif
#ifdef TCPDEBUG
tdb_setup(tp, n, input, tdp)
	struct tcb *tp;
	register struct th *n;
	int input;
	register struct tcp_debug *tdp;
{

	tdp->td_tod = time;
	tdp->td_tcb = tp;
	tdp->td_old = tp->t_state;
	tdp->td_inp = input;
	tdp->td_tim = 0;
	tdp->td_new = -1;
	if (n) {
		tdp->td_sno = n->t_seq;
		tdp->td_ano = n->t_ackno;
		tdp->td_wno = n->t_win;
		tdp->td_lno = n->t_len;
		tdp->td_flg = n->th_flags;
	} else
		tdp->td_sno = tdp->td_ano = tdp->td_wno = tdp->td_lno =
		    tdp->td_flg = 0;
}

tdb_stuff(tdp, nstate)
	struct tcp_debug *tdp;
	int nstate;
{

	tdp->td_new = nstate;
	tcp_debug[tdbx++ % TDBSIZE] = *tdp;
	if (tcpconsdebug & 2)
		tcp_prt(tdp);
}
#endif
