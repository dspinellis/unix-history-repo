/*	tcp_output.c	4.17	81/11/24	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/inet.h"
#include "../net/inet_pcb.h"
#include "../net/inet_systm.h"
#include "../net/imp.h"
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../net/tcp.h"
#include "../net/tcp_var.h"
#include "../net/tcp_fsm.h"
#include "/usr/include/errno.h"

/*
 * Special routines to send control messages.
 */
tcp_sndctl(tp)
	struct tcpcb *tp;
{
COUNT(TCP_SNDCTL);

        if (tcp_send(tp))
		return (1);
	tcp_sndnull(tp);
	return (0);
}

tcp_sndwin(tp)
	struct tcpcb *tp;
{
	int ihave, hehas;
COUNT(TCP_SNDWIN);

	if (tp->rcv_adv) {
		register struct socket *so = tp->t_inpcb->inp_socket;

		ihave = so->so_rcv.sb_hiwat -
		    (so->so_rcv.sb_cc + tp->seqcnt);
		hehas = tp->rcv_adv - tp->rcv_nxt;
		if ((100*(ihave-hehas)/so->so_rcv.sb_hiwat) < 35)
			return;
	}
        if (tcp_send(tp))
		return;
	tcp_sndnull(tp);
}

tcp_sndnull(tp)
	register struct tcpcb *tp;
{
COUNT(TCP_SNDNULL);

	(void) tcp_output(tp, 0, 0, (struct mbuf *)0);
        tp->tc_flags &= ~TC_ACK_DUE;
}

/*
 * Tcp segment output routine.
 */
tcp_send(tp)
	register struct tcpcb *tp;
{
	register unsigned long last, wind;
	register struct socket *so = tp->t_inpcb->inp_socket;
	struct mbuf *m;
	int flags = 0, forced, sent, len;

COUNT(TCP_SEND);
	tp->snd_lst = tp->snd_nxt;
	forced = 0;
	m = NULL;
	if (tp->snd_nxt == tp->iss) {
		flags |= TH_SYN;
		tp->snd_lst++;
	}
	last = tp->snd_off;
	for (m = so->so_snd.sb_mb; m != NULL; m = m->m_next)
		last += m->m_len;
	if (tp->snd_nxt > last) {
		if ((tp->tc_flags&TC_SND_FIN) &&
		    (tp->seq_fin == tp->iss || tp->snd_nxt <= tp->seq_fin)) {

			flags |= TH_FIN;
			tp->seq_fin = tp->snd_lst++;
		}
	} else {
		if (tp->tc_flags&TC_SYN_ACKED) {
			wind = tp->snd_una + tp->snd_wnd;
			tp->snd_lst = MIN(last, wind);
			if ((len = tp->snd_lst - tp->snd_nxt) > 1024)
				tp->snd_lst -= len - 1024;
			if (tp->snd_lst >= wind)
				tp->t_persist = T_PERS;
		}
		if ((tp->tc_flags&TC_FORCE_ONE) && (tp->snd_lst == wind)) {
			tp->snd_lst = tp->snd_nxt + 1;
			forced = 1;
		} else if (tp->snd_nxt >= tp->snd_lst && (tp->tc_flags&TC_SND_FIN) == 0)
			return (0);
		m = m_copy(so->so_snd.sb_mb,
		      (int)(MAX(tp->iss+1,tp->snd_nxt) - tp->snd_off),
		      (int)(tp->snd_lst - tp->snd_off));
		if (tp->snd_end > tp->iss && tp->snd_end <= tp->snd_lst)
			flags |= TH_EOL;
		if ((tp->tc_flags&TC_SND_FIN) && !forced &&
		    tp->snd_lst == last &&
		    (tp->seq_fin == tp->iss || tp->snd_nxt <= tp->seq_fin)) {
			flags |= TH_FIN;
			tp->seq_fin = tp->snd_lst++;
		}
	}
	if (tp->snd_nxt >= tp->snd_lst)
		return (0);
	if (tp->tc_flags & TC_SND_URG)
		flags |= TH_URG;
	sent = tcp_output(tp, flags, (int)(tp->snd_lst - tp->snd_nxt), m);
	if (!forced) {
		tp->t_rexmt = tp->t_xmtime;
		tp->t_rexmt_val = tp->snd_lst;
		if ((tp->tc_flags&TC_REXMT) == 0) {
			tp->t_rexmttl = T_REXMTTL;
			tp->t_rtl_val = tp->snd_lst;
		}
	}
	if (sent)
		tp->snd_nxt = tp->snd_lst;
	if ((tp->tc_flags&TC_SYN_ACKED) &&
	    tp->snd_una > tp->t_xmt_val) {
		tp->t_xmt = 0;
		tp->t_xmt_val = tp->snd_lst;
	}
	tp->tc_flags &= ~(TC_ACK_DUE|TC_REXMT|TC_FORCE_ONE);
	tp->snd_hi = MAX(tp->snd_nxt, tp->snd_hi);
	return (1);
}
tcp_output(tp, flags, len, dat)
	register struct tcpcb *tp;
	register int flags;
	int len;
	struct mbuf *dat;
{
	register struct tcpiphdr *t;			/* known to be r9 */
	register struct mbuf *m;
	struct socket *so = tp->t_inpcb->inp_socket;
	register struct ip *ip;
COUNT(TCP_OUTPUT);

	if ((t = tp->t_template) == 0)
		return (0);
	MGET(m, 0);
	if (m == 0)
		return (0);
	m->m_off = MMAXOFF - sizeof(struct tcpiphdr);
	m->m_len = sizeof (struct tcpiphdr);
	m->m_next = dat;
	if (flags & TH_SYN)
		len--;
	if (flags & TH_FIN)
		len--;
	if (len < 0)
		panic("tcp_output");
	bcopy((caddr_t)t, mtod(m, caddr_t), sizeof (struct tcpiphdr));
	t = mtod(m, struct tcpiphdr *);
	if (tp->tc_flags&TC_SND_RST) {
		flags &= ~TH_SYN;
		flags |= TH_RST;
	}
	if (tp->tc_flags&TC_SYN_RCVD)
		flags |= TH_ACK;
	t->ti_flags = flags;
	if (flags & TH_URG)
		t->ti_urp = htons((u_short)tp->snd_urp);	/*XXX */
	t->ti_win =
	    so->so_rcv.sb_hiwat -
		(so->so_rcv.sb_cc + tp->seqcnt);
	if (tp->rcv_nxt + t->ti_win > tp->rcv_adv)
		tp->rcv_adv = tp->rcv_nxt + t->ti_win;
	if (len)
		t->ti_len = htons((u_short)(len + sizeof (struct tcphdr)));
	t->ti_win = htons(t->ti_win);
	t->ti_seq = htonl(tp->snd_nxt);
	t->ti_ackno = htonl(tp->rcv_nxt);
	t->ti_sum = 0;		/* gratuitous? */
	t->ti_sum = inet_cksum(m, sizeof (struct tcpiphdr) + len);
	ip = (struct ip *)t;
	ip->ip_v = IPVERSION;
	ip->ip_hl = 5;
	ip->ip_tos = 0;
	ip->ip_len = len + sizeof(struct tcpiphdr);
	ip->ip_id = ip_id++;
	ip->ip_off = 0;
	ip->ip_ttl = MAXTTL;
	ip_send(ip);
	return (1);
}
