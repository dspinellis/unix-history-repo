/* tcp_output.c 4.9 81/11/04 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../inet/inet_cksum.h"
#include "../inet/inet.h"
#include "../inet/inet_host.h"
#include "../inet/inet_systm.h"
#include "../inet/imp.h"
#include "../inet/ip.h"
#include "../inet/tcp.h"
#include "../inet/tcp_fsm.h"

/*
 * Special routines to send control messages.
 */
tcp_sndctl(tp)
	struct tcb *tp;
{
COUNT(TCP_SNDCTL);

        if (tcp_send(tp))
		return (1);
	tcp_sndnull(tp);
	return(0);
}

tcp_sndwin(tp)
	struct tcb *tp;
{
	int ihave, hehas;
COUNT(TCP_SNDWIN);

	if (tp->rcv_adv) {
		ihave = tp->t_ucb->uc_rhiwat -
		    (tp->t_ucb->uc_rcc + tp->seqcnt);
		hehas = tp->rcv_adv - tp->rcv_nxt;
		if ((100*(ihave-hehas)/tp->t_ucb->uc_rhiwat) < 35)
			return;
	}
        if (tcp_send(tp))
		return (1);
	tcp_sndnull(tp);
	return (0);
}

tcp_sndnull(tp)
	register struct tcb *tp;
{
COUNT(TCP_SNDNULL);

	tcp_output(tp, 0, 0, (struct mbuf *)0);
        tp->tc_flags &= ~TC_ACK_DUE;
}

tcp_sndrst(tp, n)
	register struct tcb *tp;
	register struct th *n;
{
COUNT(TCP_SNDRST);

        /* don't send a reset in response to a reset */
	if (n->th_flags&TH_RST)
		return;
	tp->tc_flags |= TC_SND_RST;
	if (n->th_flags&TH_ACK)
		tp->snd_nxt = n->t_ackno;
	tp->tc_flags &= ~TC_SYN_RCVD;
	tcp_sndnull(tp);
	tp->tc_flags &= ~TC_SND_RST;
}

/*
 * Tcp segment output routine.
 */
tcp_send(tp)
	register struct tcb *tp;
{
	register struct ucb *up;
	register unsigned long last, wind;
	struct mbuf *m;
	int flags = 0, forced, sent;
	struct mbuf *tcp_sndcopy();
	int len;

COUNT(TCP_SEND);
	up = tp->t_ucb;
	tp->snd_lst = tp->snd_nxt;
	forced = 0;
	m = NULL;
	if (tp->snd_nxt == tp->iss) {
		flags |= TH_SYN;
		tp->snd_lst++;
	}
	last = tp->snd_off;
	for (m = up->uc_sbuf; m != NULL; m = m->m_next)
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
			tp->snd_lst = min(last, wind);
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
		m = tcp_sndcopy(tp, MAX(tp->iss+1,tp->snd_nxt), tp->snd_lst);
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
	sent = tcp_output(tp, flags, tp->snd_lst - tp->snd_nxt, m);
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

/*
 * Create template to be used to send tcp packets on a connection.
 * Call after host entry created, allocates an mbuf and fills
 * in a skeletal tcp/ip header, minimizing the amount of work
 * necessary when the connection is used.
 */
struct th *
tcp_template(tp)
	struct tcb *tp;
{
	register struct host *h = tp->t_ucb->uc_host;
	register struct mbuf *m;
	register struct th *n;
	register struct ip *ip;

	if (h == 0)
		return (0);
	m = m_get(1);
	if (m == 0)
		return (0);
	m->m_off = MMAXOFF - sizeof (struct th);
	m->m_len = sizeof (struct th);
	n = mtod(m, struct th *);
	n->t_next = n->t_prev = 0;
	n->t_x1 = 0;
	n->t_pr = IPPROTO_TCP;
	n->t_len = htons(sizeof (struct th) - sizeof (struct ip));
	n->t_s.s_addr = n_lhost.s_addr;
	n->t_d.s_addr = h->h_addr.s_addr;
	n->t_src = htons(tp->t_lport);
	n->t_dst = htons(tp->t_fport);
	n->t_seq = 0;
	n->t_ackno = 0;
	n->t_x2 = 0;
	n->t_off = 5;
	n->th_flags = 0;
	n->t_win = 0;
	n->t_sum = 0;
	n->t_urp = 0;
	return (n);
}

tcp_output(tp, flags, len, dat)
	register struct tcb *tp;
	register int flags;
	int len;
	struct mbuf *dat;
{
	register struct th *t;			/* known to be r9 */
	register struct mbuf *m;
	register struct ip *ip;
	int i;
#ifdef TCPDEBUG
	struct tcp_debug tdb;
#endif
COUNT(TCP_OUTPUT);

	if ((t = tp->t_template) == 0)
		return (0);
	MGET(m, 0);
	if (m == 0)
		return (0);
	m->m_off = MMAXOFF - sizeof(struct th);
	m->m_len = sizeof (struct th);
	m->m_next = dat;
	if (flags & TH_SYN)
		len--;
	if (flags & TH_FIN)
		len--;
	bcopy((caddr_t)t, mtod(m, caddr_t), sizeof (struct th));
	t = mtod(m, struct th *);
	if (tp->tc_flags&TC_SND_RST) {
		flags &= ~TH_SYN;
		flags |= TH_RST;
	}
	if (tp->tc_flags&TC_SYN_RCVD)
		flags |= TH_ACK;
	t->th_flags = flags;
	if (flags & TH_URG)
		t->t_urp = htons(tp->snd_urp);
	t->t_win =
	    tp->t_ucb->uc_rhiwat - (tp->t_ucb->uc_rcc + tp->seqcnt);
	if (tp->rcv_nxt + t->t_win > tp->rcv_adv)
		tp->rcv_adv = tp->rcv_nxt + t->t_win;
	if (len)
		t->t_len = htons(len + TCPSIZE);
	t->t_win = htons(t->t_win);
#ifdef TCPDEBUG
	if ((tp->t_ucb->uc_flags & UDEBUG) || tcpconsdebug) {
		t->t_seq = tp->snd_nxt;
		t->t_ackno = tp->rcv_nxt;
		tdb_setup(tp, t, INSEND, &tdb);
		tdb_stuff(&tdb, -2);
	}
#endif
	t->t_seq = htonl(tp->snd_nxt);
	t->t_ackno = htonl(tp->rcv_nxt);
	t->t_sum = 0;		/* gratuitous? */
	CKSUM_TCPSET(m, t, r9, sizeof (struct th) + len);
	ip = (struct ip *)t;
	ip->ip_v = IPVERSION;
	ip->ip_hl = 5;
	ip->ip_tos = 0;
	ip->ip_len = len + sizeof(struct th);
	ip->ip_id = ip_id++;
	ip->ip_off = 0;
	ip->ip_ttl = MAXTTL;
	i = ip_send(ip);
	return(i);
}

firstempty(tp)
	register struct tcb *tp;
{
	register struct th *p, *q;
COUNT(FIRSTEMPTY);

	if ((p = tp->t_rcv_next) == (struct th *)tp || tp->rcv_nxt < p->t_seq)
		return (tp->rcv_nxt);
	while ((q = p->t_next) != (struct th *)tp &&
	    (t_end(p) + 1) == q->t_seq)
		p = q;
	return (t_end(p) + 1);
}

struct mbuf *
tcp_sndcopy(tp, start, end)
	struct tcb *tp;
	u_long start, end;
{
	register struct mbuf *m, *n, **np;
	u_long off;
	register int len;
	int adj;
	struct mbuf *top, *p;
COUNT(TCP_SNDCOPY);

	if (start >= end)    
		return(NULL);
	off = tp->snd_off;
	m = tp->t_ucb->uc_sbuf;
	while (m != NULL && start >= (off + m->m_len)) {
		off += m->m_len;
		m = m->m_next;
	}
	np = &top;
	top = 0;
	adj = start - off;
	len = end - start;
	while (m && len > 0) {
		MGET(n, 1);
		*np = n;
		if (n == 0)
			goto nospace;
		n->m_len = MIN(len, m->m_len - adj);
		if (m->m_off > MMAXOFF) {
			p = mtod(m, struct mbuf *);
			n->m_off = ((int)p - (int)n) + adj;
			mprefcnt[mtopf(p)]++;
		} else {
			n->m_off = MMINOFF;
			bcopy(mtod(m, caddr_t)+adj, mtod(n, caddr_t),
			    n->m_len);
		}
		len -= n->m_len;
		adj = 0;
		m = m->m_next;
		/* SHOULD TRY PACKING INTO SMALL MBUFS HERE */
		np = &n->m_next;
	}
	/* SHOULD NEVER RUN OUT OF m WHEN LEN */
	if (len)
		printf("snd_copy: m %x len %d\n", m, len);
	return (top);
nospace:
	printf("snd_copy: no space\n");
	m_freem(top);
	return (0);
}
