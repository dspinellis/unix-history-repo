/* tcp_subr.c 4.2 81/11/25 */

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
 * Create template to be used to send tcp packets on a connection.
 * Call after host entry created, allocates an mbuf and fills
 * in a skeletal tcp/ip header, minimizing the amount of work
 * necessary when the connection is used.
 */
struct tcpiphdr *
tcp_template(tp)
	struct tcpcb *tp;
{
	register struct inpcb *inp = tp->t_inpcb;
	register struct mbuf *m;
	register struct tcpiphdr *n;

COUNT(TCP_TEMPLATE);
	m = m_get(1);
	if (m == 0)
		return (0);
	m->m_off = MMAXOFF - sizeof (struct tcpiphdr);
	m->m_len = sizeof (struct tcpiphdr);
	n = mtod(m, struct tcpiphdr *);
	n->ti_next = n->ti_prev = 0;
	n->ti_x1 = 0;
	n->ti_pr = IPPROTO_TCP;
	n->ti_len = htons(sizeof (struct tcpiphdr) - sizeof (struct ip));
	n->ti_src = inp->inp_laddr;
	n->ti_dst = inp->inp_faddr;
	n->ti_sport = inp->inp_lport;
	n->ti_dport = inp->inp_fport;
	n->ti_seq = 0;
	n->ti_ackno = 0;
	n->ti_x2 = 0;
	n->ti_off = 5;
	n->ti_flags = 0;
	n->ti_win = 0;
	n->ti_sum = 0;
	n->ti_urp = 0;
	return (n);
}

/*
 * Reflect a control message back to sender of tcp segment ti,
 * with ack, seq and flags fields as specified by parameters.
 */
tcp_reflect(ti, ack, seq, flags)
	register struct tcpiphdr *ti;
	tcpseq_t ack, seq;
	int flags;
{

	m_freem(m->m_next);
	m->m_next = 0;
	m->m_len = sizeof(struct tcpiphdr);
#define xchg(a,b) j=a; a=b; b=j
	xchg(ti->ti_dst.s_addr, ti->ti_src.s_addr);
	xchg(ti->ti_dport, ti->ti_sport);
#undef xchg
	ti->ti_ack = htonl(ack);
	ti->ti_seq = htonl(seq);
	ti->ti_flags = flags;

	ti->ti_len = htons(sizeof (struct tcphdr));
	ti->ti_off = 5;
	ti->ti_sum = inet_cksum(m, sizeof(struct tcpiphdr));
	((struct ip *)ti)->ip_len = sizeof(struct tcpiphdr);
	((struct ip *)ti)->ip_ttl = MAXTTL;
	ip_output(m);
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

tcp_drop(tp, errno)
	struct tcpcb *tp;
	int errno;
{
	struct socket *so = tp->t_inpcb->inp_socket;

COUNT(TCP_DROP);
	if (TCPS_HAVERCVDSYN(tp->t_state) &&
	    TCPS_OURFINISACKED(tp->t_state) == 0) {
		tp->t_state = TCPS_CLOSED;
		tcp_output(tp);
	}
	so->so_error = errno;
	socantrcvmore(so);
	socantsndmore(so);
	tcp_close(tp);
}

tcp_close(tp)
	register struct tcpcb *tp;
{
	register struct tcpiphdr *t;

COUNT(TCP_CLOSE);
	tcp_canceltimers(tp);
	t = tp->seg_next;
	for (; t != (struct tcpiphdr *)tp; t = (struct tcpiphdr *)t->ti_next)
		m_freem(dtom(t));
	if (tp->t_template) {
		(void) m_free(dtom(tp->t_template));
		tp->t_template = 0;
	}
	in_pcbfree(tp->t_inpcb);
	(void) m_free(dtom(tp));
}

/*ARGSUSED*/
tcp_sense(m)
	struct mbuf *m;
{

COUNT(TCP_SENSE);
	return (EOPNOTSUPP);
}

tcp_drain()
{
	register struct inpcb *ip;

COUNT(TCP_DRAIN);
}

tcp_ctlinput(m)
	struct mbuf *m;
{

COUNT(TCP_CTLINPUT);
	m_freem(m);
}
