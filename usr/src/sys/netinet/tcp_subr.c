/* tcp_subr.c 4.3 81/11/26 */

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
#define TCPFSTAB
#include "../net/tcp_fsm.h"
#include "../net/tcp_seq.h"
#include "../net/tcp_timer.h"
#include "../net/tcp_var.h"
#include "../net/tcpip.h"
#include "/usr/include/errno.h"

/*
 * Tcp initialization
 */
tcp_init()
{

COUNT(TCP_INIT);
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
	n->ti_ack = 0;
	n->ti_x2 = 0;
	n->ti_off = 5;
	n->ti_flags = 0;
	n->ti_win = 0;
	n->ti_sum = 0;
	n->ti_urp = 0;
	return (n);
}

/*
 * Send a reset message back to send of TCP segment ti,
 * with ack, seq and flags fields as specified by parameters.
 */
tcp_respond(ti, ack, seq, flags)
	register struct tcpiphdr *ti;
	tcp_seq ack, seq;
	int flags;
{
	struct mbuf *m = dtom(ti);

COUNT(TCP_RESPOND);
	m_freem(m->m_next);
	m->m_next = 0;
	m->m_len = sizeof(struct tcpiphdr);
#define xchg(a,b,type) { type t; t=a; a=b; b=t; }
	xchg(ti->ti_dst.s_addr, ti->ti_src.s_addr, u_long);
	xchg(ti->ti_dport, ti->ti_sport, u_short);
#undef xchg
	ti->ti_next = ti->ti_prev = 0;
	ti->ti_x1 = 0;
	ti->ti_len = htons(sizeof (struct tcphdr));
	ti->ti_seq = htonl(seq);
	ti->ti_ack = htonl(ack);
	ti->ti_x2 = 0;
	ti->ti_off = sizeof (struct tcphdr) >> 2;
	ti->ti_flags = flags;
	ti->ti_win = ti->ti_urp = 0;
	ti->ti_sum = in_cksum(m, sizeof(struct tcpiphdr));
	((struct ip *)ti)->ip_len = sizeof(struct tcpiphdr);
	((struct ip *)ti)->ip_ttl = TCP_TTL;
	ip_output(m, (struct mbuf *)0);
}

/*
 * Create a new TCP control block, making an
 * empty reassembly queue and hooking it to the argument
 * protocol control block.
 */
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
	tp->seg_next = tp->seg_prev = (struct tcpiphdr *)tp;
	tp->t_maxseg = 1024;
	tp->t_inpcb = inp;
	inp->inp_ppcb = (caddr_t)tp;
	return (tp);
}

/*
 * Drop a TCP connection, reporting
 * the specified error.  If connection is synchronized,
 * then send a RST to peer.
 */
tcp_drop(tp, errno)
	struct tcpcb *tp;
	int errno;
{
	struct socket *so = tp->t_inpcb->inp_socket;

COUNT(TCP_DROP);
	if (TCPS_HAVERCVDSYN(tp->t_state) &&
	    TCPS_OURFINNOTACKED(tp->t_state)) {
		tp->t_state = TCPS_CLOSED;
		tcp_output(tp);
	}
	so->so_error = errno;
	tcp_close(tp);
}

/*
 * Close a TCP control block:
 *	discard all space held by the tcp
 *	discard internet protocol block
 *	wake up any sleepers
 */
tcp_close(tp)
	register struct tcpcb *tp;
{
	register struct tcpiphdr *t;
	struct socket *so = tp->t_inpcb->inp_socket;

COUNT(TCP_CLOSE);
	t = tp->seg_next;
	for (; t != (struct tcpiphdr *)tp; t = (struct tcpiphdr *)t->ti_next)
		m_freem(dtom(t));
	if (tp->t_template)
		(void) m_free(dtom(tp->t_template));
	if (tp->t_tcpopt)
		(void) m_free(dtom(tp->t_tcpopt));
	if (tp->t_ipopt)
		(void) m_free(dtom(tp->t_ipopt));
	in_pcbfree(tp->t_inpcb);
	(void) m_free(dtom(tp));
	socantrcvmore(so);
	socantsendmore(so);
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
