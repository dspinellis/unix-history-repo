/* in_pcb.c 4.3 81/11/18 */

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/inet.h"
#include "../net/inet_systm.h"
#include "../net/inet_host.h"
#include "../net/inet_pcb.h"

struct inpcb *
in_pcballoc()
{

	struct mbuf *m;

	m = m_getclr(M_WAIT);
	m->m_off = MMINOFF;
	return (mtod(m, struct inpcb *));
}

in_pcbenter(head, new)
	struct inpcb *head, *new;
{
	register struct inpcb *inp;

	for (inp = head->inp_next; inp != head; inp = inp->inp_next)
		if (inp->inp_fhost->h_addr.s_addr == new->inp_fhost.s_addr &&
		    inp->inp_fport == new->inp_fport &&
		    inp->inp_lhost->h_addr.s_addr = new->inp_fhost.s_addr &&
		    inp->inp_lport == new->inp_lport)
			return (EADDRINUSE);
	insque(new, head);
	return (0);
}

in_pcbfree(inp)
	struct inpcb *inp;
{
	struct socket *so = inp->inp_socket;

	if (so->so_isfilerefd == 0)
		sofree(so);
	else
		so->so_pcb = 0;
	if (inp->inp_lhost)
		in_hostfree(inp->inp_lhost);
	if (inp->inp_fhost)
		in_hostfree(inp->inp_fhost);
	(void) m_free(dtom(inp));
}

struct inpcb *
in_pcblookup(head, fhost, fport, lhost, lport)
	struct inpcb *head;
	struct in_addr *fhost, *lhost;
	u_short fport, lport;
{
	register struct inpcb *inp;

	for (inp = head->inp_next; inp != head; inp = inp->inp_next)
		if (inp->inp_fhost->h_addr.s_addr == fhost->s_addr &&
		    inp->inp_fport == fport &&
		    inp->inp_lhost->h_addr.s_addr == lhost->s_addr &&
		    inp->inp_lport == lport)
			return (inp);
	for (inp = head->inp_next; inp != head; inp = inp->inp_next)
		if ((inp->inp_fhost->h_addr.s_addr == fhost->s_addr ||
		     inp->inp_fhost == 0) &&
		    (inp->inp_fport == fport || inp->inp_fport == 0) &&
		     inp->inp_lhost->h_addr.s_addr == lhost->s_addr &&
		    (inp->inp_lport == lport || inp->inp_lport == 0))
			return (inp);
	return (0);
}

in_pcbgenport(head)
	struct inpcb *head;
{
	register struct inpcb *inp;

again:
	if (head->inp_lport++ < 1024)
		head->inp_lport = 1024;
	for (inp = head->inp_next; inp != head; inp = inp->inp_next)
		if (inp->inp_lport == head->inp_lport)
			goto again;
	return (head->inp_lport);
}
