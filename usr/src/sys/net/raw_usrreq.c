/*	raw_usrreq.c	4.1	81/11/29	*/

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/mtpr.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"

/*
 * Raw protocol interface.
 */
raw_input(m, pf, af)
	struct mbuf *m;
	struct sockproto pf;
	struct sockaddr af;
{
	struct mbuf *mh;
	struct sockproto *pfp;
	int s;

	mh = m_get(0);
	if (mh == 0)
		goto drop;
	mh->m_next = m;
	mh->m_off = MMINOFF + sizeof (struct sockproto);
	*mtod(m, struct sockaddr *) = af;
	mh->m_off = MMINOFF;
	*mtod(m, struct sockproto *) = pf;
	mh->m_len = sizeof (struct sockproto) + sizeof (struct sockaddr);
	s = splimp();
	IF_ENQUEUE(&rawintrq, mh);
	splx(s);
	setrawintr();
	return;
drop:
	m_freem(m);
}

rawintr()
{
	int s;
	struct mbuf *m;

COUNT(RAWINTR);
next:
	s = splimp();
/*###45 [cc] rawintrq undefined %%%*/
	IF_DEQUEUE(&rawintrq, m);
	splx(s);
	if (m == 0)
		return;
	/* ... */
	goto drop;
drop:
	m_freem(m);
	goto next;
}

/*ARGSUSED*/
raw_usrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	caddr_t addr;
{

COUNT(RAW_USRREQ);

}
