/*	raw_ip.c	4.1	81/11/29	*/

#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/in.h"
#include "../net/in_systm.h"

/*
 * Raw protocol interface.
 */
/*ARGSUSED*/
rip_input(m)
	struct mbuf *m;
{

COUNT(RIP_INPUT);

	/* call raw_input with prepared parameters */
}

/*ARGSUSED*/
rip_ctlinput(m)
	struct mbuf *m;
{

COUNT(RIP_CTLINPUT);

}

/*ARGSUSED*/
rip_output(m)
	struct mbuf *m;
{

COUNT(RIP_OUTPUT);

}

/*ARGSUSED*/
rip_usrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	caddr_t addr;
{

COUNT(RAW_USRREQ);

}

rip_slowtimo()
{

COUNT(RIP_SLOWTIMO);

}
