/*	tp_usrreq.c	1.4	82/12/18	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/protosw.h"
#include "../netdecnet/dn_systm.h"
#include "../netdecnet/tp.h"
#include "../netdecnet/tp_var.h"

/*
 * Transport protocol interface to socket abstraction.
 * Used ONLY to initialize the Transport layer.  May be
 * used for routing control in the future.
 */

/*
 * Process a Transport user request.  Only allowed
 * operation is PRU_ATTACH to initialize the Transport
 * layer.
 */
tp_usrreq(so, req, m, addr)
	struct socket *so;
	int req;
	struct mbuf *m;
	caddr_t addr;
{
	int s = splimp();
	int error = 0;

	/*
	 */
	if (so->so_pcb != 0 || req != PRU_ATTACH) {
		splx(s);
		return (EINVAL);		/* XXX */
	}
	if (tpstate != TPS_HALT) {
		splx(s);
		return (0);
	}
	if (tp_linit() == 0) {
		splx(s);
		return (EIO);
	}
	sleep((caddr_t)&tpstate, PZERO+1);
	splx(s);
	return (0);
}

/*
 * Perform transport initialization for a line
 */
tp_linit()
{
	register struct mbuf *m;
	register struct tpin *t;
	register int n;

	m = m_get(MT_CANTWAIT, MT_HEADER);
	if (m == 0)
		return (0);
	m->m_off = MMINOFF;
	m->m_len = sizeof (struct tpin);
	t = mtod(m, struct tpin *);
	t->tpin_ctlflg = TP_INIT;
	AD_SHORT(t->tpin_srcnode, tp_host);
	t->tpin_tiinfo = TPINNT_NRT;
	AD_SHORT(t->tpin_blksize, 1024);
	t->tpin_ver[0] = 1;
	t->tpin_ver[1] = 3;
	t->tpin_ver[2] = 0;
	t->tpin_res = 0;
	n = (*tpifp->if_output)(tpifp, m, PF_DECNET);
	tpstate = TPS_TIS;
	m_freem(m);
	return (n);
}
