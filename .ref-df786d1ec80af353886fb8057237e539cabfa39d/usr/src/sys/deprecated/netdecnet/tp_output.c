/*	tp_output.c	1.3	82/10/09	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../net/if.h"
#include "../netdecnet/tp.h"
#include "../netdecnet/tp_var.h"

/*
 * Transport output routine.  Fill in the
 * transport header and pass it off to the
 * interface.
 */
tp_output(m, dstnode)
	register struct mbuf *m;
	u_short dstnode;
{
	register struct tprh *t;

	if (tpstate != TPS_RUN)
		return (1);
	if (dstnode > tprp.tprp_nn)		/* node number out of range? */
		return (1);
	m->m_off -= sizeof (struct tprh);
	m->m_len += sizeof (struct tprh);
	t = mtod(m, struct tprh *);
	t->tprh_rtflg = TP_RH;
	AD_SHORT(t->tprh_srcnode, tp_host);
	AD_SHORT(t->tprh_dstnode, dstnode);
	t->tprh_forward = 0;
	return ((*tpifp->if_output)(tpifp, m, PF_DECNET));
}
