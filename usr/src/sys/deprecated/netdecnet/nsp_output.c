/*	nsp_output.c	1.5	82/12/18	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../netdecnet/dn_systm.h"
#include "../netdecnet/nsp.h"
#include "../netdecnet/nsp_var.h"
#include <errno.h>

/*
 * NSP output routine: figure out what should be sent and send it.
 */
nsp_output(np)
	register struct nspcb *np;
{
	register struct socket *so = np->n_socket;
	register int len;
	int off, flags;
	register struct mbuf *m;


	/*
	 * Determine what type of message to send and send it.
	 */
top:
	/* interrupt to be sent? */
	if (np->n_flags & NF_INTAVAIL) && np->nf_remint > 0 &&
	    (np->n_flags & NF_OTHSENT) == 0) {
		register struct nspi *n;

		m = m_get(M_CANTWAIT, MT_HEADER);
		if (m == 0)
			return (0);
		if (np->nb_xmt)
			len = np->nb_xmt->m_len;
		else
			len = 0;
		m->m_len = sizeof (struct nspi) + len;
		m->m_off = MMAXOFF - m->m_len;
		n = mtod(m, struct nspi *);
		n->nsp_msgflg = NSP_INTR;
		n->nsp_dstaddr = np->n_rem;
		n->nsp_srcaddr = np->n_loc;
		n->nsp_acknum = NSPA_ACK | np->na_xmtoth;
		n->nsp_segnum = np->nn_oth;
		if (len)
			bcopy((char *)(n + 1), mtod(np->nb_xmt, char *), len);
		if (tp_output(m, np->n_node)) {
			(void) m_free(m);
			return (0);
		}
		np->n_flags &= ~(NF_INTAVAIL|NF_OTHACK);
		np->n_flags |= NF_OTHSENT;
		if (len)
			(void) m_free(np->nb_xmt);
		nsp_insrtq(m, np->nt_oth);
		goto top;
	}

	/* interrupt request to be sent? */
	if (np->nf_locint == NFL_SEND && (np->n_flags & NF_OTHSENT) == 0) {
		register struct nspls *n;

		m = m_get(M_CANTWAIT, MT_HEADER);
		if (m == 0)
			return (0);
		m->m_len = sizeof (struct nspls);
		m->m_off = MMAXOFF - m->m_len;
		n = mtod(m, struct nspls *);
		n->nsp_msgflg = NSP_LS;
		n->nsp_dstaddr = np->n_rem;
		n->nsp_srcaddr = np->n_loc;
		n->nsp_acknum = NSPA_ACK | np->na_xmtoth;
		n->nsp_segnum = np->nn_oth;
		n->nsp_lsflags = NSPLS_INTREQ | NSPLS_ON;
		n->nsp_fcval = 1;
		if (tp_output(m, np->n_node)) {
			(void) m_free(m);
			return (0);
		}
		np->n_flags &= ~NF_OTHACK;
		np->n_flags |= NF_OTHSENT;
		nsp_insrtq(m, np->nt_oth);
		goto top;
	}

	/* data request to be sent? */
	if (np->nf_locdat > 0 && (np->n_flags & NF_OTHSENT == 0)) {
		register struct nspls *n;

		m = m_get(M_CANTWAIT, MT_HEADER);
		if (m == 0)
			return (0);
		m->m_len = sizeof (struct nspls);
		m->m_off = MMAXOFF - m->m_len;
		n = mtod(m, struct nspls *);
		n->nsp_msgflg = NSP_LS;
		n->nsp_dstaddr = np->n_rem;
		n->nsp_srcaddr = np->n_loc;
		n->nsp_acknum = NSPA_ACK | np->na_xmtoth;
		n->nsp_segnum = np->nn_oth;
		n->nsp_lsflags = NSPLS_DATREQ | NSPLS_ON;
		n->nsp_fcval = np->nf_locdat;
		if (tp_output(m, np->n_node)) {
			(void) m_free(m);
			return (0);
		}
		np->n_flags &= ~NF_OTHACK;
		np->n_flags |= NF_OTHSENT;
		nsp_insrtq(m, np->nt_oth);
		goto top;
	}

	/* other data ack to be sent? */
	if (np->n_flags & NF_OTHACK) {
		register struct nspack *n;

		m = m_get(M_CANTWAIT, MT_HEADER);
		if (m == 0)
			return (0);
		m->m_len = sizeof (struct nspack);
		m->m_off = MMAXOFF - m->m_len;
		n = mtod(m, struct nspack *);
		n->nsp_msgflg = NSP_OTHACK;
		n->nsp_dstaddr = np->n_rem;
		n->nsp_srcaddr = np->n_loc;
		n->nsp_acknum = NSPA_ACK | np->na_xmtoth;
		if (tp_output(m, np->n_node)) {
			(void) m_free(m);
			return (0);
		}
		np->n_flags &= ~NF_OTHACK;
		(void) m_free(m);
		goto top;
	}

	/* data to be sent? */
	if () {
		register struct nspd *n;

		m = nsp_mgetcl();
		if (m == 0)
			return (0);
		if (len <= np->n_segsize) {
			m->m_next = so->so_snd.sb_mb;
			so->so_snd.sb_mb = m->m_next->m_act;
		}

		/* MORE */

	}

	/* data ack to be sent? */
	if (np->n_flags & NF_DATACK) {
		register struct nspack *n;

		m = m_get(M_CANTWAIT, MT_HEADER);
		if (m == 0)
			return (0);
		m->m_len = sizeof (struct nspack);
		m->m_off = MMAXOFF - m->m_len;
		n = mtod(m, struct nspack *);
		n->nsp_msgflg = NSP_DATACK;
		n->nsp_dstaddr = np->n_rem;
		n->nsp_srcaddr = np->n_loc;
		n->nsp_acknum = NSPA_ACK | np->na_xmtdat;
		if (tp_output(m, np->n_node)) {
			(void) m_free(m);
			return (0);
		}
		np->n_flags &= ~NF_DATACK;
		(void) m_free(m);
		goto top;
	}

	/*
	 * Nothing left to do, return success.
	 */
	return (1);
}
