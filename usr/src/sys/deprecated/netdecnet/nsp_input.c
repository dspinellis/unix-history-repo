
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../net/dn_systm.h"
#include "../net/nsp.h"
#include "../net/nsp_var.h"
#include "../errno.h"

int nspidebug = 1;
#define	printd	if(nspidebug)printf
/*
 * NSP input routine: decode incoming packet and dispatch
 * to appropriate socket.  Called from the software interrupt
 * at splnet.
 *
 * TODO:
 *	count occurances of various error conditions.
 */

nspintr()
{
	struct mbuf *m;
	struct tprh *t;
	int s, bom, eom;
	u_short srcnode;
	char *p;
	struct nspcb *np;

	/*
	 * Loop pulling packets off the interrupt queue.
	 */
next:
	s = splimp();
	IF_DEQUEUE(&nspintrq, m);
	splx(s);
	printd("nsp_input: m 0x%x", m);
	if (m == 0)
		return;
	t = mtod(m, struct tprh *);
	srcnode = t->tprh_srcnode;
	m->m_len -= sizeof (struct tprh);	/* use m_adj??? */
	m->m_off += sizeof (struct tprh);
	printd(", srcnode %d, len %d", srcnode, m->m_len);
	if (m->m_len <= 0) {
		m_freem(m);
		goto next;
	}

	/*
	 * Switch on the type of the message.
	 */
	p = mtod(m, char *);
	switch (*p) {

	/*
	 * Got a Data message, possibly with EOM and
	 * BOM flags set.  Call nsp_chkaddr to do ack
	 * and flow controll processing, then pass the
	 * data to the user.
	 */
	case NSP_DATA|NSP_EOM|NSP_BOM:
		eom = bom = 1;
		goto data;

	case NSP_DATA|NSP_EOM:
		eom = 1;
		goto data;

	case NSP_DATA|NSP_BOM:
		bom = 1;

	case NSP_DATA:
	data:
		printd(", DATA (%d,%d)", bom, eom);
		np = nsp_chkaddr(m, srcnode, NSP_DATA, &segnum);
		if (np == 0) {
			m_freem(m);
			goto next;
		}

		/*
		 * Data messages only valid in Run state
		 */
		if (np->n_state != NS_RUN) {
			printf(", !RUN (%d)\n", np->n_state);
			m_freem(m);
			goto next;
		}
		if (SEQ_GTR(segnum, np->na_xmtdat)) {
			/* SHOULD DO SEGMENT RECONSTRUCTION HERE */
			printd(", got data!");
			sbpappend(m, &np->n_socket->sb_rcv);
		} else
			np->n_flags |= NF_DATACK;
		break;

	/*
	 * Got an interrupt message.  Call nsp_chkaddr
	 * (as usual).  Save the interrupt data for the
	 * user.
	 * GENERATE A SIGNAL OF SOME SORT???
	 */
	case NSP_INTR:
		printd(", INTR");
		np = nsp_chkaddr(m, srcnode, NSP_INTR, &segnum);
		if (np == 0) {
			m_freem(m);
			goto next;
		}

		/*
		 * If we are in the Connect Confirm state then
		 * this Interrupt packet causes the transition
		 * to the Run state.  Otherwise we better be in
		 * the Run state already.
		 */
		if (np->n_state == NS_CC)
			np->n_state = NS_RUN;
		else if (np->n_state != NS_RUN) {
			printf(", !RUN %d\n", np->n_state);
			m_freem(m);
			goto next;
		}

		/*
		 * If this segment is the one after the last
		 * other data segment we acked, and there is
		 * no waiting interrupt message, then queue
		 * this one up.
		 */
		if (segnum == SEQ_ADD(np->na_xmtoth, 1) &&
		    np->nf_locint == NFL_EMPTY) {
			if (np->nb_rcv) {
				printd(", flush old intr data");
				m_freem(np->nb_rcv);
			}
			if (m->m_len > 16) {
				printd(", intr data too long\n");
				m_freem(m);
				goto next;
			}
			np->nb_rcv = m;
			np->nf_locint = NFL_INTR;
			np->na_xmtoth = segnum;		/* really += 1 */
			np->n_flags |= NF_OTHACK;
		} else if (SEQ_LEQ(segnum, np->na_xmtoth))
			np->n_flags |= NF_OTHACK;
		break;

	/*
	 * Got a Link Service message.  Process options
	 * to modify flow control values.
	 */
	case NSP_LS:
		printd(", LS");
		np = nsp_chkaddr(m, srcnode, NSP_LS, &segnum);
		if (np == 0) {
			m_freem(m);
			goto next;
		}
		
		/*
		 * If we are in the Connect Confirm state then
		 * this Link Service packet causes the transition
		 * to the Run state.  Otherwise we better be in
		 * the Run state already.
		 */
		if (np->n_state == NS_CC)
			np->n_state = NS_RUN;
		else if (np->n_state != NS_RUN) {
			printd(", !RUN %d\n", np->n_state);
			m_freem(m);
			goto next;
		}
		p = mtod(m, char *);
		lsf = *p++;
		fcval = *p;
		printd(", lsf 0x%x, fcval %d", lsf, fcval);
		switch (lsf & NSPLS_FCVALINT) {
		case NSPLS_DATREQ:
			if (seqnum == SEQ_ADD(np->na_xmtoth, 1)) {
				if (np->nf_remdat + fcval >= -128 &&
				    np->nf_remdat + fcval <= 127) {
					np->nf_remdat += fcval;
					np->na_xmtoth = segnum;
					np->n_flags |= NF_OTHACK;
					switch (lsf & NSPLS_FCMOD) {
					case NSPLS_NOCHANGE:
						break;
					case NSPLS_ON:
						np->n_flags &= ~NF_DATOFF;
						break;
					case NSPLS_OFF:
						np->n_flags |= NF_DATOFF;
						break;
					default:
						printd(", bad fcmod");
					}
				}
			} else if (SEQ_LEQ(segnum, np->na_xmtoth))
				np->n_flags |= NF_OTHACK;
			break;

		case NSPLS_INTREQ:
			if (seqnum == SEQ_ADD(np->na_xmtoth, 1)) {
				if (fcval >= 0 && np->nf_remint+fcval <= 127) {
					np->nf_remint += fcval;
					np->na_xmtoth = segnum;
					np->n_flags |= NF_OTHACK;
			} else if (SEQ_LEQ(segnum, np->na_xmtoth))
				np->n_flags |= NF_OTHACK;
			break;

		default:
			printd(", bad fcvalint");
		}
		break;

	/*
	 * Got an acknowledgement for a Data message.
	 * Nsp_chkaddr processes the ack, nothing else
	 * to do.
	 */
	case NSP_DATACK:
		printd(", DATACK");
		np = nsp_chkaddr(m, srcnode, NSP_DATACK, &segnum);
		if (np == 0) {
			m_freem(m);
			goto next;
		}
		break;

	/*
	 * Got an acknowledgement for an Other Data message.
	 * Nsp_chkaddr processes the ack, nothing else to do.
	 */
	case NSP_OTHACK:
		printd(", OTHACK");
		np = nsp_chkaddr(m, srcnode, NSP_OTHACK, &segnum);
		if (np == 0) {
			m_freem(m);
			goto next;
		}
		break;

	/*
	 * Got a Connect Acknowledgement.  Just verify
	 * the address and perform the state transition.
	 */
	case NSP_CONACK:
		DOIT
		break;

	/*
	 * Got an unknown message, count it and flush it.
	 */
	default:
		printd(", UNKNOWN!!!");
		m_freem(m);
		break;
	}
	printd("\n");
	goto next;
}
