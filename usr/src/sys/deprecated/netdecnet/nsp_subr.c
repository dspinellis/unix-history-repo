/*	nsp_subr.c	1.2	82/05/15	*/

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

extern int nspidebug;
#define	printd	if(nspidebug)printf

/*
 * NSP initialization
 */
nsp_init()
{
	init queues
	what else?
}

/*
 * Nsp_chkaddr performs many functions common to the processing
 * of input packets.  The arguments are:
 *	m	- the mbuf with the packet in it
 *	srcnode	- the srcnode from the transport header
 *	type	- the packet type, one of:
 *		  NSP_DATA, NSP_LS, NSP_INTR, NSP_DATACK, NSP_OTHACK
 *	sp	- pointer to a short to receive the segment number
 *
 * It performs the following functions:
 *	1. verify that the packet is of the correct minimum length
 *	2. find the associated NSP control block (by calling dn_addrtonspcb())
 *	3. process any ack or nak and force retransmission or remove
 *		acked data from the retransmit queue, as required
 *	4. update the mbuf to point past the segnum field
 *	5. return the segnum and nspcb pointer
 */
struct nspcb *
nsp_chkaddr(m, srcnode, type, sp)
	struct mbuf *m;
	short srcnode;
	int type;
	u_short *sp;
{
	register struct nspcb *np;
	struct nspd *n;
	u_short dstaddr;
	int ack, qual, num;

	/* make sure we are accessing valid data */
	if (m->m_len < sizeof (struct nspd) - sizeof (d_short)) {
		m_freem(m);
		return (0);
	}
	n = mtod(m, struct nspd *);
	dstaddr = D_SHORT(n->nsp_dstaddr);
	np = dn_addrtonspcb(dstaddr);
	if (np == 0) {
		no such address, return "no link" message
	}
	if (np->n_node != srcnode) {
		printf("nsp_chkaddr: n_node %d, srcnode %d\n", np->n_node,
			scrnode);
		m_freem(m);
		return (0);
	}
	/* make sure remote addresses match (consistency check) */
	if (np->n_rem != D_SHORT(n->nsp_srcaddr)) {
		printf("nsp_chkaddr: n_rem %d, srcaddr %d\n", np->n_rem,
			D_SHORT(n->nsp_srcaddr));
		m_freem(m);
		return (0);
	}
	ack = D_SHORT(n->nsp_acknum);
	if (ack & NSPA_ACK) {
		qual = ack & NSPA_QUAL;
		num = ack & NSPA_NUM;
		printd(", qual 0x%x, num %d", qual, num);
		/* make sure there's room for a segnum */
		if (m->m_len < sizeof (struct nspd)) {
			m_freem(m);
			return (0);
		}
		if (type == NSP_DATA) {
			if (SEQ_GTR(num, np->na_rcvdat) &&
			    SEQ_LEQ(num, np->nn_high)) {
				np->n_retrans = 0;
				np->nf_remdat -= SEQ_SUB(num, np->na_rcvdat);
			}
			if (qual == NSPA_NAK || SEQ_LEQ(np->nn_dat, num))
				np->nn_dat = SEQ_ADD(num, 1);
			np->na_rcvdat = num;
			nsp_purgertq(np, type);
		} else if (n == np->nn_oth && (np->n_flags&NF_OTHSENT)) {
			if (qual == NSPA_NAK) {
				/* force retransmission of other data seg */
				printf("nsp_chkaddr: NAK other\n");
			} else {
				np->n_flags &= ~NF_OTHSENT;
				np->nn_oth = SEQ_ADD(np->nn_oth, 1);
				if (np->n_flags & NF_OTHINTR) {
					np->n_flags &=
					    ~(NF_OTHINTR|NF_INTAVAIL);
					if (np->nb_xmt)
						m_freem(np->nb_xmt);
				} else
					np->nf_locdat = 0;
				nsp_purgertq(np, type);
			}
		}
		*sp = D_SHORT(n->nsp_segnum);
		num = sizeof (struct nspd);
	} else {
		*sp = (u_short)ack;
		num = sizeof (struct nspd) - sizeof (u_short);
	}
	m->m_len -= num;
	m->m_off += num;
	return (np);
}
