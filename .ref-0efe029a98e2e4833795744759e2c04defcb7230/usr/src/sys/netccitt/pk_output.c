/*
 * Copyright (c) University of British Columbia, 1984
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Laboratory for Computation Vision and the Computer Science Department
 * of the University of British Columbia.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pk_output.c	7.6 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "socket.h"
#include "socketvar.h"
#include "protosw.h"
#include "errno.h"

#include "../net/if.h"

#include "x25.h"
#include "pk.h"
#include "pk_var.h"

struct	mbuf *nextpk ();

pk_output (lcp)
register struct pklcd *lcp;
{
	register struct x25_packet *xp;
	register struct mbuf *m;
	register struct pkcb *pkp = lcp -> lcd_pkp;

	if (lcp == 0 || pkp == 0) {
		printf ("pk_output: zero arg\n");
		return;
	}

	while ((m = nextpk (lcp)) != NULL) {
		xp = mtod (m, struct x25_packet *);

		switch (pk_decode (xp) + lcp -> lcd_state) {
		/* 
		 *  All the work is already done - just set the state and
		 *  pass to peer.
		 */
		case CALL + READY: 
			lcp -> lcd_state = SENT_CALL;
			lcp -> lcd_timer = pk_t21;
			break;

		/*
		 *  Just set the state to allow packet to flow and send the
		 *  confirmation.
		 */
		case CALL_ACCEPTED + RECEIVED_CALL: 
			lcp -> lcd_state = DATA_TRANSFER;
			break;

		/* 
		 *  Just set the state. Keep the LCD around till the clear
		 *  confirmation is returned.
		 */
		case CLEAR + RECEIVED_CALL: 
		case CLEAR + SENT_CALL: 
		case CLEAR + DATA_TRANSFER: 
			lcp -> lcd_state = SENT_CLEAR;
			lcp -> lcd_retry = 0;
			/* fall through */

		case CLEAR + SENT_CLEAR:
			lcp -> lcd_timer = pk_t23;
			lcp -> lcd_retry++;
			break;

		case CLEAR_CONF + RECEIVED_CLEAR: 
		case CLEAR_CONF + SENT_CLEAR: 
		case CLEAR_CONF + READY: 
			lcp -> lcd_state = READY;
			break;

		case DATA + DATA_TRANSFER: 
			PS(xp) = lcp -> lcd_ssn;
			PR(xp) = lcp -> lcd_input_window;
			lcp -> lcd_last_transmitted_pr = lcp -> lcd_input_window;
			lcp -> lcd_ssn = (lcp -> lcd_ssn + 1) % MODULUS;
			if (lcp -> lcd_ssn == ((lcp -> lcd_output_window + pkp->pk_xcp->xc_pwsize) % MODULUS))
				lcp -> lcd_window_condition = TRUE;
			break;

		case INTERRUPT + DATA_TRANSFER: 
			xp -> packet_data = 0;
			lcp -> lcd_intrconf_pending = TRUE;
			break;

		case INTERRUPT_CONF + DATA_TRANSFER: 
			break;

		case RR + DATA_TRANSFER: 
			lcp -> lcd_input_window = (lcp -> lcd_input_window + 1) % MODULUS;
			PR(xp) = lcp -> lcd_input_window;
			lcp -> lcd_last_transmitted_pr = lcp -> lcd_input_window;
			break;

		case RNR + DATA_TRANSFER: 
			PR(xp) = lcp -> lcd_input_window;
			lcp -> lcd_last_transmitted_pr = lcp -> lcd_input_window;
			break;

		case RESET + DATA_TRANSFER: 
			lcp -> lcd_reset_condition = TRUE;
			break;

		case RESET_CONF + DATA_TRANSFER: 
			lcp -> lcd_reset_condition = FALSE;
			break;

		/* 
		 *  A restart should be only generated internally. Therefore
		 *  all logic for restart is in the pk_restart routine.
		 */
		case RESTART + READY: 
			lcp -> lcd_timer = pk_t20;
			break;

		/* 
		 *  Restarts are all  handled internally.  Therefore all the
		 *  logic for the incoming restart packet is handled in  the
		 *  pk_input routine.
		 */
		case RESTART_CONF + READY: 
			break;

		default: 
			m_freem (m);
			return;
		}

		/* Trace the packet. */
		pk_trace (pkp -> pk_xcp, xp, "P-Out");

		/* Pass the packet on down to the link layer */
		(*pkp -> pk_lloutput) (pkp -> pk_llnext, m);
	}
}

/* 
 *  This procedure returns the next packet to send or null. A
 *  packet is composed of one or more mbufs.
 */

struct mbuf *
nextpk (lcp)
struct pklcd *lcp;
{
	register struct mbuf *m, *n;
	struct socket *so = lcp -> lcd_so;
	register struct sockbuf *sb = & (so ? so -> so_snd : lcp -> lcd_sb);

	if (lcp -> lcd_template) {
		m = dtom (lcp -> lcd_template);
		lcp -> lcd_template = NULL;
	} else {
		if (lcp -> lcd_rnr_condition || lcp -> lcd_window_condition ||
				lcp -> lcd_reset_condition)
			return (NULL);

		if ((m = sb -> sb_mb) == 0)
			return (NULL);

 		sb -> sb_mb = m -> m_nextpkt;
 		m->m_act = 0;
		for (n = m; n; n = n -> m_next)
			sbfree (sb, n);
	}
	return (m);
}
