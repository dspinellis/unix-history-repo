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
 *	@(#)hd_timer.c	7.5 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/mbuf.h>
#include <sys/domain.h>
#include <sys/socket.h>
#include <sys/protosw.h>
#include <sys/errno.h>
#include <sys/time.h>
#include <sys/kernel.h>

#include <net/if.h>

#include <netccitt/hdlc.h>
#include <netccitt/hd_var.h>
#include <netccitt/x25.h>

/*
 * these can be patched with adb if the
 * default values are inappropriate
 */

int	hd_t1 = T1;
int	hd_t3 = T3;
int	hd_n2 = N2;

/*
 *  HDLC TIMER 
 *
 *  This routine is called every 500ms by the kernel. Decrement timer by this
 *  amount - if expired then process the event.
 */

hd_timer ()
{
	register struct hdcb *hdp;
	register int s = splimp ();

	for (hdp = hdcbhead; hdp; hdp = hdp->hd_next) {
		if (hdp->hd_rrtimer && (--hdp->hd_rrtimer == 0)) {
			if (hdp->hd_lasttxnr != hdp->hd_vr)
				hd_writeinternal (hdp, RR, POLLOFF);
		}

		if (!(hdp->hd_timer && --hdp->hd_timer == 0))
			continue;

		switch (hdp->hd_state) {
		case INIT: 
		case DISC_SENT:
			hd_writeinternal (hdp, DISC, POLLON);
			break;

		case ABM: 
			if (hdp->hd_lastrxnr != hdp->hd_vs) {	/* XXX */
				hdp->hd_timeouts++;
				hd_resend_iframe (hdp);
			}
			break;

		case WAIT_SABM: 
			hd_writeinternal (hdp, FRMR, POLLOFF);
			if (++hdp->hd_retxcnt == hd_n2) {
				hdp->hd_retxcnt = 0;
				hd_writeinternal (hdp, SABM, POLLOFF);
				hdp->hd_state = WAIT_UA;
			}
			break;

		case DM_SENT: 
			if (++hdp->hd_retxcnt == hd_n2) {
				/* Notify the packet level. */
				(void) pk_ctlinput (PRC_LINKDOWN, hdp->hd_pkp);
				hdp->hd_retxcnt = 0;
				hdp->hd_state = SABM_SENT;
				hd_writeinternal (hdp, SABM, POLLOFF);
			} else
				hd_writeinternal (hdp, DM, POLLOFF);
			break;

		case WAIT_UA: 
			if (++hdp->hd_retxcnt == hd_n2) {
				hdp->hd_retxcnt = 0;
				hd_writeinternal (hdp, DM, POLLOFF);
				hdp->hd_state = DM_SENT;
			} else
				hd_writeinternal (hdp, SABM, POLLOFF);
			break;

		case SABM_SENT: 
			/* Do this indefinitely. */
			hd_writeinternal (hdp, SABM, POLLON);
			break;

		case DISCONNECTED:
			/*
			 * Poll the interface driver flags waiting
			 * for the IFF_UP bit to come on.
			 */
			if (hdp->hd_ifp->if_flags & IFF_UP)
				hdp->hd_state = INIT;

		}
		SET_TIMER (hdp);
	}

	splx (s);
}
