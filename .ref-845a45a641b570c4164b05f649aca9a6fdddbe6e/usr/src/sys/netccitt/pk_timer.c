/* 
 * Copyright (c) Computing Centre, University of British Columbia, 1984
 * Copyright (C) Computer Science Department IV, 
 * 		 University of Erlangen-Nuremberg, Germany, 1990, 1992
 * Copyright (c) 1990, 1992   Regents of the University of California.
 * All rights reserved.
 * 
 * This code is derived from software contributed to Berkeley by the
 * Laboratory for Computation Vision and the Computer Science Department
 * of the the University of British Columbia and the Computer Science
 * Department (IV) of the University of Erlangen-Nuremberg, Germany.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pk_timer.c	7.7 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/mbuf.h>
#include <sys/socket.h>
#include <sys/protosw.h>
#include <sys/socketvar.h>
#include <sys/errno.h>

#include <net/if.h>

#include <netccitt/x25.h>
#include <netccitt/pk.h>
#include <netccitt/pk_var.h>

/*
 * Various timer values.  They can be adjusted
 * by patching the binary with adb if necessary.
 */
int	pk_t20 = 18 * PR_SLOWHZ;	/* restart timer */
int	pk_t21 = 20 * PR_SLOWHZ;	/* call timer */
/* XXX pk_t22 is never used */
int	pk_t22 = 18 * PR_SLOWHZ;	/* reset timer */
int	pk_t23 = 18 * PR_SLOWHZ;	/* clear timer */

pk_timer ()
{
	register struct pkcb *pkp;
	register struct pklcd *lcp, **pp;
	register int lcns_jammed, cant_restart;

	FOR_ALL_PKCBS(pkp) {
		switch (pkp -> pk_state) {
		case DTE_SENT_RESTART:
			lcp = pkp -> pk_chan[0];
			/*
			 * If restart failures are common, a link level
			 * reset should be initiated here.
			 */
			if (lcp -> lcd_timer && --lcp -> lcd_timer == 0) {
				pk_message (0, pkp -> pk_xcp,
					"packet level restart failed");
				pkp -> pk_state = DTE_WAITING;
			}
			break;

		case DTE_READY:
			lcns_jammed = cant_restart = 0;
			for (pp = &pkp -> pk_chan[1]; pp <= &pkp -> pk_chan[pkp -> pk_maxlcn]; pp++) {
				if ((lcp = *pp) == 0)
					continue;
				switch (lcp -> lcd_state) {
				case SENT_CALL: 
					if (--lcp -> lcd_timer == 0) {
					    if (lcp -> lcd_so)
						lcp -> lcd_so -> so_error = ETIMEDOUT;
					    pk_clear (lcp, 49, 1);
					}
					break;

				case SENT_CLEAR: 
					if (lcp -> lcd_retry >= 3)
						lcns_jammed++;
					else
						if (--lcp -> lcd_timer == 0)
							pk_clear (lcp, 50, 1);
					break;

				case DATA_TRANSFER:	/* lcn active */
					cant_restart++;
					break;

				case LCN_ZOMBIE:       /* zombie state */
					pk_freelcd (lcp);
					break;
				}
			}
			if (lcns_jammed > pkp -> pk_maxlcn / 2 && cant_restart == 0) {
				pk_message (0, pkp -> pk_xcp, "%d lcns jammed: attempting restart", lcns_jammed);
				pk_restart (pkp, 0);
			}
		}
	}
}
