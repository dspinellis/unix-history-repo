/* 
 * Copyright (C) Dirk Husemann, Computer Science Department IV, 
 * 		 University of Erlangen-Nuremberg, Germany, 1990, 1991, 1992
 * Copyright (c) 1992   Regents of the University of California.
 * All rights reserved.
 * 
 * This code is derived from software contributed to Berkeley by
 * Dirk Husemann and the Computer Science Department (IV) of
 * the University of Erlangen-Nuremberg, Germany.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)llc_timer.c	7.1 (Berkeley) %G%
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
#include <net/if_dl.h>
#include <net/if_llc.h>

#include <netccitt/dll.h>
#include <netccitt/llc_var.h>


/*
 * Various timer values.  They can be adjusted
 * by patching the binary with adb if necessary.
 */
/* ISO 8802-2 timers */
int 	llc_n2 			= LLC_N2_VALUE;
int 	llc_ACK_timer 		= LLC_ACK_TIMER;
int     llc_P_timer             = LLC_P_TIMER;
int     llc_BUSY_timer          = LLC_BUSY_TIMER;
int     llc_REJ_timer           = LLC_REJ_TIMER;
/* Implementation specific timers */
int 	llc_AGE_timer           = LLC_AGE_TIMER;
int     llc_DACTION_timer       = LLC_DACTION_TIMER;

/*
 * The timer routine. We are called every 500ms by the kernel.
 * Handle the various virtual timers.
 */

void
llc_timer()
{
	register struct llc_linkcb *linkp;
	register struct llc_linkcb *nlinkp;
	register int timer;
	register int action;
	register int s = splimp();

	/*
	 * All links are accessible over the doubly linked list llccb_q
	 */
	if (!LQEMPTY) {
		/*
		 * A for-loop is not that great an idea as the linkp
		 * might get deleted if the age timer has expired ...
		 */
		linkp = LQFIRST;
		while (LQVALID(linkp)) {
			nlinkp = LQNEXT(linkp);
			/*
			 * Check implementation specific timers first
			 */
			/* The delayed action/acknowledge idle timer */
			switch (LLC_TIMERXPIRED(linkp, DACTION)) {
			case LLC_TIMER_RUNNING:
				LLC_AGETIMER(linkp, DACTION);
				break;
			case LLC_TIMER_EXPIRED: {
				register int cmdrsp;
				register int pollfinal;

				switch (LLC_GETFLAG(linkp, DACTION)) {
				case LLC_DACKCMD:
					cmdrsp = LLC_CMD, pollfinal = 0;
					break;
				case LLC_DACKCMDPOLL:
					cmdrsp = LLC_CMD, pollfinal = 1;
					break;
				case LLC_DACKRSP:
					cmdrsp = LLC_RSP, pollfinal = 0;
					break;
				case LLC_DACKRSPFINAL:
					cmdrsp = LLC_RSP, pollfinal = 1;
					break;
				}
				llc_send(linkp, LLCFT_RR, cmdrsp, pollfinal);
				LLC_STOPTIMER(linkp, DACTION);
				break;
			}
			}
			/* The link idle timer */
			switch (LLC_TIMERXPIRED(linkp, AGE)) {
			case LLC_TIMER_RUNNING:
			        LLC_AGETIMER(linkp, AGE);
				break;
			case LLC_TIMER_EXPIRED:
				/*
				 * Only crunch the link when really no
				 * timers are running any more.
				 */
				if (llc_anytimersup(linkp) == 0) {
					llc_dellink(linkp);
					LLC_STOPTIMER(linkp, AGE);
					goto gone;
				} else {
					LLC_STARTTIMER(linkp, AGE);
				}
				break;
			}
			/* 
			 * Now, check all the ISO 8802-2 timers 
			 */
			FOR_ALL_LLC_TIMERS(timer) {
				action = 0;
				if ((linkp->llcl_timerflags & (1<<timer)) &&
				    (linkp->llcl_timers[timer] == 0)) {
					switch (timer) {
					case LLC_ACK_SHIFT:
						action = LLC_ACK_TIMER_EXPIRED;
						break;
					case LLC_P_SHIFT:
						action = LLC_P_TIMER_EXPIRED;
						break;
					case LLC_BUSY_SHIFT:
						action = LLC_BUSY_TIMER_EXPIRED;
						break;
					case LLC_REJ_SHIFT:
						action = LLC_REJ_TIMER_EXPIRED;
						break;
					}
					linkp->llcl_timerflags &= ~(1<<timer);
					(void)llc_statehandler(linkp, (struct llc *)0, action, 0, 1);
				} else if (linkp->llcl_timers[timer] > 0)
					linkp->llcl_timers[timer]--;
			}
			
gone:			linkp = nlinkp;
		}
	}
	splx (s);
}
