/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)event_var.h	7.3 (Berkeley) %G%
 *
 * from: $Header: event_var.h,v 1.5 92/11/26 01:11:51 torek Exp $ (LBL)
 */

/*
 * Internal `Firm_event' interface for the keyboard and mouse drivers.
 * The drivers are expected not to place events in the queue above spltty(),
 * i.e., are expected to run off serial ports.
 */

/* EV_QSIZE should be a power of two so that `%' is fast */
#define	EV_QSIZE	256	/* may need tuning; this uses 2k */

struct evvar {
	u_int	ev_get;		/* get (read) index (modified synchronously) */
	volatile u_int ev_put;	/* put (write) index (modified by interrupt) */
	struct	selinfo ev_sel;	/* process selecting */
	struct	proc *ev_io;	/* process that opened queue (can get SIGIO) */
	char	ev_wanted;	/* wake up on input ready */
	char	ev_async;	/* send SIGIO on input ready */
	struct	firm_event *ev_q;/* circular buffer (queue) of events */
};

#define	splev()	spltty()

#define	EV_WAKEUP(ev) { \
	selwakeup(&(ev)->ev_sel); \
	if ((ev)->ev_wanted) { \
		(ev)->ev_wanted = 0; \
		wakeup((caddr_t)(ev)); \
	} \
	if ((ev)->ev_async) \
		psignal((ev)->ev_io, SIGIO); \
}

void	ev_init __P((struct evvar *));
void	ev_fini __P((struct evvar *));
int	ev_read __P((struct evvar *, struct uio *, int));
int	ev_select __P((struct evvar *, int, struct proc *));

/*
 * PEVENT is set just above PSOCK, which is just above TTIPRI, on the
 * theory that mouse and keyboard `user' input should be quick.
 */
#define	PEVENT	23
