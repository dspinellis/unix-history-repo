/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)event_var.h	8.1 (Berkeley) 6/11/93
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
