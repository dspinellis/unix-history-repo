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
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)event.c	7.3 (Berkeley) %G%
 *
 * from: $Header: event.c,v 1.4 92/06/17 05:35:45 torek Exp $ (LBL)
 */

/*
 * Internal `Firm_event' interface for the keyboard and mouse drivers.
 */

#include <sys/param.h>
#include <sys/fcntl.h>
#include <sys/malloc.h>
#include <sys/proc.h>
#include <sys/systm.h>
#include <sys/vnode.h>

#include <sparc/dev/vuid_event.h>
#include <sparc/dev/event_var.h>

/*
 * Initialize a firm_event queue.
 */
void
ev_init(ev)
	register struct evvar *ev;
{

	ev->ev_get = ev->ev_put = 0;
	ev->ev_q = malloc((u_long)EV_QSIZE * sizeof(struct firm_event),
	    M_DEVBUF, M_WAITOK);
	bzero((caddr_t)ev->ev_q, EV_QSIZE * sizeof(struct firm_event));
}

/*
 * Tear down a firm_event queue.
 */
void
ev_fini(ev)
	register struct evvar *ev;
{

	free(ev->ev_q, M_DEVBUF);
}

/*
 * User-level interface: read, select.
 * (User cannot write an event queue.)
 */
int
ev_read(ev, uio, flags)
	register struct evvar *ev;
	struct uio *uio;
	int flags;
{
	int s, n, cnt, error;

	/*
	 * Make sure we can return at least 1.
	 */
	if (uio->uio_resid < sizeof(struct firm_event))
		return (EMSGSIZE);	/* ??? */
	s = splev();
	while (ev->ev_get == ev->ev_put) {
		if (flags & IO_NDELAY) {
			splx(s);
			return (EWOULDBLOCK);
		}
		ev->ev_wanted = 1;
		error = tsleep((caddr_t)ev, PEVENT | PCATCH, "firm_event", 0);
		if (error) {
			splx(s);
			return (error);
		}
	}
	/*
	 * Move firm_events from tail end of queue (there is at least one
	 * there).
	 */
	if (ev->ev_put < ev->ev_get)
		cnt = EV_QSIZE - ev->ev_get;	/* events in [get..QSIZE) */
	else
		cnt = ev->ev_put - ev->ev_get;	/* events in [get..put) */
	splx(s);
	n = howmany(uio->uio_resid, sizeof(struct firm_event));
	if (cnt > n)
		cnt = n;
	error = uiomove((caddr_t)&ev->ev_q[ev->ev_get],
	    cnt * sizeof(struct firm_event), uio);
	n -= cnt;
	/*
	 * If we do not wrap to 0, used up all our space, or had an error,
	 * stop.  Otherwise move from front of queue to put index, if there
	 * is anything there to move.
	 */
	if ((ev->ev_get = (ev->ev_get + cnt) % EV_QSIZE) != 0 ||
	    n == 0 || error || (cnt = ev->ev_put) == 0)
		return (error);
	if (cnt > n)
		cnt = n;
	error = uiomove((caddr_t)&ev->ev_q[0],
	    cnt * sizeof(struct firm_event), uio);
	ev->ev_get = cnt;
	return (error);
}

int
ev_select(ev, rw, p)
	register struct evvar *ev;
	int rw;
	struct proc *p;
{
	int s = splev();

	switch (rw) {

	case FREAD:
		/* succeed if there is something to read */
		if (ev->ev_get != ev->ev_put) {
			splx(s);
			return (1);
		}
		selrecord(p, &ev->ev_sel);
		break;

	case FWRITE:
		return (1);	/* always fails => never blocks */
	}
	splx(s);
	return (0);
}
