#include <X/mit-copyright.h>

/* $Header: XNextEvent.c,v 10.4 86/02/01 15:37:26 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

extern _QEvent *_qfree;

/* Flush output and (wait for and) return the next event in the queue.
*/

XNextEvent (event)
	register XEvent *event;
{
	register _QEvent *qelt;
	register Display *dpy = _XlibCurrentDisplay;
	
	_XFlush (dpy);
	
	/* if the queue is empty, read as many events as possible
	   and enqueue them */
	while (dpy->head == NULL) {
	    char buf[BUFSIZE];
	    int pend_not_register; /* because can't "&" a register variable */
	    register int pend;
	    register XRep *rep;

	    /* find out how much data can be read */
	    if (ioctl(dpy->fd, FIONREAD, &pend_not_register) < 0)
	    	_XIOError(dpy);
	    pend = pend_not_register;

	    /* must read at least one XRep; if none is pending, then
	       we'll just block waiting for it */
	    if (pend < sizeof(XRep))
	    	pend = sizeof (XRep);
		
	    /* but we won't read more than the max buffer size */
	    if (pend > BUFSIZE)
	    	pend = BUFSIZE;

	    /* round down to an integral number of XReps */
	    pend = (pend / sizeof (XRep)) * sizeof (XRep);

	    _XRead (dpy, buf, pend);
	    for (rep = (XRep *) buf; pend > 0; rep++, pend -= sizeof(XRep))
		if (rep->code == X_Error)
		    _XError (dpy, (XErrorEvent *) rep);
		else  /* it's an event packet; enqueue it */
		    _XEnq (dpy, (XEvent *) rep);
	    }

	*event = (qelt = dpy->head)->event;

	/* move the head of the queue to the free list */
	if ((dpy->head = qelt->next) == NULL)
	    dpy->tail = NULL;
	qelt->next = _qfree;
	_qfree = qelt;
	dpy->qlen--;
}

