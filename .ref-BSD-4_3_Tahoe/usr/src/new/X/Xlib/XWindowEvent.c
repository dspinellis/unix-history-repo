#include <X/mit-copyright.h>

/* $Header: XWindowEvent.c,v 10.5 86/04/22 15:21:13 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

extern _QEvent *_qfree;

/* 
 * Flush output and (wait for and) return the next event in the queue
 * for the given window matching one of the events in the mask.
 * Events earlier in the queue are not discarded.
 */

XWindowEvent (w, mask, event)
	Window w;		/* Selected window. */
	int mask;		/* Selected event mask. */
	register XEvent *event;	/* XEvent to be filled in. */
{
	register Display *dpy;
	register _QEvent *prev, *qelt;

	dpy = _XlibCurrentDisplay;
	_XFlush (dpy);
	for (prev = NULL, qelt = dpy->head;
	     qelt && (qelt->event.window != w ||
		      !(qelt->event.type & mask));
	     qelt = (prev = qelt)->next) ;
	if (qelt) {
	    *event = qelt->event;
	    if (prev) {
		if ((prev->next = qelt->next) == NULL)
		    dpy->tail = prev;
	    } else {
		if ((dpy->head = qelt->next) == NULL)
		    dpy->tail = NULL;
	    }
	    qelt->next = _qfree;
	    _qfree = qelt;
	    dpy->qlen--;
	    return;
	}
	while (1) {
	    _XRead (dpy, (char *)event, sizeof(XEvent));
	    if (event->type == X_Error)
		_XError(dpy, (XErrorEvent *)event);
	    else if ((event->window == w) && (event->type & mask))
		return;
	    else
		_XEnq(dpy, event);
	}
}

