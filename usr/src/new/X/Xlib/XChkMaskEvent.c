#include <X/mit-copyright.h>

/* $Header: XChkMaskEvent.c,v 10.1 86/03/28 11:28:22 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

extern _QEvent *_qfree;

/* 
 * Flush output and return the next event in the queue
 * matching one of the events in the mask if there is one.
 * Return whether such an event was found.
 * Events earlier in the queue are not discarded.
 */

int XCheckMaskEvent (mask, event)
	int mask;		/* Selected event mask. */
	register XEvent *event;	/* XEvent to be filled in. */
{
	register Display *dpy;
	register _QEvent *prev, *qelt;

	dpy = _XlibCurrentDisplay;
	_XFlush (dpy);
	for (prev = NULL, qelt = dpy->head;
	     qelt &&  !(qelt->event.type & mask);
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
	    return 1;
	} else return 0;
}
