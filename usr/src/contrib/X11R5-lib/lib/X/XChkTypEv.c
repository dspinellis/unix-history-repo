/* $XConsortium: XChkTypEv.c,v 11.8 91/01/06 11:44:31 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985, 1987	*/

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#define NEED_EVENTS
#include "Xlibint.h"

extern _XQEvent *_qfree;
/* 
 * Check existing events in queue to find if any match.  If so, return.
 * If not, flush buffer and see if any more events are readable. If one
 * matches, return.  If all else fails, tell the user no events found.
 */

Bool XCheckTypedEvent (dpy, type, event)
        register Display *dpy;
	int type;		/* Selected event type. */
	register XEvent *event;	/* XEvent to be filled in. */
{
	register _XQEvent *prev, *qelt;
	int n;		/* time through count */

        LockDisplay(dpy);
	prev = NULL;
	for (n = 3; --n >= 0;) {
	    for (qelt = prev ? prev->next : dpy->head;
		 qelt;
		 prev = qelt, qelt = qelt->next) {
		if (qelt->event.type == type) {
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
		    UnlockDisplay(dpy);
		    return True;
		}
	    }
	    switch (n) {
	      case 2:
		_XEventsQueued(dpy, QueuedAfterReading);
		break;
	      case 1:
		_XFlush(dpy);
		break;
	    }
	}
	UnlockDisplay(dpy);
	return False;
}
