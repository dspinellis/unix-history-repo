/* $XConsortium: XWinEvent.c,v 11.19 91/02/20 18:48:57 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

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

#if __STDC__
#define Const const
#else
#define Const /**/
#endif

extern _XQEvent *_qfree;
extern long Const _Xevent_to_mask[];
#define AllPointers (PointerMotionMask|PointerMotionHintMask|ButtonMotionMask)
#define AllButtons (Button1MotionMask|Button2MotionMask|Button3MotionMask|\
		    Button4MotionMask|Button5MotionMask)

/* 
 * Return the next event in the queue
 * for the given window matching one of the events in the mask.
 * Events earlier in the queue are not discarded.
 * If none found, flush, and then wait until an event arrives which
 * matches.
 */

XWindowEvent (dpy, w, mask, event)
        register Display *dpy;
	Window w;		/* Selected window. */
	long mask;		/* Selected event mask. */
	register XEvent *event;	/* XEvent to be filled in. */
{
	register _XQEvent *prev, *qelt;

        LockDisplay(dpy);
	prev = NULL;
	while (1) {
	    for (qelt = prev ? prev->next : dpy->head;
		 qelt;
		 prev = qelt, qelt = qelt->next) {
		if ((qelt->event.xany.window == w) &&
		    (qelt->event.type < LASTEvent) &&
		    (_Xevent_to_mask[qelt->event.type] & mask) &&
		    ((qelt->event.type != MotionNotify) ||
		     (mask & AllPointers) ||
		     (mask & AllButtons & qelt->event.xmotion.state))) {
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
		    return;
		}
	    }
	    _XReadEvents(dpy);
	}
}
