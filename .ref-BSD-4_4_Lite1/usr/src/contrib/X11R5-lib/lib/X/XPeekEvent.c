/* $XConsortium: XPeekEvent.c,v 11.14 91/01/06 11:47:14 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1986	*/

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
 * Return the next event in the queue,
 * BUT do not remove it from the queue.
 * If none found, flush and wait until there is an event to peek.
 */

XPeekEvent (dpy, event)
	register Display *dpy;
	register XEvent *event;
{
	LockDisplay(dpy);
	if (dpy->head == NULL)
	    _XReadEvents(dpy);
	*event = (dpy->head)->event;
	UnlockDisplay(dpy);
}

