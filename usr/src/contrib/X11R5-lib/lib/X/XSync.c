/* $XConsortium: XSync.c,v 11.15 91/01/06 11:48:24 rws Exp $ */
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

#define NEED_REPLIES
#define NEED_EVENTS
#include "Xlibint.h"

extern _XQEvent *_qfree;

/* Synchronize with errors and events, optionally discarding pending events */

XSync (dpy, discard)
    register Display *dpy;
    Bool discard;
{
    xGetInputFocusReply rep;
    register xReq *req;

    LockDisplay(dpy);
    GetEmptyReq(GetInputFocus, req);
    (void) _XReply (dpy, (xReply *)&rep, 0, xTrue);

    if (discard && dpy->head) {
       ((_XQEvent *)dpy->tail)->next = _qfree;
       _qfree = (_XQEvent *)dpy->head;
       dpy->head = dpy->tail = NULL;
       dpy->qlen = 0;
    }
    UnlockDisplay(dpy);
}

