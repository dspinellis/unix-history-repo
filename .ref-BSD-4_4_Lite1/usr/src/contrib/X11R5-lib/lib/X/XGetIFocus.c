/* $XConsortium: XGetIFocus.c,v 11.10 91/01/06 11:46:05 rws Exp $ */
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
#include "Xlibint.h"

XGetInputFocus(dpy, focus, revert_to)
     register Display *dpy;
     Window *focus;
     int *revert_to;
{       
    xGetInputFocusReply rep;
    register xReq *req;
    LockDisplay(dpy);
    GetEmptyReq(GetInputFocus, req);
    (void) _XReply (dpy, (xReply *)&rep, 0, xTrue);
    *focus = rep.focus;
    *revert_to = rep.revertTo;
    UnlockDisplay(dpy);
    SyncHandle();
}

