/* $XConsortium: XGrButton.c,v 11.9 91/01/06 11:46:22 rws Exp $ */
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

#include "Xlibint.h"

XGrabButton(dpy, button, modifiers, grab_window, owner_events, event_mask,
	    pointer_mode, keyboard_mode, confine_to, curs)
register Display *dpy;
unsigned int modifiers; /* CARD16 */
unsigned int button; /* CARD8 */
Window grab_window;
Bool owner_events;
unsigned int event_mask; /* CARD16 */
int pointer_mode, keyboard_mode;
Window confine_to;
Cursor curs;
{
    register xGrabButtonReq *req;
    LockDisplay(dpy);
    GetReq(GrabButton, req);
    req->modifiers = modifiers;
    req->button = button;
    req->grabWindow = grab_window;
    req->ownerEvents = owner_events;
    req->eventMask = event_mask;
    req->pointerMode = pointer_mode;
    req->keyboardMode = keyboard_mode;
    req->confineTo = confine_to;
    req->cursor = curs;
    UnlockDisplay(dpy);
    SyncHandle();
}
