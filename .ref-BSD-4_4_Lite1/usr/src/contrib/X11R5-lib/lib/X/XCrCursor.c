/* $XConsortium: XCrCursor.c,v 11.8 91/01/06 11:44:53 rws Exp $ */
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

Cursor XCreatePixmapCursor(dpy, source, mask, foreground, background, x, y)
     register Display *dpy;
     Pixmap source, mask;
     XColor *foreground, *background;
     unsigned int  x, y;

{       
    register xCreateCursorReq *req;
    Cursor cid;

    LockDisplay(dpy);
    GetReq(CreateCursor, req);
    req->cid = cid = XAllocID(dpy);
    req->source = source;
    req->mask = mask;
    req->foreRed = foreground->red;
    req->foreGreen = foreground->green;
    req->foreBlue = foreground->blue;
    req->backRed = background->red;
    req->backGreen = background->green;
    req->backBlue = background->blue;
    req->x = x;
    req->y = y;
    UnlockDisplay(dpy);
    SyncHandle();
    return (cid);
}

