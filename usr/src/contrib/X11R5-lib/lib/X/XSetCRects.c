/* $XConsortium: XSetCRects.c,v 11.19 91/01/06 11:47:53 rws Exp $ */
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

XSetClipRectangles (dpy, gc, clip_x_origin, clip_y_origin, rectangles, n,
                    ordering)
    register Display *dpy;
    GC gc;
    int clip_x_origin, clip_y_origin;
    XRectangle *rectangles;
    int n;
    int ordering;
{
    LockDisplay(dpy);
    _XSetClipRectangles (dpy, gc, clip_x_origin, clip_y_origin, rectangles, n,
                    ordering);
    UnlockDisplay(dpy);
    SyncHandle();
}
    
/* can only call when display is locked. */
_XSetClipRectangles (dpy, gc, clip_x_origin, clip_y_origin, rectangles, n,
                    ordering)
    register Display *dpy;
    GC gc;
    int clip_x_origin, clip_y_origin;
    XRectangle *rectangles;
    int n;
    int ordering;
{
    register xSetClipRectanglesReq *req;
    register long nbytes;

    GetReq (SetClipRectangles, req);
    req->gc = gc->gid;
    req->xOrigin = gc->values.clip_x_origin = clip_x_origin;
    req->yOrigin = gc->values.clip_y_origin = clip_y_origin;
    req->ordering = ordering;
    req->length += n * (SIZEOF(xRectangle)/4);
    gc->rects = 1;
    nbytes = (long)n * SIZEOF(xRectangle);
    Data16 (dpy, (short *) rectangles, nbytes);
    gc->dirty &= ~(GCClipMask | GCClipXOrigin | GCClipYOrigin);
}
