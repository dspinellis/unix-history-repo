/* $XConsortium: XFillRects.c,v 11.14 91/01/06 11:45:37 rws Exp $ */
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

XFillRectangles(dpy, d, gc, rectangles, n_rects)
register Display *dpy;
Drawable d;
GC gc;
XRectangle *rectangles;
int n_rects;
{
    register xPolyFillRectangleReq *req;
    long len;
    int n;

    LockDisplay(dpy);
    FlushGC(dpy, gc);
    while (n_rects) {
	GetReq(PolyFillRectangle, req);
	req->drawable = d;
	req->gc = gc->gid;
	n = n_rects;
	len = ((long)n) << 1;
	if (len > (dpy->max_request_size - req->length)) {
	    n = (dpy->max_request_size - req->length) >> 1;
	    len = ((long)n) << 1;
	}
	req->length += len;
	len <<= 2; /* watch out for macros... */
	Data16 (dpy, (short *) rectangles, len);
	n_rects -= n;
	rectangles += n;
    }
    UnlockDisplay(dpy);
    SyncHandle();
}
    
