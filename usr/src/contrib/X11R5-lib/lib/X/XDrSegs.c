/* $XConsortium: XDrSegs.c,v 11.13 91/01/06 11:45:21 rws Exp $ */
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

XDrawSegments (dpy, d, gc, segments, nsegments)
    register Display *dpy;
    Drawable d;
    GC gc;
    XSegment *segments;
    int nsegments;
{
    register xPolySegmentReq *req;
    long len;
    int n;

    LockDisplay(dpy);
    FlushGC(dpy, gc);
    while (nsegments) {
	GetReq (PolySegment, req);
	req->drawable = d;
	req->gc = gc->gid;
	n = nsegments;
	len = ((long)n) << 1;
	if (len > (dpy->max_request_size - req->length)) {
	    n = (dpy->max_request_size - req->length) >> 1;
	    len = ((long)n) << 1;
	}
	req->length += len;
	len <<= 2; /* watch out for macros... */
	Data16 (dpy, (short *) segments, len);
	nsegments -= n;
	segments += n;
    }
    UnlockDisplay(dpy);
    SyncHandle();
}

