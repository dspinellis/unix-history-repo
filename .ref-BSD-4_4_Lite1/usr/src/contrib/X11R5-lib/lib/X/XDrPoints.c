/* $XConsortium: XDrPoints.c,v 1.15 91/01/06 11:45:17 rws Exp $ */
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

XDrawPoints(dpy, d, gc, points, n_points, mode)
    register Display *dpy;
    Drawable d;
    GC gc;
    XPoint *points;
    int n_points;
    int mode; /* CoordMode */
{
    register xPolyPointReq *req;
    register long nbytes;
    int n;
    int xoff, yoff;
    XPoint pt;

    xoff = yoff = 0;
    LockDisplay(dpy);
    FlushGC(dpy, gc);
    while (n_points) {
	GetReq(PolyPoint, req);
	req->drawable = d;
	req->gc = gc->gid;
	req->coordMode = mode;
	n = n_points;
	if (n > (dpy->max_request_size - req->length))
	    n = dpy->max_request_size - req->length;
	req->length += n;
	nbytes = ((long)n) << 2; /* watch out for macros... */
	if (xoff || yoff) {
	    pt.x = xoff + points->x;
	    pt.y = yoff + points->y;
	    Data16 (dpy, (short *) &pt, 4);
	    if (nbytes > 4) {
		Data16 (dpy, (short *) (points + 1), nbytes - 4);
	    }
	} else {
	    Data16 (dpy, (short *) points, nbytes);
	}
	n_points -= n;
	if (n_points && (mode == CoordModePrevious)) {
	    register XPoint *pptr = points;
	    points += n;
	    while (pptr != points) {
		xoff += pptr->x;
		yoff += pptr->y;
		pptr++;
	    }
	} else
	    points += n;
    }
    UnlockDisplay(dpy);
    SyncHandle();
}
