/* $XConsortium: XFillArc.c,v 11.14 91/01/06 11:45:28 rws Exp $ */
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

/* precompute the maximum size of batching request allowed */

#define size (SIZEOF(xPolyFillArcReq) + FARCSPERBATCH * SIZEOF(xArc))

XFillArc(dpy, d, gc, x, y, width, height, angle1, angle2)
    register Display *dpy;
    Drawable d;
    GC gc;
    int x, y; /* INT16 */
    unsigned int width, height; /* CARD16 */
    int angle1, angle2; /* INT16 */
{
    xArc *arc;
#ifdef MUSTCOPY
    xArc arcdata;
    long len = SIZEOF(xArc);

    arc = &arcdata;
#endif /* MUSTCOPY */

    LockDisplay(dpy);
    FlushGC(dpy, gc);

    {
    register xPolyFillArcReq *req = (xPolyFillArcReq *) dpy->last_req;

    /* if same as previous request, with same drawable, batch requests */
    if (
          (req->reqType == X_PolyFillArc)
       && (req->drawable == d)
       && (req->gc == gc->gid)
       && ((dpy->bufptr + SIZEOF(xArc)) <= dpy->bufmax)
       && (((char *)dpy->bufptr - (char *)req) < size) ) {
	 req->length += SIZEOF(xArc) >> 2;
#ifndef MUSTCOPY
         arc = (xArc *) dpy->bufptr;
	 dpy->bufptr += SIZEOF(xArc);
#endif /* not MUSTCOPY */
	 }

    else {
	GetReqExtra(PolyFillArc, SIZEOF(xArc), req);

	req->drawable = d;
	req->gc = gc->gid;
#ifdef MUSTCOPY
	dpy->bufptr -= SIZEOF(xArc);
#else
	arc = (xArc *) NEXTPTR(req,xPolyFillArcReq);
#endif /* MUSTCOPY */
	}
    arc->x = x;
    arc->y = y;
    arc->width = width;
    arc->height = height;
    arc->angle1 = angle1;
    arc->angle2 = angle2;

#ifdef MUSTCOPY
    Data (dpy, (char *) arc, len);
#endif /* MUSTCOPY */

    }
    UnlockDisplay(dpy);
    SyncHandle();
}
