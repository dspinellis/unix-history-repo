/* $XConsortium: XDrPoint.c,v 11.13 91/01/06 11:45:16 rws Exp $ */
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

#define size (SIZEOF(xPolyPointReq) + PTSPERBATCH * SIZEOF(xPoint))

XDrawPoint(dpy, d, gc, x, y)
    register Display *dpy;
    Drawable d;
    GC gc;
    int x, y; /* INT16 */
{
    xPoint *point;
#ifdef MUSTCOPY
    xPoint pointdata;
    long len = SIZEOF(xPoint);

    point = &pointdata;
#endif /* MUSTCOPY */

    LockDisplay(dpy);
    FlushGC(dpy, gc);

    {
    register xPolyPointReq *req = (xPolyPointReq *) dpy->last_req;


    /* if same as previous request, with same drawable, batch requests */
    if (
          (req->reqType == X_PolyPoint)
       && (req->drawable == d)
       && (req->gc == gc->gid)
       && (req->coordMode == CoordModeOrigin)
       && ((dpy->bufptr + SIZEOF(xPoint)) <= dpy->bufmax)
       && (((char *)dpy->bufptr - (char *)req) < size) ) {
	 req->length += SIZEOF(xPoint) >> 2;
#ifndef MUSTCOPY
         point = (xPoint *) dpy->bufptr;
	 dpy->bufptr += SIZEOF(xPoint);
#endif /* not MUSTCOPY */
	 }

    else {
	GetReqExtra(PolyPoint, 4, req); /* 1 point = 4 bytes */
	req->drawable = d;
	req->gc = gc->gid;
	req->coordMode = CoordModeOrigin;
#ifdef MUSTCOPY
	dpy->bufptr -= SIZEOF(xPoint);
#else
	point = (xPoint *) NEXTPTR(req,xPolyPointReq);
#endif /* MUSTCOPY */
	}

    point->x = x;
    point->y = y;

#ifdef MUSTCOPY
    Data (dpy, (char *) point, len);
#endif /* MUSTCOPY */
    }
    UnlockDisplay(dpy);
    SyncHandle();
}

