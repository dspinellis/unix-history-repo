/* $XConsortium: XDrLines.c,v 11.15 91/01/06 11:45:14 rws Exp $ */
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

XDrawLines (dpy, d, gc, points, npoints, mode)
    register Display *dpy;
    Drawable d;
    GC gc;
    XPoint *points;
    int npoints;
    int mode;
{
    register xPolyLineReq *req;
    register long length;
    LockDisplay(dpy);
    FlushGC(dpy, gc);
    GetReq (PolyLine, req);
    req->drawable = d;
    req->gc = gc->gid;
    req->coordMode = mode;
    if ((req->length + npoints) > (unsigned)65535)
	npoints = 65535 - req->length; /* force BadLength, if possible */
    req->length += npoints;
       /* each point is 2 16-bit integers */
    length = npoints << 2;		/* watch out for macros... */
    Data16 (dpy, (short *) points, length);
    UnlockDisplay(dpy);
    SyncHandle();
}

