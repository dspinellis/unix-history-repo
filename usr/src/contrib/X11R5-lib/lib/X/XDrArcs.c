/* $XConsortium: XDrArcs.c,v 11.17 91/01/06 11:45:12 rws Exp $ */
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

#define arc_scale (SIZEOF(xArc) / 4)

XDrawArcs(dpy, d, gc, arcs, n_arcs)
register Display *dpy;
Drawable d;
GC gc;
XArc *arcs;
int n_arcs;
{
    register xPolyArcReq *req;
    register long len;

    LockDisplay(dpy);
    FlushGC(dpy, gc);
    GetReq(PolyArc,req);
    req->drawable = d;
    req->gc = gc->gid;
    len = ((long)n_arcs) * arc_scale;
    if ((req->length + len) > (unsigned)65535)
	len = 1; /* force BadLength */
    req->length += len;
    len <<= 2; /* watch out for macros... */
    Data16 (dpy, (short *) arcs, len);
    UnlockDisplay(dpy);
    SyncHandle();
}
