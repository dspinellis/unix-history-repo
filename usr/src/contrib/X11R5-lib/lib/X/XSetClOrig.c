/* $XConsortium: XSetClOrig.c,v 11.13 91/01/06 11:47:55 rws Exp $ */
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

XSetClipOrigin (dpy, gc, xorig, yorig)
register Display *dpy;
GC gc;
int xorig, yorig;
{
    XGCValues *gv = &gc->values;

    LockDisplay(dpy);
    if (xorig != gv->clip_x_origin) {
        gv->clip_x_origin = xorig;
	gc->dirty |= GCClipXOrigin;
    }
    if (yorig != gv->clip_y_origin) {
        gv->clip_y_origin = yorig;
	gc->dirty |= GCClipYOrigin;
    }
    UnlockDisplay(dpy);
    SyncHandle();
}
