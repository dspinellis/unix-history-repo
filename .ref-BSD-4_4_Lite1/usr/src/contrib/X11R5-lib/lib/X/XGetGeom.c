/* $XConsortium: XGetGeom.c,v 11.16 91/01/06 11:45:57 rws Exp $ */
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

#define NEED_REPLIES
#include "Xlibint.h"

Status XGetGeometry (dpy, d, root, x, y, width, height, borderWidth, depth)
    register Display *dpy;
    Drawable d;
    Window *root; /* RETURN */
    int *x, *y;  /* RETURN */
    unsigned int *width, *height, *borderWidth, *depth;  /* RETURN */
{
    xGetGeometryReply rep;
    register xResourceReq *req;
    LockDisplay(dpy);
    GetResReq(GetGeometry, d, req);
    if (!_XReply (dpy, (xReply *)&rep, 0, xTrue)) {
	UnlockDisplay(dpy);
	SyncHandle();
	return (0);
	}
    *root = rep.root;
    *x = cvtINT16toInt (rep.x);
    *y = cvtINT16toInt (rep.y);
    *width = rep.width;
    *height = rep.height;
    *borderWidth = rep.borderWidth;
    *depth = rep.depth;
    UnlockDisplay(dpy);
    SyncHandle();
    return (1);
}

