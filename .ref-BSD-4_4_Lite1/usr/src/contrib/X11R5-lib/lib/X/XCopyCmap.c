/* $XConsortium: XCopyCmap.c,v 11.9 91/02/12 16:10:45 dave Exp $ */
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

Colormap XCopyColormapAndFree(dpy, src_cmap)
register Display *dpy;
Colormap src_cmap;
{
    Colormap mid;
    register xCopyColormapAndFreeReq *req;

    LockDisplay(dpy);
    GetReq(CopyColormapAndFree, req);

    mid = req->mid = XAllocID(dpy);
    req->srcCmap = src_cmap;

    UnlockDisplay(dpy);
    SyncHandle();

    _XcmsCopyCmapRecAndFree(dpy, src_cmap, mid);

    return(mid);
}
