/* $XConsortium: XStColor.c,v 11.10 91/01/06 11:48:18 rws Exp $ */
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

XStoreColor(dpy, cmap, def)
register Display *dpy;
Colormap cmap;
XColor *def;
{
    xColorItem *citem;
    register xStoreColorsReq *req;
#ifdef MUSTCOPY
    xColorItem citemdata;
    long len = SIZEOF(xColorItem);

    citem = &citemdata;
#endif /* MUSTCOPY */

    LockDisplay(dpy);
    GetReqExtra(StoreColors, SIZEOF(xColorItem), req); /* assume size is 4*n */

    req->cmap = cmap;

#ifndef MUSTCOPY
    citem = (xColorItem *) NEXTPTR(req,xStoreColorsReq);
#endif /* not MUSTCOPY */

    citem->pixel = def->pixel;
    citem->red = def->red;
    citem->green = def->green;
    citem->blue = def->blue;
    citem->flags = def->flags; /* do_red, do_green, do_blue */

#ifdef MUSTCOPY
    dpy->bufptr -= SIZEOF(xColorItem);		/* adjust for GetReqExtra */
    Data (dpy, (char *) citem, len);
#endif /* MUSTCOPY */

    UnlockDisplay(dpy);
    SyncHandle();
}
