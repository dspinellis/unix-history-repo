/* $XConsortium: XAllPlanes.c,v 11.17 91/01/06 11:44:04 rws Exp $ */
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

Status XAllocColorPlanes(dpy, cmap, contig, pixels, ncolors, nreds, ngreens, 
                         nblues, rmask, gmask, bmask)
register Display *dpy;
Colormap cmap;
Bool contig;
unsigned long *pixels; /* LISTofCARD32 */ /* RETURN */
int ncolors;
int nreds, ngreens, nblues;
unsigned long *rmask, *gmask, *bmask; /* CARD32 */ /* RETURN */
{
    xAllocColorPlanesReply rep;
    Status status;
    register xAllocColorPlanesReq *req;

    LockDisplay(dpy);
    GetReq(AllocColorPlanes,req);

    req->cmap = cmap;
    req->colors = ncolors;
    req->red = nreds;
    req->green = ngreens;
    req->blue = nblues;
    req->contiguous = contig;

    status = _XReply(dpy, (xReply *)&rep, 0, xFalse);


    if (status) {
	*rmask = rep.redMask;
	*gmask = rep.greenMask;
	*bmask = rep.blueMask;

	/* sizeof(CARD32) = 4 */
	_XRead32 (dpy, (char *) pixels, (long)(ncolors * 4));
    }

    UnlockDisplay(dpy);
    SyncHandle();
    return(status);
}    
