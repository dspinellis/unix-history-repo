/* $XConsortium: XLoadFont.c,v 11.11 91/01/06 11:46:51 rws Exp $ */
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

#if NeedFunctionPrototypes
Font XLoadFont (
    register Display *dpy,
    _Xconst char *name)
#else
Font XLoadFont (dpy, name)
    register Display *dpy;
    char *name;
#endif
{
    register long nbytes;
    Font fid;
    register xOpenFontReq *req;
    LockDisplay(dpy);
    GetReq(OpenFont, req);
    nbytes = req->nbytes = name ? strlen(name) : 0;
    req->fid = fid = XAllocID(dpy);
    req->length += (nbytes+3)>>2;
    Data (dpy, name, nbytes);
    UnlockDisplay(dpy);
    SyncHandle();
    return (fid); 
       /* can't return (req->fid) since request may have already been sent */
}

