/* $XConsortium: XChWindow.c,v 11.10 91/01/06 11:44:26 rws Exp $ */
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

XResizeWindow(dpy, w, width, height)
register Display *dpy;
Window w;
unsigned int width, height;
{
    register xConfigureWindowReq *req;

    LockDisplay(dpy);
    GetReqExtra(ConfigureWindow, 8, req); /* 2 4-byte quantities */

    /* XXX assuming that sizeof(unsigned long) is 32 bits */

    req->window = w;
    req->mask = CWWidth | CWHeight;
#ifdef MUSTCOPY
    {
	unsigned long lwidth = width, lheight = height;
    dpy->bufptr -= 8;
    Data32 (dpy, (long *) &lwidth, 4);	/* order dictated by values of */
    Data32 (dpy, (long *) &lheight, 4);	/* CWWidth and CWHeight */
    }
#else
    {
	unsigned long * valuePtr =
	  (unsigned long *) NEXTPTR(req,xConfigureWindowReq);
	*valuePtr++ = width;
	*valuePtr++ = height;
    }
#endif /* MUSTCOPY */
    UnlockDisplay(dpy);
    SyncHandle();
}
