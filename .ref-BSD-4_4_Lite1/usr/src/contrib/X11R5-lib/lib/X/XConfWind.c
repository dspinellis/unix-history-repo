/* $XConsortium: XConfWind.c,v 11.10 91/01/06 11:44:40 rws Exp $ */
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

XMoveResizeWindow(dpy, w, x, y, width, height)
register Display *dpy;
Window w;
int x, y;
unsigned int width, height;
{
    register xConfigureWindowReq *req;

    LockDisplay(dpy);
    GetReqExtra(ConfigureWindow, 16, req);
    req->window = w;
    req->mask = CWX | CWY | CWWidth | CWHeight;
#ifdef MUSTCOPY
    {
	long lx = x, ly = y;
	unsigned long lwidth = width, lheight = height;

	dpy->bufptr -= 16;
	Data32 (dpy, (long *) &lx, 4);	/* order must match values of */
	Data32 (dpy, (long *) &ly, 4);	/* CWX, CWY, CWWidth, and CWHeight */
	Data32 (dpy, (long *) &lwidth, 4);
	Data32 (dpy, (long *) &lheight, 4);
    }
#else
    {
	register unsigned long *valuePtr =
	  (unsigned long *) NEXTPTR(req,xConfigureWindowReq);
	*valuePtr++ = x;
	*valuePtr++ = y;
	*valuePtr++ = width;
	*valuePtr   = height;
    }
#endif /* MUSTCOPY */
    UnlockDisplay(dpy);
    SyncHandle();
}
