/* $XConsortium: XCrWindow.c,v 11.13 91/01/06 11:45:00 rws Exp $ */
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

Window XCreateSimpleWindow(dpy, parent, x, y, width, height, 
                      borderWidth, border, background)
    register Display *dpy;
    Window parent;
    int x, y;
    unsigned int width, height, borderWidth;
    unsigned long border;
    unsigned long background;
{
    Window wid;
    register xCreateWindowReq *req;

    LockDisplay(dpy);
    GetReqExtra(CreateWindow, 8, req);
    req->parent = parent;
    req->x = x;
    req->y = y;
    req->width = width;
    req->height = height;
    req->borderWidth = borderWidth;
    req->depth = 0;
    req->class = CopyFromParent;
    req->visual = CopyFromParent;
    wid = req->wid = XAllocID(dpy);
    req->mask = CWBackPixel | CWBorderPixel;

#ifdef MUSTCOPY
    {
	unsigned long lbackground = background, lborder = border;
	dpy->bufptr -= 8;
	Data32 (dpy, (long *) &lbackground, 4);
	Data32 (dpy, (long *) &lborder, 4);
    }
#else
    {
	register unsigned long *valuePtr =
	  (unsigned long *) NEXTPTR(req,xCreateWindowReq);
	*valuePtr++ = background;
	*valuePtr = border;
    }
#endif /* MUSTCOPY */

    UnlockDisplay(dpy);
    SyncHandle();
    return (wid);
    }
