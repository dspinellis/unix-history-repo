/* $XConsortium: XWarpPtr.c,v 11.7 91/01/06 11:48:47 rws Exp $ */
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

XWarpPointer(dpy, src_win, dest_win, src_x, src_y, src_width, src_height,
	     dest_x, dest_y)
     register Display *dpy;
     Window src_win, dest_win;
     int src_x, src_y;
     unsigned int src_width, src_height;
     int dest_x, dest_y;
{       
    register xWarpPointerReq *req;

    LockDisplay(dpy);
    GetReq(WarpPointer, req);
    req->srcWid = src_win;
    req->dstWid = dest_win;
    req->srcX = src_x;
    req->srcY = src_y;
    req->srcWidth = src_width;
    req->srcHeight = src_height;
    req->dstX = dest_x;
    req->dstY = dest_y;
    UnlockDisplay(dpy);
    SyncHandle();
}

