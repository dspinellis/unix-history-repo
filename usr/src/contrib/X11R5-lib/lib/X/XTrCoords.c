/* $XConsortium: XTrCoords.c,v 11.14 91/01/06 11:48:31 rws Exp $ */
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

Bool XTranslateCoordinates(dpy, src_win, dest_win, src_x, src_y, 
		      dst_x, dst_y, child)
     register Display *dpy;
     Window src_win, dest_win;
     int src_x, src_y;
     int *dst_x, *dst_y;
     Window *child;
{       
    register xTranslateCoordsReq *req;
    xTranslateCoordsReply rep;

    LockDisplay(dpy);
    GetReq(TranslateCoords, req);
    req->srcWid = src_win;
    req->dstWid = dest_win;
    req->srcX = src_x;
    req->srcY = src_y;
    if (_XReply (dpy, (xReply *)&rep, 0, xTrue) == 0) {
	    UnlockDisplay(dpy);
	    SyncHandle();
	    return(False);
	}
	
    *child = rep.child;
    *dst_x = cvtINT16toInt (rep.dstX);
    *dst_y = cvtINT16toInt (rep.dstY);
    UnlockDisplay(dpy);
    SyncHandle();
    return ((int)rep.sameScreen);
}

