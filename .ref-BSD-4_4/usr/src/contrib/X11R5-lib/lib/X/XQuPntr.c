/* $XConsortium: XQuPntr.c,v 11.15 91/01/06 11:47:34 rws Exp $ */
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

Bool XQueryPointer(dpy, w, root, child, root_x, root_y, win_x, win_y, mask)
     register Display *dpy;
     Window w, *root, *child;
     int *root_x, *root_y, *win_x, *win_y;
     unsigned int *mask;

{       
    xQueryPointerReply rep;
    xResourceReq *req;

    LockDisplay(dpy);
    GetResReq(QueryPointer, w, req);
    if (_XReply (dpy, (xReply *)&rep, 0, xTrue) == 0) {
	    UnlockDisplay(dpy);
	    SyncHandle();
	    return(False);
	}

    *root = rep.root;
    *child = rep.child;
    *root_x = cvtINT16toInt (rep.rootX);
    *root_y = cvtINT16toInt (rep.rootY);
    *win_x = cvtINT16toInt (rep.winX);
    *win_y = cvtINT16toInt (rep.winY);
    *mask = rep.mask;
    UnlockDisplay(dpy);
    SyncHandle();
    return (rep.sameScreen);
}

