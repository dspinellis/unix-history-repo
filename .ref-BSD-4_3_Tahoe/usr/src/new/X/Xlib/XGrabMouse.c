#include <X/mit-copyright.h>

/* $Header: XGrabMouse.c,v 10.4 86/02/01 15:35:26 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Status XGrabMouse (w, cursor, mask)
	Window w;
	Cursor cursor;
	int mask;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_GrabMouse, w);
	req->param.l[0] = cursor;
	req->param.l[1] = mask;
	return(_XReply(dpy, &rep));
}

