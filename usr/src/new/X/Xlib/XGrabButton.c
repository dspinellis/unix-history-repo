#include <X/mit-copyright.h>

/* $Header: XGrabButton.c,v 10.4 86/02/01 15:35:22 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Status XGrabButton (w, cursor, buttonMask, eventMask)
	Window w;
	Cursor cursor;
	int buttonMask, eventMask;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_GrabButton, w);
	req->mask = buttonMask;
	req->param.l[0] = cursor;
	req->param.l[1] = eventMask;
	return((_XReply(dpy, &rep)));
}

