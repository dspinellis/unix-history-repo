#include <X/mit-copyright.h>

/* $Header: XQueryCursor.c,v 10.4 86/02/01 15:38:56 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XQueryCursorShape (width, height, rwidth, rheight)
    	int width, height, *rwidth, *rheight;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_QueryShape, 0);
	req->func = CursorShape;
	req->param.s[0] = height;
	req->param.s[1] = width;
	_XReply (dpy, &rep);
	*rheight = rep.param.s[0];
	*rwidth = rep.param.s[1];
}

