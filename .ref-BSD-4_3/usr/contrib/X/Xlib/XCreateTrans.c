#include <X/mit-copyright.h>

/* $Header: XCreateTrans.c,v 10.4 86/02/01 15:31:27 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Window XCreateTransparency (parent, x, y, width, height)
	int x, y, width, height;
	Window parent;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_CreateTransparency, parent);
	req->param.s[0] = height;
	req->param.s[1] = width;
	req->param.s[2] = x;
	req->param.s[3] = y;
	if (!_XReply(dpy, &rep))
	    return(NULL);
	return (rep.param.l[0]);
}

