#include <X/mit-copyright.h>

/* $Header: XCreateTrans.c,v 10.5 86/04/22 15:25:31 jg Rel $ */
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
	req->params0 = height;
	req->params1 = width;
	req->params2 = x;
	req->params3 = y;
	if (!_XReply(dpy, &rep))
	    return(NULL);
	return (rep.param.l[0]);
}

