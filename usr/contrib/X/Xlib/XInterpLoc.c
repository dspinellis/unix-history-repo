#include <X/mit-copyright.h>

/* $Header: XInterpLoc.c,v 10.4 86/02/01 15:35:41 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Status XInterpretLocator (w, x, y, subw, loc)
	Window w;
	Window *subw;
	Locator loc;
	int *x, *y;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_InterpretLocator, w);
	req->param.l[0] = loc;
	if (!_XReply(dpy, &rep))
	    return(0);
	*x = rep.param.s[2];
	*y = rep.param.s[3];
	*subw = rep.param.l[0];
	return (1);
}

