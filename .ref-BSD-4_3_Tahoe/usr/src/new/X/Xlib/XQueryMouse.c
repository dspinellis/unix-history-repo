#include <X/mit-copyright.h>

/* $Header: XQueryMouse.c,v 10.5 86/04/22 15:25:29 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Status XQueryMouse (w, x, y, subw)
	Window w;
	Window *subw;
	int *x, *y;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_QueryMouse, w);
	if (!_XReply(dpy, &rep))
	    return (0);
	*x = rep.params2;
	*y = rep.params3;
	*subw = rep.param.l[0];
	return (1);
}

