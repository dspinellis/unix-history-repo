#include <X/mit-copyright.h>

/* $Header: XWarpCond.c,v 10.5 86/04/22 15:16:06 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XCondWarpMouse (dw, x, y, sw, left, top, width, height)
	Window dw, sw;
	int x, y, left, top, width, height;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_WarpMouse, dw);
	req->params0 = x;
	req->params1 = y;
	req->param.l[1] = sw;
	req->params4 = height;
	req->params5 = width;
	req->params6 = left;
	req->params7 = top;
}

