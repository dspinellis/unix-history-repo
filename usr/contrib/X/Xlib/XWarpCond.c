#include <X/mit-copyright.h>

/* $Header: XWarpCond.c,v 10.4 86/02/01 15:41:41 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XCondWarpMouse (dw, x, y, sw, left, top, width, height)
	Window dw, sw;
	int x, y, left, top, width, height;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_WarpMouse, dw);
	req->param.s[0] = x;
	req->param.s[1] = y;
	req->param.l[1] = sw;
	req->param.s[4] = height;
	req->param.s[5] = width;
	req->param.s[6] = left;
	req->param.s[7] = top;
}

