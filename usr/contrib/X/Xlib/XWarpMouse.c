#include <X/mit-copyright.h>

/* $Header: XWarpMouse.c,v 10.6 86/02/01 15:41:44 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XWarpMouse (w, x, y)
	Window w;
	int x, y;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_WarpMouse, w);
	req->param.s[0] = x;
	req->param.s[1] = y;
	req->param.l[1] = RootWindow;
	req->param.s[4] = 0;
	req->param.s[5] = 0;
	req->param.s[6] = 0;
	req->param.s[7] = 0;
}

