#include <X/mit-copyright.h>

/* $Header: XWarpMouse.c,v 10.7 86/04/22 15:16:49 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XWarpMouse (w, x, y)
	Window w;
	int x, y;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_WarpMouse, w);
	req->params0 = x;
	req->params1 = y;
	req->param.l[1] = RootWindow;
	req->params4 = 0;
	req->params5 = 0;
	req->params6 = 0;
	req->params7 = 0;
}

