#include <X/mit-copyright.h>

/* $Header: XClrIconWind.c,v 10.4 86/02/01 15:30:39 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XClearIconWindow (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_SetIconWindow, w);
	req->param.l[0] = 0;
}

