#include <X/mit-copyright.h>

/* $Header: XSetIconWind.c,v 10.4 86/02/01 15:39:45 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XSetIconWindow (w, iw)
	Window w, iw;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_SetIconWindow, w);
	req->param.l[0] = iw;
}

