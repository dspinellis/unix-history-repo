#include <X/mit-copyright.h>

/* $Header: XUnmapSubs.c,v 10.4 86/02/01 15:41:30 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XUnmapSubwindows (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_UnmapSubwindows, w);
}

