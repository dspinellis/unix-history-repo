#include <X/mit-copyright.h>

/* $Header: XClipClippd.c,v 10.4 86/02/01 15:30:25 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XClipClipped (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_ClipMode, w);
	req->func = ClipModeClipped;
}

