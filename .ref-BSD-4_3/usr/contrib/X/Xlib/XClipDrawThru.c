#include <X/mit-copyright.h>

/* $Header: XClipDrawThru.c,v 10.4 86/02/01 15:30:28 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XClipDrawThrough (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_ClipMode, w);
	req->func = ClipModeDrawThru;
}

