#include <X/mit-copyright.h>

/* $Header: XUnmapTrans.c,v 10.4 86/02/01 15:41:32 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XUnmapTransparent (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_UnmapTransparent, w);
}

