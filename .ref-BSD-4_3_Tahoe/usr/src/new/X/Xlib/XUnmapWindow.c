#include <X/mit-copyright.h>

/* $Header: XUnmapWindow.c,v 10.4 86/02/01 15:41:35 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XUnmapWindow (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_UnmapWindow, w);
}

