#include <X/mit-copyright.h>

/* $Header: XDestWindow.c,v 10.4 86/02/01 15:32:17 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XDestroyWindow (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_DestroyWindow, w);
}

