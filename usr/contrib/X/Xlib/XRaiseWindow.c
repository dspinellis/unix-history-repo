#include <X/mit-copyright.h>

/* $Header: XRaiseWindow.c,v 10.4 86/02/01 15:39:21 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XRaiseWindow (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_RaiseWindow, w);
}

