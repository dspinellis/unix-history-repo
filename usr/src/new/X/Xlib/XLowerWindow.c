#include <X/mit-copyright.h>

/* $Header: XLowerWindow.c,v 10.4 86/02/01 15:36:28 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XLowerWindow (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_LowerWindow, w);
}

