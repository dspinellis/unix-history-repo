#include <X/mit-copyright.h>

/* $Header: XDestSubWind.c,v 10.4 86/02/01 15:32:11 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XDestroySubwindows (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_DestroySubwindows, w);
}

