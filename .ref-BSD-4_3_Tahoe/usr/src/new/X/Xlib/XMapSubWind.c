#include <X/mit-copyright.h>

/* $Header: XMapSubWind.c,v 10.4 86/02/01 15:36:55 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XMapSubwindows (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_MapSubwindows, w);
}









