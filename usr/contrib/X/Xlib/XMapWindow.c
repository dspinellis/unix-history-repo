#include <X/mit-copyright.h>

/* $Header: XMapWindow.c,v 10.4 86/02/01 15:37:00 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XMapWindow (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_MapWindow, w);
}

