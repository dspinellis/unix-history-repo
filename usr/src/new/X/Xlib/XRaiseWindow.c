#include <X/mit-copyright.h>

/* $Header: XRaiseWindow.c,v 10.5 86/04/22 15:22:23 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XRaiseWindow (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_RaiseWindow, w);
}

