#include <X/mit-copyright.h>

/* $Header: XTileRelative.c,v 10.4 86/02/01 15:41:08 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XTileRelative (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_TileMode, w);
	req->func = TileModeRelative;
}

