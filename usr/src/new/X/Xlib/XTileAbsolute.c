#include <X/mit-copyright.h>

/* $Header: XTileAbsolute.c,v 10.4 86/02/01 15:41:01 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XTileAbsolute (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_TileMode, w);
	req->func = TileModeAbsolute;
}

