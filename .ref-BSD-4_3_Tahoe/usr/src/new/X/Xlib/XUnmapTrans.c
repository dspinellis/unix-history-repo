#include <X/mit-copyright.h>

/* $Header: XUnmapTrans.c,v 10.5 86/04/22 15:31:02 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XUnmapTransparent (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_UnmapTransparent, w);
}

