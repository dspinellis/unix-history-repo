#include <X/mit-copyright.h>

/* $Header: XClear.c,v 10.4 86/02/01 15:30:20 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XClear (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_Clear, w);
}

