#include <X/mit-copyright.h>

/* $Header: XUngrabMouse.c,v 10.4 86/02/01 15:41:22 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XUngrabMouse ()
{
	register XReq *req;
	register Display *dpy;

	GetReq(X_UngrabMouse, 0);
}

