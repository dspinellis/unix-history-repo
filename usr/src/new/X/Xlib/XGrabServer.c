#include <X/mit-copyright.h>

/* $Header: XGrabServer.c,v 10.4 86/02/01 15:35:31 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XGrabServer ()
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_GrabServer, 0);
}

