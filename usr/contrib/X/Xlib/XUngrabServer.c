#include <X/mit-copyright.h>

/* $Header: XUngrabServer.c,v 10.4 86/02/01 15:41:25 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XUngrabServer ()
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_UngrabServer, 0);
}

