#include <X/mit-copyright.h>

/* $Header: XCircWinDn.c,v 10.4 86/02/01 15:30:11 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XCircWindowDown (w)
	register Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_CircWindowDown, w);
}

