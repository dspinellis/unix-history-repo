#include <X/mit-copyright.h>

/* $Header: XCircWinUp.c,v 10.4 86/02/01 15:30:15 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XCircWindowUp (w)
	register Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_CircWindowUp, w);
}

