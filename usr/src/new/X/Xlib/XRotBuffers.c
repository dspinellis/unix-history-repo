#include <X/mit-copyright.h>

/* $Header: XRotBuffers.c,v 10.4 86/02/01 15:39:34 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"


XRotateBuffers (n)
    	int n;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_RotateCuts, 0);
	req->func = n;
}

