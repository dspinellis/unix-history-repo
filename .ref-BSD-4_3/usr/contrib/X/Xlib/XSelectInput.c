#include <X/mit-copyright.h>

/* $Header: XSelectInput.c,v 10.4 86/02/01 15:39:39 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XSelectInput (w, mask)
	Window w;
	int mask;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_SelectInput, w);
	req->param.l[0] = mask;
}

