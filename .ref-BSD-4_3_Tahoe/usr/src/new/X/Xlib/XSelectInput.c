#include <X/mit-copyright.h>

/* $Header: XSelectInput.c,v 10.5 86/04/22 15:16:52 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XSelectInput (w, mask)
	Window w;
	unsigned int mask;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_SelectInput, w);
	req->param.l[0] = mask;
}

