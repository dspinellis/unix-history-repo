#include <X/mit-copyright.h>

/* $Header: XStoreName.c,v 10.4 86/02/01 15:40:20 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XStoreName (w, name)
	Window w;
	char *name;
{
	register Display *dpy;
	register XReq *req;
	int len;

	GetReq(X_StoreName, w);
	len = req->param.s[0] = strlen(name);
	Data(dpy, name, len);
}

