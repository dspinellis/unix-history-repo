#include <X/mit-copyright.h>

/* $Header: XStoreName.c,v 10.5 86/04/22 15:30:11 jg Rel $ */
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
	len = req->params0 = strlen(name);
	Data(dpy, name, len);
}

