#include <X/mit-copyright.h>

/* $Header: XAppendBuffer.c,v 1.1 86/04/11 23:21:58 newman Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XAppendToBuffer (str, length, buffer)

	char *str;
	int length;
{
	register XReq *req;
	register Display *dpy;

	GetReq(X_AppendBytes, 0);
	req->params0 = length;
	req->func = buffer;
	Data(dpy, str, length);
}

