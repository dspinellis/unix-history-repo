#include <X/mit-copyright.h>

/* $Header: XStoreBuffer.c,v 10.4 86/02/01 15:39:58 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XStoreBuffer (str, nbytes, buffer)

	char *str;
	int nbytes;
{
	register XReq *req;
	register Display *dpy;

	GetReq(X_StoreBytes, 0);
	req->param.s[0] = nbytes;
	req->func = buffer;
	Data(dpy, str, nbytes);
}

