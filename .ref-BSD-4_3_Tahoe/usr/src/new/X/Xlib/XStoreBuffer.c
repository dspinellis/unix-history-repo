#include <X/mit-copyright.h>

/* $Header: XStoreBuffer.c,v 10.5 86/04/22 15:28:09 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XStoreBuffer (str, nbytes, buffer)

	char *str;
	int nbytes;
{
	register XReq *req;
	register Display *dpy;

	GetReq(X_StoreBytes, 0);
	req->params0 = nbytes;
	req->func = buffer;
	Data(dpy, str, nbytes);
}

