#include <X/mit-copyright.h>

/* $Header: XStoreBytes.c,v 10.4 86/02/01 15:40:04 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XStoreBytes (str, length)

	char *str;
	int length;
{
	register XReq *req;
	register Display *dpy;

	GetReq(X_StoreBytes, 0);
	req->param.s[0] = length;
	req->func = 0;  /* cut buffer number 0 */
	Data(dpy, str, length);
}

