#include <X/mit-copyright.h>

/* $Header: XStoreBytes.c,v 10.5 86/04/22 15:28:34 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XStoreBytes (str, length)

	char *str;
	int length;
{
	register XReq *req;
	register Display *dpy;

	GetReq(X_StoreBytes, 0);
	req->params0 = length;
	req->func = 0;  /* cut buffer number 0 */
	Data(dpy, str, length);
}

