#include <X/mit-copyright.h>

/* $Header: XUngrabButton.c,v 10.4 86/02/01 15:41:18 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XUngrabButton (mask)
	int mask;
{
	register XReq *req;
	register Display *dpy;

	GetReq(X_UngrabButton, 0);
	req->mask = mask;
}

