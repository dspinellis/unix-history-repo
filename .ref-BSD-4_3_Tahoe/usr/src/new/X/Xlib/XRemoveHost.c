#include <X/mit-copyright.h>

/* $Header: XRemoveHost.c,v 10.4 86/02/01 15:39:29 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XRemoveHost (host)
	struct in_addr *host;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_RemoveHost, 0);
	req->func = XAF_INET;
	req->param.l[0] = host->s_addr;
}

