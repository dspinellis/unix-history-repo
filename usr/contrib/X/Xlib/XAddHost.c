#include <X/mit-copyright.h>

/* $Header: XAddHost.c,v 10.6 86/02/01 15:29:19 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
#include <sys/socket.h>
XAddHost (host)
	struct in_addr *host;
{
	register Display *dpy;
	register XReq *req;

	GetReq (X_AddHost, 0);
	req->func = XAF_INET;
	req->param.l[0] = host->s_addr;
}

