#include <X/mit-copyright.h>

/* $Header: XConfWindow.c,v 10.4 86/02/01 15:30:45 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XConfigureWindow (w, x, y, width, height)
	Window w;
	int x, y, width, height;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_ConfigureWindow, w);
	req->func = 0;  /* "for future expansion", sez X.doc */
	req->param.s[0] = height;
	req->param.s[1] = width;
	req->param.s[2] = x;
	req->param.s[3] = y;
}

