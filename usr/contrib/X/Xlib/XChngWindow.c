#include <X/mit-copyright.h>

/* $Header: XChngWindow.c,v 10.4 86/02/01 15:30:06 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XChangeWindow (w, width, height)
	Window w;
	int width, height;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_ChangeWindow, w);
	req->func = 0;  /* "for future expansion", sez X.doc */
	req->param.s[0] = height;
	req->param.s[1] = width;
}

