#include <X/mit-copyright.h>

/* $Header: XMoveWindow.c,v 10.4 86/02/01 15:37:20 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XMoveWindow (w, x, y)
	Window w;
	int x, y;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_MoveWindow, w);
	req->func = 0;  /* "for future expansion", sez X.doc */
	req->param.s[0] = x;
	req->param.s[1] = y;
}

