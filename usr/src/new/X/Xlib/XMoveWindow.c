#include <X/mit-copyright.h>

/* $Header: XMoveWindow.c,v 10.5 86/04/22 15:16:10 jg Rel $ */
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
	req->params0 = x;
	req->params1 = y;
}

