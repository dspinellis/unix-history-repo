#include <X/mit-copyright.h>

/* $Header: XConfWindow.c,v 10.5 86/04/22 15:24:00 jg Rel $ */
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
	req->params0 = height;
	req->params1 = width;
	req->params2 = x;
	req->params3 = y;
}

