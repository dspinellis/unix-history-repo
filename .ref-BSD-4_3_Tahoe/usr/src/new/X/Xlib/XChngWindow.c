#include <X/mit-copyright.h>

/* $Header: XChngWindow.c,v 10.5 86/04/22 15:22:52 jg Rel $ */
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
	req->params0 = height;
	req->params1 = width;
}

