#include <X/mit-copyright.h>

/* $Header: XFocusKbd.c,v 10.4 86/02/01 15:33:49 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XFocusKeyboard (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_FocusKeyboard, w);
	req->func = 0;  /* "for future expansion", sez X.doc */
}

