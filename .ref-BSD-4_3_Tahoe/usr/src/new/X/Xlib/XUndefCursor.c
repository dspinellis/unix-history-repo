#include <X/mit-copyright.h>

/* $Header: XUndefCursor.c,v 10.4 86/02/01 15:41:14 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XUndefineCursor (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_DefineCursor, w);
	req->param.l[0] = 0;
}
