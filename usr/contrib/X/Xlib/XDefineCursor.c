#include <X/mit-copyright.h>

/* $Header: XDefineCursor.c,v 10.4 86/02/01 15:31:52 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XDefineCursor (w, cursor)
	Window w;
	Cursor cursor;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_DefineCursor, w);
	req->param.l[0] = cursor;
}
