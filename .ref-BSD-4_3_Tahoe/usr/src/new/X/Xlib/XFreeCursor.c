#include <X/mit-copyright.h>

/* $Header: XFreeCursor.c,v 10.4 86/02/01 15:34:13 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XFreeCursor (cursor)
	Cursor cursor;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_FreeCursor, 0);
	req->param.l[0] = cursor;
}

