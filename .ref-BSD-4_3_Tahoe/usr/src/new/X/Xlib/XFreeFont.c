#include <X/mit-copyright.h>

/* $Header: XFreeFont.c,v 10.4 86/02/01 15:34:19 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XFreeFont (font)
	Font font;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_FreeFont, 0);
	req->param.l[0] = font;
}

