#include <X/mit-copyright.h>

/* $Header: XSetResHint.c,v 10.4 86/02/01 15:39:49 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XSetResizeHint (w, width0, height0, widthinc, heightinc)
	Window w;
	int width0, widthinc, height0, heightinc;

{
	register Display *dpy;
	register XReq *req;

	GetReq(X_SetResizeHint, w);
	req->param.s[0] = height0;
	req->param.s[1] = heightinc;
	req->param.s[2] = width0;
	req->param.s[3] = widthinc;
}

