#include <X/mit-copyright.h>

/* $Header: XSetResHint.c,v 10.5 86/04/22 15:28:03 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XSetResizeHint (w, width0, height0, widthinc, heightinc)
	Window w;
	int width0, widthinc, height0, heightinc;

{
	register Display *dpy;
	register XReq *req;

	GetReq(X_SetResizeHint, w);
	req->params0 = height0;
	req->params1 = heightinc;
	req->params2 = width0;
	req->params3 = widthinc;
}

