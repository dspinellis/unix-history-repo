#include <X/mit-copyright.h>

/* $Header: XFreePixmap.c,v 10.5 86/02/01 15:34:23 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XFreePixmap (pixmap)
    	register Pixmap pixmap;
{
	register Display *dpy = _XlibCurrentDisplay;
	register XReq *req;
	
	if (pixmap == dpy->black || pixmap == dpy->white)
	    return;  /* don't free the constant tile pixmaps! */

	GetReq(X_FreePixmap, 0);
	req->param.l[0] = pixmap;
	return;
}

