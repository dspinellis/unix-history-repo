#include <X/mit-copyright.h>

/* $Header: XPixmapGetZ.c,v 10.5 86/04/22 15:20:08 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Status XPixmapGetZ (w, x, y, width, height, data)
    Window w;
    int x, y, width, height;
    caddr_t data;
{
    register Display *dpy;
    register XReq *req;
    XRep rep;

    GetReq (X_PixmapGet, w);
    req->func = ZFormat;
    req->params0 = height;
    req->params1 = width;
    req->params2 = x;
    req->params3 = y;
    if (!_XReply (dpy, &rep))
    	return(0);
    if (dpy->dplanes > 8)
    	_XReadPad (dpy, data, WZPixmapSize (width, height));
    else 
    	_XReadPad (dpy, data, BZPixmapSize (width, height));
    return (1);
}
