#include <X/mit-copyright.h>

/* $Header: XPixmapGetZ.c,v 10.4 86/02/01 15:38:16 tony Rel $ */
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
    req->param.s[0] = height;
    req->param.s[1] = width;
    req->param.s[2] = x;
    req->param.s[3] = y;
    if (!_XReply (dpy, &rep))
    	return(0);
    if (dpy->dplanes > 8)
    	_XReadPad (dpy, data, WZPixmapSize (width, height));
    else 
    	_XReadPad (dpy, data, BZPixmapSize (width, height));
    return (1);
}
