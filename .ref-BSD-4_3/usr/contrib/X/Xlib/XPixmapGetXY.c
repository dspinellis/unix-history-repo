#include <X/mit-copyright.h>

/* $Header: XPixmapGetXY.c,v 10.5 86/02/01 15:38:13 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Status XPixmapGetXY (w, x, y, width, height, data)
    Window w;
    int x, y, width, height;
    short *data;
{
    register Display *dpy;
    register XReq *req;
    XRep rep;

    GetReq (X_PixmapGet, w);
    req->func = XYFormat;
    req->param.s[0] = height;
    req->param.s[1] = width;
    req->param.s[2] = x;
    req->param.s[3] = y;
    if (!_XReply (dpy, &rep))
    	return(0);
    _XReadPad (dpy, (char *)data, XYPixmapSize (width, height, dpy->dplanes));
    return (1);
}
