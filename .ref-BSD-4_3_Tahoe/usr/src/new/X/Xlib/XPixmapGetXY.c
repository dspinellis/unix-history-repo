#include <X/mit-copyright.h>

/* $Header: XPixmapGetXY.c,v 10.6 86/04/22 15:19:40 jg Rel $ */
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
    req->params0 = height;
    req->params1 = width;
    req->params2 = x;
    req->params3 = y;
    if (!_XReply (dpy, &rep))
    	return(0);
    _XReadPad (dpy, (char *)data, XYPixmapSize (width, height, dpy->dplanes));
    return (1);
}
