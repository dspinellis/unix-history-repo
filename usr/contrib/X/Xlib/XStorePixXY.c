#include <X/mit-copyright.h>

/* $Header: XStorePixXY.c,v 10.5 86/02/01 15:40:23 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Pixmap XStorePixmapXY (width, height, data)
	int width, height;
	short *data;
{
    	register Display *dpy;
	register XReq *req;
	XRep rep;
	int len;

	GetReq(X_StorePixmap, 0);
	req->func = XYFormat;
	req->param.s[0] = height;
	req->param.s[1] = width;
	len = XYPixmapSize (width, height, dpy->dplanes);
	Data(dpy, (char *)data, len);
	if (!_XReply(dpy, &rep))
	    return(NULL);
	return (rep.param.l[0]);
}

