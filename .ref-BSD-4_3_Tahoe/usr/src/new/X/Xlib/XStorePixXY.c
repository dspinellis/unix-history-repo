#include <X/mit-copyright.h>

/* $Header: XStorePixXY.c,v 10.6 86/04/22 15:26:55 jg Rel $ */
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
	req->params0 = height;
	req->params1 = width;
	len = XYPixmapSize (width, height, dpy->dplanes);
	PackData(dpy, data, len);
	if (!_XReply(dpy, &rep))
	    return(NULL);
	return (rep.param.l[0]);
}

