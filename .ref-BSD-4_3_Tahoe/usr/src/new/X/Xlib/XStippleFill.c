#include <X/mit-copyright.h>
 
/* $Header: XStippleFill.c,v 10.2 86/04/22 15:15:05 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/
 
#include "XlibInternal.h"
XStippleFill (w, x, y, width, height, pixel, stipmask, func, planes)
	Window w;
	int x, y;
	int width, height;
	int pixel;
	Bitmap stipmask;
	int func, planes;
{
	register Display *dpy;
	register XReq *req;
 
	GetReq(X_StippleFill, w);
	req->mask = planes;
	req->func = func;
	req->params0 = height;
	req->params1 = width;
	req->params2 = x;
	req->params3 = y;
	req->paramu4 = pixel;
	req->param.l[3] = stipmask;
}
 

