#include <X/mit-copyright.h>

/* $Header: XFreeColors.c,v 10.4 86/02/01 15:34:07 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XFreeColors (pixels, npixels, planes)
    	register int *pixels;
	int npixels;
	int planes;
{
	register Display *dpy;
	register XReq *req;
	
	GetReq (X_FreeColors, 0);
	req->mask = planes;
	req->param.s[0] = npixels;

	/* "pixels" is an array of ints, but the protocol wants
	 * an array of shorts, therefore data must be copied */
	{
	register int i;
	int nbytes = npixels*sizeof(short);
	register u_short *proto_pixels = (u_short *) malloc (nbytes);
	for (i=0;i<npixels;i++)
	    proto_pixels[i] = pixels[i];
	Data (dpy, proto_pixels, nbytes);
	free (proto_pixels);
	}
}

