#include <X/mit-copyright.h>

/* $Header: XFreeColors.c,v 10.5 86/04/22 15:19:33 jg Rel $ */
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
	req->params0 = npixels;

	/* "pixels" is an array of ints, but the protocol wants
	 * an array of shorts, therefore data must be copied */
	{
	register int i;
	int nbytes = npixels*psizeof(short);
#ifdef BIGSHORTS
	PackData(dpy, (short *)pixels, nbytes);
#else
	register u_short *proto_pixels = (u_short *) malloc (nbytes);
	for (i=0;i<npixels;i++)
	    proto_pixels[i] = pixels[i];
	Data (dpy, proto_pixels, nbytes);
	free ((char *)proto_pixels);
#endif
	}
}

