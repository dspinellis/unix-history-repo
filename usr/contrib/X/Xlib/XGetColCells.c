#include <X/mit-copyright.h>

/* $Header: XGetColCells.c,v 10.4 86/02/01 15:34:32 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Status XGetColorCells (contig, ncolors, nplanes, planes, pixels)
    	int contig;
	int ncolors;
	int nplanes;
	int *planes;
	register int pixels[];
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq (X_GetColorCells, 0);
	req->func = contig;
	req->param.s[0] = ncolors;
	req->param.s[1] = nplanes;
	if (!_XReply (dpy, &rep)) {
	    *planes = 0;
	    return (0);
	    }
	*planes = rep.param.u[0];
	if (!ncolors)
	    return (1);

	/* "pixels" is an array of ints, but the protocol
	 * returns an array of shorts.  Therefore data must be copied. */
	{
	register int i;
	int nbytes = ncolors*2;
	register u_short *proto_pixels = (u_short *) malloc (nbytes);
	_XReadPad (dpy, proto_pixels, nbytes);
	for (i=0;i<ncolors;i++)
	    pixels[i] = proto_pixels[i];
	free (proto_pixels);
	return (1);
	}
}

