#include <X/mit-copyright.h>

/* $Header: XGetColCells.c,v 10.5 86/04/22 15:30:25 jg Rel $ */
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
	req->params0 = ncolors;
	req->params1 = nplanes;
	if (!_XReply (dpy, &rep)) {
	    *planes = 0;
	    return (0);
	    }
	*planes = rep.paramu0;
	if (!ncolors)
	    return (1);

	/* "pixels" is an array of ints, but the protocol
	 * returns an array of shorts.  Therefore data must be copied. */
	{
	register int i;
	int nbytes = ncolors*2;
#ifdef BIGSHORTS
	register ushort_p *proto_pixels = (ushort_p *) malloc (nbytes);
	_XReadPad (dpy, (char *)proto_pixels, nbytes);
	UnpackShorts(proto_pixels, (short *)pixels, nbytes);
#else
	register u_short *proto_pixels = (u_short *) malloc (nbytes);
	_XReadPad (dpy, proto_pixels, nbytes);
	for (i=0;i<ncolors;i++)
	    pixels[i] = proto_pixels[i];
#endif
	free ((char *)proto_pixels);
	return (1);
	}
}

