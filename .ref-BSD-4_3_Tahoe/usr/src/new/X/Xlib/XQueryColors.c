#include <X/mit-copyright.h>

/* $Header: XQueryColors.c,v 10.5 86/04/22 15:23:30 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
#define MAXREQS 80  /* to prevent deadlock due to full TCP buffers */

XQueryColors (colors, ncolors)
    	Color colors[];
	int ncolors;
{
	register Display *dpy;
	register XReq *req;
	register int i;
	register Color *color = colors;
	XRep rep;

	while (ncolors > 0) {
	    register int nreqs = (ncolors > MAXREQS) ? MAXREQS : ncolors;
	    for (i=0;i<nreqs;i++) {
	     	GetReq (X_QueryColor, 0);
	     	req->paramu0 = (color++)->pixel;
	     	}

	    /* Reset request number, so error packets are handled correctly */
	    dpy->request -= nreqs;
	    color -= nreqs;

	    for (i=0;i<nreqs;i++) {
	     	/* Increment request number, so error packets are handled
	    	    correctly */
	     	dpy->request++;
	     	_XReply (dpy, &rep);
	     	color->red = rep.paramu0;
	     	color->green = rep.paramu1;
	     	(color++)->blue = rep.paramu2;
	    	}
	    
	    ncolors -= nreqs;
	    }
}

