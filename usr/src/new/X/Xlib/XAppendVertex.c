#include <X/mit-copyright.h>

/* $Header: XAppendVertex.c,v 10.6 86/04/22 15:19:07 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XAppendVertex (vertices, nvert)
    Vertex vertices[];
    int nvert;
    {
    register Display *dpy = _XlibCurrentDisplay;
    register XReq *lastdraw;
    register char *old_bufptr, *new_bufptr;
    int nbytes;

    if ((lastdraw = (XReq*)(dpy->lastdraw)) == NULL)
    	return (0);  /* no draw command in output buffer */

    old_bufptr = dpy->bufptr;
    /* subtract off any padding */
    if (lastdraw->params0 & 1)
	old_bufptr -= 2;
    new_bufptr = old_bufptr + (nbytes = nvert*sizeof(Vertex));
    if (new_bufptr >= dpy->bufmax)
    	return (-1); /* vertices don't fit */
    
    /* increment number of vertices in XDraw packet */
    lastdraw->params0 += nvert;
    PackShorts(vertices, old_bufptr, nbytes);
    /* add in padding if needed */
    if (lastdraw->params0 & 1)
	new_bufptr += 2;
    dpy->bufptr = new_bufptr;
    return (nvert);
    }
