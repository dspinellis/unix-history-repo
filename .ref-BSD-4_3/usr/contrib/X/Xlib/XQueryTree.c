#include <X/mit-copyright.h>

/* $Header: XQueryTree.c,v 10.4 86/02/01 15:39:07 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Status XQueryTree (w, parent, nchildren, children)
	Window w;
	Window *parent;
	int *nchildren;
	Window **children;

{
	register Display *dpy;
	register XReq *req;
	XRep rep;
	int nbytes;

	GetReq(X_QueryTree, w);

	if (!_XReply(dpy, &rep))
	    return (0);
	*parent = rep.param.l[0];
	if ((*nchildren = rep.param.l[1]) == 0) {
	    *children = NULL;
	    return (1);
	    }
	
	if ((*children = (Window *) malloc (nbytes = rep.param.l[1]*sizeof(Window))) == NULL)
	    _XIOError (dpy);

	_XRead (dpy, *children, nbytes);
	return (1);
	}
