#include <X/mit-copyright.h>

/* $Header: XFetchName.c,v 10.5 86/04/22 15:29:04 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

Status XFetchName (w, name)
	Window w;
	char **name;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;
	register int nbytes;

	GetReq(X_FetchName, w);
	if (!_XReply(dpy, &rep)) {
	    /* error */
	    *name = NULL;
	    return(0);
	    }
	if ((nbytes = rep.params0) == 0) {
	    /* no name set */
	    *name = NULL;
	    return(1);
	    }
	if ((*name = (char *) malloc(nbytes + 1)) == NULL) {
	    errno = ENOMEM;
	    _XIOError(dpy);
	    }
	_XReadPad (dpy, *name, nbytes);
	(*name)[nbytes] = '\0';
	return (1);
}

