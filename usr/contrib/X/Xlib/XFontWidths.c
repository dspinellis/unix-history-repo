#include <X/mit-copyright.h>

/* $Header: XFontWidths.c,v 10.5 86/02/01 15:33:55 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

short *XFontWidths (font)
	Font font;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;
	char *buf;

	GetReq(X_FontWidths, 0);
	req->param.l[0] = font;
	if (!_XReply(dpy, &rep))
	    return(NULL);
	if ((buf = (char *) malloc(rep.param.l[0])) == NULL) {
	    errno = ENOMEM;
	    _XIOError(dpy);
	}
	_XReadPad (dpy, buf, rep.param.l[0]);
	return ((short *) buf);
}
