#include <X/mit-copyright.h>

/* $Header: XFontWidths.c,v 10.6 86/04/22 15:21:46 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

short *XFontWidths (font)
	Font font;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;
	short *buf;
	int nbytes;

	GetReq(X_FontWidths, 0);
	req->param.l[0] = font;
	if (!_XReply(dpy, &rep))
	    return(NULL);
	nbytes = rep.param.l[0];
	if ((buf = (short *) malloc(nbytes*sizeof(short)/psizeof(short))) == NULL) {
	    errno = ENOMEM;
	    _XIOError(dpy);
	}
#ifdef BIGSHORTS
	{
        ushort_p *proto_shorts = (ushort_p *) malloc (nbytes);
	_XReadPad (dpy, (char *)proto_shorts, nbytes);
	UnpackShorts(proto_shorts, buf, nbytes);
	free((char *)proto_shorts);
	}
#else
	_XReadPad(dpy, (char *)buf, nbytes);
#endif
	return (buf);
}
