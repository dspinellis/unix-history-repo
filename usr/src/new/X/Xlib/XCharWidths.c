#include <X/mit-copyright.h>

/* $Header: XCharWidths.c,v 10.5 86/04/22 15:22:44 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
/* returns number of widths on success, NULL on error */

int XCharWidths (chars, len, font, widths)
    	char *chars;  /* not necessarily null-terminated */
	int len;
	Font font;
	short *widths;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_CharWidths, 0);
	req->param.l[0] = font;
	req->params2 = len;
	Data (dpy, chars, len);
	if (!_XReply(dpy, &rep))
	    return(NULL);

#ifdef BIGSHORTS
	{
        ushort_p *proto_shorts = (ushort_p *) malloc (rep.param.l[0]);
	_XReadPad (dpy, (char *)proto_shorts, rep.param.l[0]);
	UnpackShorts(proto_shorts, widths, rep.param.l[0]);
	free((char *)proto_shorts);
	}
#else
	_XReadPad (dpy, (char *)widths, rep.param.l[0]);
#endif
	return(rep.param.l[0] >> 1);
}

