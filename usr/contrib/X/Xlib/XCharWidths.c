#include <X/mit-copyright.h>

/* $Header: XCharWidths.c,v 10.4 86/02/01 15:29:46 tony Rel $ */
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
	req->param.s[2] = len;
	Data (dpy, chars, len);
	if (!_XReply(dpy, &rep))
	    return(NULL);

	_XReadPad (dpy, (char *)widths, rep.param.l[0]);
	return(rep.param.l[0] >> 1);
}

