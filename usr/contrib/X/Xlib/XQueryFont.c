#include <X/mit-copyright.h>

/* $Header: XQueryFont.c,v 10.4 86/02/01 15:38:59 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Status XQueryFont (font, info)
	Font font;
	register FontInfo *info;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_QueryFont, 0);
	req->param.l[0] = font;
	if (!_XReply(dpy, &rep))
	    return (0);
	info->id = font;
	info->height = rep.param.s[0];
	info->width = rep.param.s[1];
	info->firstchar = (unsigned char) rep.param.s[2];
	info->lastchar = (unsigned char) rep.param.s[3];
	info->baseline = rep.param.s[4];
	info->fixedwidth = rep.param.s[5];
	info->widths = NULL;
	return (1);
}

