#include <X/mit-copyright.h>

/* $Header: XQueryFont.c,v 10.5 86/04/22 15:24:44 jg Rel $ */
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
	info->height = rep.params0;
	info->width = rep.params1;
	info->firstchar = (unsigned char) rep.params2;
	info->lastchar = (unsigned char) rep.params3;
	info->baseline = rep.params4;
	info->fixedwidth = rep.params5;
	info->widths = NULL;
	return (1);
}

