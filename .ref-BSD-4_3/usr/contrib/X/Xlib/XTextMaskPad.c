#include <X/mit-copyright.h>

/* $Header: XTextMaskPad.c,v 10.4 86/02/01 15:40:50 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XTextMaskPad (w, x, y, str, len, font, 
   charpad, spacepad, source, func, planes)
	Window w;
	int x, y, len;
	char *str;
	Font font;
	int source;
	int func, planes, charpad, spacepad;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_TextMask, w);
	req->func = func;
	req->mask = planes;
	req->param.s[0] = x;
	req->param.s[1] = y;
	req->param.l[1] = font;
	req->param.u[4] = source;
	req->param.s[6] = len;
	req->param.b[14] = charpad;
	req->param.b[15] = spacepad;
	Data(dpy, str, len);
}

