#include <X/mit-copyright.h>

/* $Header: XTextPad.c,v 10.4 86/02/01 15:40:53 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XTextPad (w, x, y, str, len, font,
   charpad, spacepad, foreground, background, func, planes)
	Window w;
	int func, planes;
	int charpad, spacepad;
	int x, y, len;
	char *str;
	Font font;
	int foreground, background;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_Text, w);
	req->func = func;
	req->mask = planes;
	req->param.s[0] = x;
	req->param.s[1] = y;
	req->param.l[1] = font;
	req->param.u[4] = foreground;
	req->param.u[5] = background;
	req->param.s[6] = len;
	req->param.b[14] = charpad;
	req->param.b[15] = spacepad;
	Data(dpy, str, len);
}

