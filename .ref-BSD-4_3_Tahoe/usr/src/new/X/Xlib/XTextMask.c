#include <X/mit-copyright.h>

/* $Header: XTextMask.c,v 10.5 86/04/22 15:32:09 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XTextMask (w, x, y, str, len, font, source)
	Window w;
	int x, y, len;
	char *str;
	Font font;
	int source;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_TextMask, w);
	req->func = GXcopy;
	req->mask = ~0;  /* all planes */
	req->params0 = x;
	req->params1 = y;
	req->param.l[1] = font;
	req->paramu4 = source;
	req->params6 = len;
	req->param.b[14] = 0;  /* no character pad */
	req->param.b[15] = 0;  /* no space pad */
	Data(dpy, str, len);
}

