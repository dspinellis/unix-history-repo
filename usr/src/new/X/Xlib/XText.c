#include <X/mit-copyright.h>

/* $Header: XText.c,v 10.5 86/04/22 15:30:59 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XText (w, x, y, str, len, font, foreground, background)
	Window w;
	int x, y, len;
	char *str;
	Font font;
	int foreground, background;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_Text, w);
	req->func = GXcopy;
	req->mask = ~0;  /* all planes */
	req->params0 = x;
	req->params1 = y;
	req->param.l[1] = font;
	req->paramu4 = foreground;
	req->paramu5 = background;
	req->params6 = len;
	req->param.b[14] = 0;  /* no char pad */
	req->param.b[15] = 0;  /* no space pad */
	Data(dpy, str, len);
}

