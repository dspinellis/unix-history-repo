#include <X/mit-copyright.h>

/* $Header: XCharBitmap.c,v 10.4 86/02/01 15:29:43 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Bitmap XCharBitmap (font, the_char)
    Font font;
    int the_char;  /* can't call it "char"; that's a C keyword */
{
    register Display *dpy;
    register XReq *req;
    XRep rep;

    GetReq (X_CharBitmap, 0);
    req->param.l[0] = font;
    req->param.s[2] = the_char;
    if (!_XReply (dpy, &rep))
    	return (NULL);
    return (rep.param.l[0]);
}
