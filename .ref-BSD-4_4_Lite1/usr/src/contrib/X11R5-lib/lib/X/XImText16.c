/* $XConsortium: XImText16.c,v 11.19 91/07/12 16:28:47 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1986	*/

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#define NEED_REPLIES
#include "Xlibint.h"

#if NeedFunctionPrototypes
XDrawImageString16(
    register Display *dpy,
    Drawable d,
    GC gc,
    int x,
    int y,
    _Xconst XChar2b *string,
    int length)
#else
XDrawImageString16(dpy, d, gc, x, y, string, length)
    register Display *dpy;
    Drawable d;
    GC gc;
    int x, y;
    XChar2b *string;
    int length;
#endif
{   
    register xImageText16Req *req;
    xQueryTextExtentsReq *qreq;
    xQueryTextExtentsReply rep;
    XChar2b *CharacterOffset = (XChar2b *)string;
    int FirstTimeThrough = True;
    int lastX = 0;
    char *buf;
    unsigned char *ptr;
    XChar2b *str;
    int i;

    LockDisplay(dpy);
    FlushGC(dpy, gc);
    if (length > 255 &&
	! (buf = _XAllocScratch (dpy, (unsigned long) 512))) {
	UnlockDisplay(dpy);
	SyncHandle();
	return;
    }

    while (length > 0) 
    {
	int Unit, Datalength;

	if (length > 255) Unit = 255;
	else Unit = length;

   	if (FirstTimeThrough)
	{
	    FirstTimeThrough = False;
        }
	else
	{
	    GetReq(QueryTextExtents, qreq);
	    qreq->fid = gc->gid;
	    qreq->length += (510 + 3)>>2;
	    qreq->oddLength = 1;
	    str = CharacterOffset - 255;
	    for (ptr = (unsigned char *)buf, i = 255; --i >= 0; str++) {
		*ptr++ = str->byte1;
		*ptr++ = str->byte2;
	    }
	    Data (dpy, buf, 510);
	    if (!_XReply (dpy, (xReply *)&rep, 0, xTrue))
		break;

	    x = lastX + cvtINT32toInt (rep.overallWidth);
	}

        GetReq (ImageText16, req);
        req->length += ((Unit << 1) + 3) >> 2;
        req->nChars = Unit;
        req->drawable = d;
        req->gc = gc->gid;
        req->y = y;

	lastX = req->x = x;
	Datalength = Unit << 1;
        Data (dpy, (char *)CharacterOffset, (long)Datalength);
        CharacterOffset += Unit;
	length -= Unit;
    }
    UnlockDisplay(dpy);
    SyncHandle();
}

