/* $XConsortium: XText16.c,v 11.21 91/01/06 11:48:27 rws Exp $ */
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

#include "Xlibint.h"

#if NeedFunctionPrototypes
XDrawString16(
    register Display *dpy,
    Drawable d,
    GC gc,
    int x,
    int y,
    _Xconst XChar2b *string,
    int length)
#else
XDrawString16(dpy, d, gc, x, y, string, length)
    register Display *dpy;
    Drawable d;
    GC gc;
    int x, y;
    XChar2b *string;
    int length;
#endif
{   
    int Datalength = 0;
    register xPolyText16Req *req;

    if (length <= 0)
       return;

    LockDisplay(dpy);
    FlushGC(dpy, gc);
    GetReq (PolyText16, req);
    req->drawable = d;
    req->gc = gc->gid;
    req->x = x;
    req->y = y;


    Datalength += SIZEOF(xTextElt) * ((length + 253) / 254) + (length << 1);


    req->length += (Datalength + 3)>>2;  /* convert to number of 32-bit words */


    /* 
     * If the entire request does not fit into the remaining space in the
     * buffer, flush the buffer first.   If the request does fit into the
     * empty buffer, then we won't have to flush it at the end to keep
     * the buffer 32-bit aligned. 
     */

    if (dpy->bufptr + Datalength > dpy->bufmax)
    	_XFlush (dpy);

    {
	int nbytes;
	int PartialNChars = length;
        register xTextElt *elt;
 	XChar2b *CharacterOffset = (XChar2b *)string;

	while(PartialNChars > 254)
        {
 	    nbytes = 254 * 2 + SIZEOF(xTextElt);
	    BufAlloc (xTextElt *, elt, nbytes);
	    elt->delta = 0;
	    elt->len = 254;
#ifdef MUSTCOPY
	    {
		register int i;
		register unsigned char *cp;
		for (i = 0, cp = ((unsigned char *)elt) + 2; i < 254; i++) {
		    *cp++ = CharacterOffset[i].byte1;
		    *cp++ = CharacterOffset[i].byte2;
		}
	    }
#else
            bcopy ((char *)CharacterOffset, (char *) (elt + 1), 254 * 2);
#endif
	    PartialNChars = PartialNChars - 254;
	    CharacterOffset += 254;
	}
	    
        if (PartialNChars)
        {
	    nbytes = PartialNChars * 2  + SIZEOF(xTextElt);
	    BufAlloc (xTextElt *, elt, nbytes); 
	    elt->delta = 0;
	    elt->len = PartialNChars;
#ifdef MUSTCOPY
	    {
		register int i;
		register unsigned char *cp;
		for (i = 0, cp = ((unsigned char *)elt) + 2; i < PartialNChars;
		     i++) {
		    *cp++ = CharacterOffset[i].byte1;
		    *cp++ = CharacterOffset[i].byte2;
		}
	    }
#else
            bcopy ((char *)CharacterOffset, (char *) (elt + 1), PartialNChars * 2);
#endif
	 }
    }

    /* Pad request out to a 32-bit boundary */

    if (Datalength &= 3) {
	char *pad;
	/* 
	 * BufAlloc is a macro that uses its last argument more than
	 * once, otherwise I'd write "BufAlloc (char *, pad, 4-length)" 
	 */
	length = 4 - Datalength;
	BufAlloc (char *, pad, length);
	/* 
	 * if there are 3 bytes of padding, the first byte MUST be 0
	 * so the pad bytes aren't mistaken for a final xTextElt 
	 */
	*pad = 0;
        }

    /* 
     * If the buffer pointer is not now pointing to a 32-bit boundary,
     * we must flush the buffer so that it does point to a 32-bit boundary
     * at the end of this routine. 
     */

    if ((dpy->bufptr - dpy->buffer) & 3)
       _XFlush (dpy);
    UnlockDisplay(dpy);
    SyncHandle();
    return;
}

