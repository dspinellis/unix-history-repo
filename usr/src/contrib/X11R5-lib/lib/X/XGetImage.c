/* $XConsortium: XGetImage.c,v 11.27 91/01/26 14:08:52 rws Exp $ */
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

#define ROUNDUP(nbytes, pad) (((((nbytes) - 1) + (pad)) / (pad)) * (pad))

static unsigned int Ones(mask)                /* HACKMEM 169 */
    unsigned long mask;
{
    register unsigned long y;

    y = (mask >> 1) &033333333333;
    y = mask - y - ((y >>1) & 033333333333);
    return ((unsigned int) (((y + (y >> 3)) & 030707070707) % 077));
}

XImage *XGetImage (dpy, d, x, y, width, height, plane_mask, format)
     register Display *dpy;
     Drawable d;
     int x, y;
     unsigned int width, height;
     unsigned long plane_mask;
     int format;	/* either XYPixmap or ZPixmap */
{
	xGetImageReply rep;
	register xGetImageReq *req;
	char *data;
	long nbytes;
	XImage *image;
	LockDisplay(dpy);
	GetReq (GetImage, req);
	/*
	 * first set up the standard stuff in the request
	 */
	req->drawable = d;
	req->x = x;
	req->y = y;
	req->width = width;
	req->height = height;
	req->planeMask = plane_mask;
	req->format = format;
	
	if (_XReply (dpy, (xReply *) &rep, 0, xFalse) == 0) {
		UnlockDisplay(dpy);
		SyncHandle();
		return (XImage *)NULL;
	}
		
	nbytes = (long)rep.length << 2;
	data = (char *) Xmalloc((unsigned) nbytes);
	if (! data) {
	    _XEatData(dpy, (unsigned long) nbytes);
	    UnlockDisplay(dpy);
	    SyncHandle();
	    return (XImage *) NULL;
	}
        _XReadPad (dpy, data, nbytes);
        if (format == XYPixmap)
	   image = XCreateImage(dpy, _XVIDtoVisual(dpy, rep.visual),
		  Ones (plane_mask &
			(((unsigned long)0xFFFFFFFF) >> (32 - rep.depth))),
		  format, 0, data, width, height, dpy->bitmap_pad, 0);
	else /* format == ZPixmap */
           image = XCreateImage (dpy, _XVIDtoVisual(dpy, rep.visual),
		 rep.depth, ZPixmap, 0, data, width, height,
		  _XGetScanlinePad(dpy, (int) rep.depth), 0);

	UnlockDisplay(dpy);
	SyncHandle();
	return (image);
}

XImage *XGetSubImage(dpy, d, x, y, width, height, plane_mask, format,
		     dest_image, dest_x, dest_y)
     register Display *dpy;
     Drawable d;
     int x, y;
     unsigned int width, height;
     unsigned long plane_mask;
     int format;	/* either XYFormat or ZFormat */
     XImage *dest_image;
     int dest_x, dest_y;
{
	XImage *temp_image;
	temp_image = XGetImage(dpy, d, x, y, width, height, 
				plane_mask, format);
	if (!temp_image)
	    return (XImage *)NULL;
	_XSetImage(temp_image, dest_image, dest_x, dest_y);
	XDestroyImage(temp_image);
	return (dest_image);
}	
