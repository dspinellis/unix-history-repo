/* $XConsortium: XCrPFBData.c,v 1.6 91/02/01 16:33:55 gildea Exp $ */
/* Copyright    Massachusetts Institute of Technology    1987	*/

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

#include "Xlib.h"
#include <stdio.h>

/*
 * XCreatePixmapFromBitmapData: Routine to make a pixmap from user supplied bitmap data.
 *             D is any drawable on the same screen that the pixmap will be used in.
 *             Data is a pointer to the bit data, and 
 *             width & height give the size in bits of the pixmap.
 *             Fg and Bg are the pixel values to use for the two colors.
 *             Depth is the depth of the pixmap to create.
 *
 * The following format is assumed for data:
 *
 *    format=XYPixmap
 *    bit_order=LSBFirst
 *    byte_order=LSBFirst
 *    padding=8
 *    bitmap_unit=8
 *    xoffset=0
 *    no extra bytes per line
 */  
Pixmap XCreatePixmapFromBitmapData(display,d,data,width,height,fg,bg,depth)
    Display *display;
    Drawable d;
    char *data;
    unsigned int width, height;
    unsigned long fg, bg;
    unsigned int depth;
{
    XImage ximage;
    GC gc;
    XGCValues gcv;
    Pixmap pix;

    pix = XCreatePixmap(display, d, width, height, depth);
    gcv.foreground = fg;
    gcv.background = bg;
    if (! (gc = XCreateGC(display, pix, GCForeground|GCBackground, &gcv)))
	return (Pixmap) NULL;
    ximage.height = height;
    ximage.width = width;
    ximage.depth = 1;
    ximage.xoffset = 0;
    ximage.format = XYBitmap;
    ximage.data = data;
    ximage.byte_order = LSBFirst;
    ximage.bitmap_unit = 8;
    ximage.bitmap_bit_order = LSBFirst;
    ximage.bitmap_pad = 8;
    ximage.bytes_per_line = (width+7)/8;

    XPutImage(display, pix, gc, &ximage, 0, 0, 0, 0, width, height);
    XFreeGC(display, gc);
    return(pix);
}
