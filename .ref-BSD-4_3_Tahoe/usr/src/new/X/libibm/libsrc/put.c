#ifndef lint
static char *rcsid_put_c = "$Header: put.c,v 10.1 86/11/19 10:43:52 jg Exp $";
#endif	lint
/* Copyright 1985 Massachusetts Institute of Technology */

/* put.c - Perform a raster operation with a source bitmap
 *
 *      PixmapPut       Puts a pixmap up on the screen
 *      PixmapBitsPut   Puts masked region of pixmap up on screen
 *      BitmapBitsPut   Puts masked region of bitmap up on screen
 *
 *  	Author:
 *		Scott Bates
 *		Brown University
 *		IRIS, Box 1946
 *      	Providence, RI 02912
 *
 *
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 */

#include "private.h"
#include "bitblt.h"

/*
 * Copy pixmap to screen
 */

PixmapPut (src, srcx, srcy, width, height, dstx, dsty, clips, clipcount,
           func, zmask)
        PIXMAP *src;
	register width, height; 
        int srcx, srcy;
	register dstx, dsty;
	int clipcount, zmask;
	register func;
        CLIP *clips;
{
	register Blt_Rectangle *dest = &DstRect;

#ifdef TRACE_X
	fprintf(stderr, "In PixmapPut\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * There better be at least one plane
	 */

        if ((zmask & 1) == 0)
		return;

	/*
	 * If pixmap needs to be inverted before being displayed
	 * remap funtion to reflect this change.
	 */

        func = SSMap[func | (src->kind & InvertFlag)];

	/*
	 * Fill in destination rectangle
	 */

	FillInRect(dstx, dsty, width, height, dest);

	/*
	 * If pixmap has no associated bitmap determine which constant
	 * tile to use and tile the destination. Otherwise, copy bitmap 
	 * to screen destination.
	 */

        if (PTYPE(src) == ConstantPixmap) {

		/*
		 * Tile screen destination
		 */

		CopyBits ((u_short *) src->data, NIL, NIL, NILRECT, 
			  (u_short *) pbm.data, pbm.width, pbm.height, dest,
			  NILMASK, NIL, NIL, MAKE_TILE_RULE(func),
			  clipcount, clips);
        } else	{
		register Blt_Rectangle *source = &SrcRect;
        	register BITMAP *bm = (BITMAP *) src->data;

		/*
		 * Fill in source rectangle
		 */

		FillInRect(srcx, srcy, width, height, source);

		/*
		 * Copy bitmap to screen
		 */

		CopyBits ((u_short *)bm->data, bm->width, bm->height, source, 
			  (u_short *)pbm.data, pbm.width, pbm.height, dest,
			  NILMASK, NIL, NIL, func, clipcount, clips);
	}
}

/*
 * Copy pixmap to screen using a mask
 */

/*ARGSUSED*/
PixmapBitsPut (width, height, format, data, xymask, dstx, dsty,
               clips, clipcount, func, zmask)
        char *data;
        int width, height, format, dstx, dsty, clipcount, zmask;
        BITMAP *xymask;
        CLIP *clips;
        int func;
{
#ifdef TRACE_X
	fprintf(stderr, "In PixmapBitsPut\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Since this hardware has only one bit plane PixmapBitsPut
	 * and BitmapBitsPut are the same.
	 */

        BitmapBitsPut (width, height, data, 1, 0, xymask, dstx, dsty,
                       clips, clipcount, func, zmask);
}

/*
 * Copy bitmap to screen using a mask
 */

BitmapBitsPut (width, height, data, fore, back, xymask, dstx, dsty,
               clips, clipcount, func, zmask)
        char *data;
        register width, height, dstx, dsty;
	int fore, back, clipcount, zmask;
        BITMAP *xymask;
        CLIP *clips;
        register func;
{
	register Blt_Rectangle *source = &SrcRect;
	register Blt_Rectangle *dest = &DstRect;
	int new_width, new_height;
	u_short *clipmask;

#ifdef TRACE_X
	fprintf(stderr, "In BitmapBitsPut\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * There better be at least one plane
	 */

        if ((zmask & 1) == 0)
		return;

	/*
	 * Change function to reflect desired foreground and
	 * background colors
	 */

        if (fore & 1)
		func += 0x20;
        if (back & 1)
		func += 0x10;
        func = FBMap[func];

	/*
	 * Reverse bits in each short of the image data. Image data 
	 * recieved from the client is in VAX bit order and needs to
	 * be reversed for this hardware.
	 */

	ReverseShortBits((u_short *) data, BitmapSize(width, height) >> 1); 

	/*
	 * Clip xymask and destnation rectangle to minimum size
	 */

	if(xymask) {
		new_width = MIN (xymask->width, width);
		new_height = MIN (xymask->height, height);
		clipmask = (u_short *) xymask->data;
	} else	{
		new_width = width;
		new_height = height;
		clipmask = NILMASK;
	}

	/*
	 * Fill in source and destination rectangles
	 */

	FillInRect(0, 0, width, height, source);
	FillInRect(dstx, dsty, new_width, new_height, dest);

	/*
	 * Copy bitmap to screen using a mask
	 */

        CopyBits ((u_short *) data, width, height, source,
                  (u_short *) pbm.data, pbm.width, pbm.height, dest,
		  clipmask, new_width, new_height, func, clipcount, clips);
}
