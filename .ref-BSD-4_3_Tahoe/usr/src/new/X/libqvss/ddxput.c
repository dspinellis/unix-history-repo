/* Copyright 1985, Massachusetts Institute of Technology */

/* put.c	Perform a raster operation with a source bitmap
 *
 *	PixmapPut	Puts a pixmap up on the screen
 *	PixmapBitsPut	Puts a pixmap up on the screen
 *	BitmapBitsPut	Puts a pixmap up on the screen
 *
 * Modification History:
 *
 *   Carver 8601.13 Added extern statement for SSmap
 *
 *   Carver 8601.06 Added line to PixmapPut to adjust the func to compensate
 *		    for the reverse flag being set in the pixmap descriptor.
 *
 *   Carver 8510.10 Put in support for source bitmap/sub-bitmap mask
 *		    copy areas.
 *
 *   Carver 8510.09 In PixmapBitsPut fixed the table lookup on foreground
 *		    and background.
 */

#include "ddxqvss.h"
#include "vstagbl.h"

extern BITMAP pbm;

PixmapPut (src, srcx, srcy, width, height, dstx, dsty, clips, clipcount,
	   func, zmask)
	register PIXMAP *src;
	int srcx, srcy, width, height, dstx, dsty, clipcount, zmask;
	register int func;
	CLIP *clips;
{
	BITMAP *bm = (BITMAP *) src->data;
	extern char SSMap[];

	if (!(zmask & 1))  return;

	/* ADJUST MAP FOR REVERSE PIXMAP */

	func = SSMap[ func | (src->kind & InvertFlag)];

	copyrmsk( VSTA$K_SRC_BITMAP, (short *)bm->data, bm->width, bm->height,
		srcx, srcy, width, height, 
		(short *)pbm.data, pbm.width, pbm.height,
		dstx, dsty, func, clipcount, clips);
	return;
}


/*ARGSUSED*/
PixmapBitsPut (width, height, format, data, xymask, dstx, dsty,
	       clips, clipcount, func, zmask)
	char *data;
	int width, height, format, dstx, dsty, clipcount, zmask;
	BITMAP *xymask;
	CLIP *clips;
	int func;
{
	BitmapBitsPut (width, height, data, 1, 0, xymask, dstx, dsty,
		       clips, clipcount, func, zmask);
}


BitmapBitsPut (width, height, data, fore, back, xymask, dstx, dsty,
	       clips, clipcount, func, zmask)
	char *data;
	int width, height, fore, back, dstx, dsty, clipcount, zmask;
	register BITMAP *xymask;
	CLIP *clips;
	register int func;
{
	int mask_x = pbm.width, mask_y = pbm.height;
	extern char FBMap[];

	if (!(zmask & 1))  return;

	/* USE FBMap TABLE TO CONVERT FORE/BACK INTO A SINGLE SOURCE
	   MAPPING FUNCTION */

	if (fore & 1) func += 0x20;
	if (back & 1) func += 0x10;

	func = FBMap[func];

	if (xymask == NULL) 
		{
		copyrmsk (VSTA$K_SRC_BITMAP, (short *) data, width, height,
			  0, 0, mask_x, mask_y, (short *) pbm.data, pbm.width,
			  pbm.height, dstx, dsty, func, clipcount, clips);
		}
	else
		{
			
		copybmsk (
			VSTA$K_SRC_BITMAP, (short *)data, width, height, 
			0, 0, 
			(short *) xymask->data, xymask->width, xymask->height,
			0, 0, width, height,
			(short *)pbm.data, pbm.width, pbm.height,
			dstx, dsty, func, clipcount, clips);
		};



	return;
}
