#ifndef lint
static char *rcsid_tile_c = "$Header: tile.c,v 10.1 86/11/19 10:44:34 jg Exp $";
#endif	lint
/* Copyright 1985 Massachusetts Institute of Technology */

/* tile.c - Perform a raster operation involving a pattern
 *
 *      TileFill        Patterns a portion of the screen
 *      DrawFilled      Draw a filled generalized line/polygon/combination
 *	AlignTile	Aligns tile to a given coordinates
 *
 *  	Changes and modifications by:
 *
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
 * Tile area of screen using a mask
 */

/*ARGSUSED*/
TileFill (tile, xoff, yoff, xymask, dstx, dsty, width, height,
          clips, clipcount, func, zmask)
        register PIXMAP *tile;
        register BITMAP *xymask;
	register width, height;
        int xoff, yoff, dstx, dsty, zmask;
        register func;
        CLIP *clips;
{
	u_short *clipmask = NILMASK;
        register BITMAP *bm = (BITMAP *) tile->data;
	register u_short *tilepattern;
	u_short newtilepattern[TILE_SIZE];

#ifdef TRACE_X
	fprintf(stderr, "In TileFill\n");
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

       	func = SSMap[func | (tile->kind & InvertFlag)];

	/*
         * Get tile. If pixmap has no associated bitmap then pixmap
         * struct contains pointer to constant tile. Otherwise, it
         * contains pointer to bitmap which points to the tile.
         */

        if (PTYPE(tile) == ConstantPixmap) {
		tilepattern = (u_short *) tile->data;
        } else	{
		tilepattern = (u_short *) bm->data;

		/*
		 * Align tile to offset supplied
		 */

		if ((xoff | yoff) & 0x0F) {
			AlignTile(tilepattern, newtilepattern, xoff, yoff);
			tilepattern = newtilepattern;
		}
	}

	/*
	 * Clip xymask and destnation rectangle to minimum size
	 */

	if(xymask) {
		width = MIN (xymask->width, width);
		height = MIN (xymask->height, height);
		clipmask = (u_short *) xymask->data;
	}

	/*
         * Fill in destination rectangle
         */

	FillInRect(dstx, dsty, width, height, &DstRect);

	/*
         * Tile area of screen using a mask
         */

	CopyBits (tilepattern, NIL, NIL, NILRECT, 
		  (u_short *) pbm.data, pbm.width, pbm.height, &DstRect,
		  clipmask, width, height, MAKE_TILE_RULE(func),
		  clipcount, clips);
}

/*
 * Draw a filled generalized line/polygon/combination
 */

/*ARGSUSED*/
DrawFilled (verts, vertcount, xbase, ybase, srcpix, tile, xoff, yoff,
            clips, clipcount, func, zmask)
        Vertex *verts;
        register PIXMAP *tile;
        int vertcount, xbase, ybase, srcpix, xoff, yoff, clipcount, zmask;
        int func;
        CLIP *clips;
{
	BITMAP *xymask;
	Vertex *newverts;
	int newvertcount;

#ifdef TRACE_X
	fprintf(stderr, "In DrawFilled\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Limit draw operation to one plane
	 */

        if ((zmask & 1) == 0 || vertcount < 2)
		return;

	/*
	 * Convert path list to absolute line segments
	 */

	if(PathListConverter(verts, vertcount, xbase, ybase, &newverts,
	   &newvertcount, FILL_PATH_LIST) == NULL) {
		DeviceError("DrawCurve failure in PathListConverter()\n");
		return;
	}

	/*
	 * Make mask for polygon fill
	 */

	if((xymask = MakeMask(newverts, newvertcount)) == NULL) {
		DeviceError("DrawFilled failure in MakeMask()\n");
		return;
	}

	/*
	 * Fill the polygon
	 */

	if(tile) {
		/*
		 * Tile fill the polygon using the mask
		 */

		TileFill(tile, xoff, yoff, xymask, 0, 0, xymask->width,
			 xymask->height, clips, clipcount, func, zmask);
	} else	{
		/*
		 * Pix fill the polygon using the mask
		 */

		PixFill(srcpix, xymask, 0, 0, xymask->width, xymask->height,
			clips, clipcount, func, zmask);
	}

	/*
	 * Free mask BITMAP and space used by converted vertex list
	 */

	FreeBitmap(xymask);
	free((caddr_t)newverts);
}

/*
 * Align tile to offset provided.
 * Note: tile is in IBM bit order not VAX
 */

static
AlignTile(src, dst, xoff, yoff)
	register u_short *src, *dst;
	register int xoff, yoff;
{
	register int i;
	register int shift;
	u_short mask;

#ifdef TRACE_X
	fprintf(stderr, "In Align_Tile\n");
	fflush(stderr);
#endif TRACE_X

	xoff &= 0x0F;
	yoff = (TILE_HEIGHT - (yoff & 0x0F)) & 0x0F;
	shift = (TILE_WIDTH - xoff) & 0x0F;
	mask = (1 << xoff) - 1;

	for (i = 0; i < TILE_HEIGHT; i++) {
		dst[i] = (src[yoff] >> xoff) | ((src[yoff] & mask) << shift);
		yoff++;
		yoff &= 0x0F;
	}
}
