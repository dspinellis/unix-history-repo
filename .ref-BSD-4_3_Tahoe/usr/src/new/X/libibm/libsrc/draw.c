#ifndef lint
static char *rcsid_draw_c = "$Header: draw.c,v 10.1 86/11/19 10:41:27 jg Exp $";
#endif	lint
/* draw.c - Draw lines, curves, and polygons on the screen
 *
 *      DrawCurve       Draw a generalized line/polygon/combination
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

DrawCurve (verts, vertcount, xbase, ybase, srcpix, altpix, mode,
           bwidth, bheight, pat, patlen, patmul, clips, clipcount, func, zmask)
        Vertex *verts;
        int vertcount, xbase, ybase, srcpix, altpix, mode, bwidth, bheight;
        int pat, patlen, patmul, clipcount, zmask;
        int func;
        CLIP *clips;
{
	register NumberOfClips;
	register CLIP *ClipPointer;
	Vertex *newverts;
	int newvertcount, i;

#ifdef TRACE_X
	fprintf(stderr, "In DrawCurve\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Limit draw operation to one plane and
	 * ignore requests of less than two points
	 */

        if ((zmask & 1) == 0 || vertcount < 2)
            return;

	/*
	 * Convert vertex list to absolute straight lines
	 */

	if(PathListConverter(verts, vertcount, xbase, ybase, &newverts,
	   &newvertcount, DRAW_PATH_LIST) == NULL ) {
		DeviceError("DrawCurve failure in PathListConverter()\n");
		return;
	}

	/*
	 * Single pixel wide line ?
	 */

	if (bwidth == 1 && bheight == 1) {
		/*
		 * Loop thru vertex list
		 */

		for(i = 0; i < newvertcount; i += 2)  {
			ClipPointer = clips;
			NumberOfClips = clipcount;

			/*
			 * Draw the same line ounce for each
			 * clipping rectangle
			 */

			for (;;) {
				SinglePixelLine(&pbm, newverts[i].x,
					newverts[i].y, newverts[i + 1].x,
					newverts[i + 1].y, ClipPointer,
					func, mode, srcpix, altpix, pat,
					patlen, patmul);
				/*
				 * Any more clips ?
				 */

				if(--NumberOfClips <= 0) {
					/*
					 * No more clips so lets leave
					 */

					break;
				}

				/*
			 	* Point to next clip
			 	*/

				ClipPointer++;
			}
		}
	} else	{
		Blt_Rectangle dst_rect;
		Blt_Line line;
		register Blt_Line *Line = &line;
		register Blt_Rectangle *DstRect = &dst_rect;
		register Blt_Rectangle *ClipRect = &Line->blt.clp_rect;

		/*
		 * Zero line data structure
		 */

		bzero((char *) Line, sizeof(Blt_Line));

		/*
		 * Fill in destination bitmap
		 */

		BitimageToBitmap((u_short *)pbm.data, 0, 0, pbm.width,
				 pbm.height, &Line->blt.dst_bitmap);

		/*
		 * Set brush size and tile pattern(s)
		 */

		Line->BrushX = bwidth;
		Line->BrushY = bheight;
		Line->blt.tile_ptr = (Blt_Tile *) ConstantTiles[srcpix & 1];
		if(mode == DrawPatternedLine)
			Line->AlternateTile =
					(Blt_Tile *) ConstantTiles[altpix & 1];

		/*
		 * Fill in dash pattern
		 */

		if(mode != DrawSolidLine) {
			Line->Pattern = (u_short) pat;
			Line->PatternLength = (short) patlen;
			Line->PatternMultiplier = (short) patmul;
		}

		/*
		 * Fill in appropriate combination rule
		 */

		if(IS_RULE_TILE(func)) {
			/*
			 * Fill in rule directly
			 */

			Line->blt.comb_rule = func;
		} else	{
			/*
			 * Convert standard rule to tile rule
			 */

			Line->blt.comb_rule = MAKE_TILE_RULE(func);
		}

		/*
		 * Fill in draw mode
		 */

		Line->DrawMode = mode;

		/*
		 * Loop thru vertex list
		 */

		for (;newvertcount > 0; newvertcount -= 2) {

			/*
			 * Set start and stop endpoints
			 */

			Line->StartX = newverts->x;
			Line->StartY = (newverts++)->y;
			Line->StopX = newverts->x;
			Line->StopY = (newverts++)->y;

			/*
			 * Compute and set destination rectangle
			 */

			DstRect->origin_y = MIN(Line->StopY, Line->StartY);
			DstRect->origin_x = MIN(Line->StopX, Line->StartX);
			DstRect->corner_y =
				MAX(Line->StopY, Line->StartY) + Line->BrushY;
			DstRect->corner_x =
				MAX(Line->StopX, Line->StartX) + Line->BrushX;
			Line->blt.dst_rect = *DstRect;

			/*
			 * Setup for clips
			 */

			NumberOfClips = clipcount;
			ClipPointer = clips;

			/*
			 * Draw same line ounce for each clip specified
			 */

			for(;;) {
				/*
				 * Convert X clip to clipping rectangle
				 */

				ClipToRect(ClipPointer, ClipRect);

				/*
				 * If destination rectangle is inside the
				 * clipping rectangle then turn off clipping
				 * else turn it on.
				 */

				if(InsideBounds(DstRect, ClipRect)) {
					Line->blt.blt_flags &= ~BLT_CLIPON;
				} else {
					Line->blt.blt_flags |= BLT_CLIPON;
				}

				/*
				 * Call multi pixel line drawing routine
				 */

				MultiPixelLine(Line);

				/*
				 * Any more clips ?
				 */

				if(--NumberOfClips <= 0) {
					/*
					 * No more clips so lets leave
					 */

					break;
				}

				/*
				 * Point to next clip
				 */

				ClipPointer++;
			}
		}
	}

	/*
	 * Free space used by new vertex list
	 */

	free((caddr_t)newverts);
}
