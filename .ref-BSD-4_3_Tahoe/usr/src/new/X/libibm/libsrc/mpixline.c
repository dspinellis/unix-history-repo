#ifndef lint
static char *rcsid_mpixline_c = "$Header: mpixline.c,v 10.1 86/11/19 10:43:05 jg Exp $";
#endif	lint
/* mpixline.c - multi pixel wide line drawing routines
 *
 *	MultiPixelLine	preprocessing routine for multi pixel wide line drawing
 *	SolidLine	draws solid lines
 *	DashedLine	draws dashed lines
 *	PatternedLine	draws patterned lines
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

MultiPixelLine(Line)
	register Blt_Line *Line;
{
	register Blt *blt = &Line->blt;

#ifdef TRACE_X
	fprintf(stderr, "In MultiPixelLine\n");
	fflush(stderr);
#endif TRACE_X

#ifdef AED
	blt->blt_flags |= BLT_ECHO;
#endif AED

	/*
	 * If the line is a solid horizontal or vertical tile
	 * the destination rectangle.
	 */

	if (Line->DrawMode == DrawSolidLine && (Line->StopX == Line->StartX ||
	    Line->StopY == Line->StartY)) {
		/*
		 * Line is a solid horizontal or vertical so handle
		 * this as a tile fill of destination rectangle
		 */

		bitblt(blt);
	} else	{			/* Line is a diagonal */
		register short StartX = Line->StartX;
		register short StartY = Line->StartY;
		register short StopX = Line->StopX;
		register short StopY = Line->StopY;
		int BrushX = Line->BrushX;
		int BrushY = Line->BrushY;
		short *Top, *Left, *Bottom, *Right;
		int DeltaX, DeltaY, SignY;

		/*
		 * Always draw line from left to right
		 */

		if (StopX < StartX) {
			short Temp;

			Temp = StopX; StopX = StartX; StartX = Temp;
			Temp = StopY; StopY = StartY; StartY = Temp;
		}

		/*
		 * Initialize pointers to destination rectangle
		 */

		Top = &blt->dst_rect.origin_y;
		Left = &blt->dst_rect.origin_x;
		Bottom = &blt->dst_rect.corner_y;
		Right = &blt->dst_rect.corner_x;

		/*
		 * Set destination rectangle to be the first
		 * the point in line
		 */

		*Left = StartX;
		*Top = StartY;
		*Right = StartX + Line->BrushX;
		*Bottom = StartY + Line->BrushY;

		/*
		 * Compute X and Y deltas
		 */

		DeltaX = StopX - StartX;
		DeltaY = StopY - StartY;

		/*
		 * Determine vertical drawing direction
		 * and adjust values if required
		 */

		if(DeltaY < 0) {
			DeltaY = -DeltaY;	/* bottom to top */
			SignY = -1;
			BrushY = -BrushY;
			Top = &blt->dst_rect.corner_y;
			Bottom = &blt->dst_rect.origin_y;
		} else	{
			SignY = 1;		/* top to bottom */
		}

		/*
		 * Call appropriate line drawing routine
		 */

		switch (Line->DrawMode) {
		case (DrawSolidLine):
			SolidLine(Line, Top, Left, Bottom, Right,
				  DeltaX, DeltaY, BrushX, BrushY, SignY);
			break;
		case (DrawDashedLine):
			DashedLine(Line, Top, Left, Bottom, Right,
				   DeltaX, DeltaY, BrushX, BrushY, SignY);
			break;
		case (DrawPatternedLine):
			PatternedLine(Line, Top, Left, Bottom, Right,
				      DeltaX, DeltaY, BrushX, BrushY, SignY);
		}
	}
}

/*
 * Draw a solid line
 */

static
SolidLine(Line, Top, Left, Bottom, Right, DeltaX, DeltaY,
		BrushX, BrushY, SignY)
	Blt_Line *Line;
	register short *Top, *Left;
	register short *Bottom, *Right;
	int DeltaX, DeltaY;
	int BrushX, BrushY;
	int SignY;
{
	Blt *blt = &Line->blt;
	register BitsLeftToDraw, Sentinel;

#ifdef TRACE_X
	fprintf(stderr, "In SolidLine\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * draw solid line
	 */
				/* line is more horizontal */
	if(DeltaX > DeltaY) {	
		Sentinel = DeltaX >> 1;
		BitsLeftToDraw = DeltaX;

		while (BitsLeftToDraw--) {
			if ((Sentinel -= DeltaY) < 0) {
				bitblt(blt);
				*Left = *Right - BrushX + 1;
				*Top += SignY;
				*Bottom += SignY;
				Sentinel += DeltaX;
			}
			(*Right)++;
		}
		bitblt(blt);
	} else	{		/* line is more vertical */
		Sentinel = DeltaY >> 1;
		BitsLeftToDraw = DeltaY;

		while (BitsLeftToDraw--) {
			if ((Sentinel -= DeltaX) < 0) {
				bitblt(blt);
				*Top = *Bottom - BrushY + SignY;
				(*Left)++;
				(*Right)++;
				Sentinel += DeltaY;
			}
			*Bottom += SignY;
		}
		bitblt(blt);
	}
}

/*
 * Draw a dashed line
 */

static
DashedLine(Line, Top, Left, Bottom, Right, DeltaX, DeltaY,
		BrushX, BrushY, SignY)
	Blt_Line *Line;
	register short *Top, *Left;
	register short *Bottom, *Right;
	int DeltaX, DeltaY;
	int BrushX, BrushY;
	int SignY;
{
	Blt *blt = &Line->blt;
	u_short PatternFirstBit = 1 << (Line->PatternLength - 1);
	u_short PatternBit = PatternFirstBit;
	u_short Pattern = Line->Pattern;
	short PatternMultiplier = Line->PatternMultiplier;
	register BitsLeftToDraw, Sentinel;

#ifdef TRACE_X
	fprintf(stderr, "In DashedLine\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Draw dashed line
	 */

				/* line is more horizontal */
	if(DeltaX > DeltaY) {
		Sentinel = DeltaX >> 1;
		BitsLeftToDraw = DeltaX;

		while (BitsLeftToDraw--) {
			if(Pattern & PatternBit)
				bitblt(blt);
			if(--PatternMultiplier == 0) {
				PatternMultiplier = Line->PatternMultiplier;
				if((PatternBit >>= 1) == 0)
					PatternBit = PatternFirstBit;
			}
			*Left = *Right - BrushX + 1;
			(*Right)++;
			if ((Sentinel -= DeltaY) < 0) {
				*Top += SignY;
				*Bottom += SignY;
				Sentinel += DeltaX;
			}
		}
		if(Pattern & PatternBit)
			bitblt(blt);
	} else	{		/* line is more vertical */
		Sentinel = DeltaY >> 1;
		BitsLeftToDraw = DeltaY;

		while (BitsLeftToDraw--) {
			if(Pattern & PatternBit)
				bitblt(blt);
			if(--PatternMultiplier == 0) {
				PatternMultiplier = Line->PatternMultiplier;
				if((PatternBit >>= 1) == 0)
					PatternBit = PatternFirstBit;
			}
			*Top = *Bottom - BrushY + SignY;
			*Bottom += SignY;
			if ((Sentinel -= DeltaX) < 0) {
				(*Left)++;
				(*Right)++;
				Sentinel += DeltaY;
			}
		}
		if(Pattern & PatternBit)
			bitblt(blt);
	}
}

/*
 * Draw a patterned line
 */

static
PatternedLine(Line, Top, Left, Bottom, Right, DeltaX, DeltaY,
		BrushX, BrushY, SignY)
	Blt_Line *Line;
	register short *Top, *Left;
	register short *Bottom, *Right;
	int DeltaX, DeltaY;
	int BrushX, BrushY;
	int SignY;
{
	Blt *blt = &Line->blt;
	u_short PatternFirstBit = 1 << (Line->PatternLength - 1);
	u_short PatternBit = PatternFirstBit;
	u_short Pattern = Line->Pattern;
	short PatternMultiplier = Line->PatternMultiplier;
	Blt_Tile *SourceTile = blt->tile_ptr;
	Blt_Tile *AltSourceTile = Line->AlternateTile;
	register BitsLeftToDraw, Sentinel;

#ifdef TRACE_X
	fprintf(stderr, "In PatternedLine\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Draw patterned line
	 */

				/* line is more horizontal */
	if(DeltaX > DeltaY) {
		Sentinel = DeltaX >> 1;
		BitsLeftToDraw = DeltaX;

		while (BitsLeftToDraw--) {
			if(Pattern & PatternBit)
				blt->tile_ptr = SourceTile;
			else
				blt->tile_ptr = AltSourceTile;
			bitblt(blt);

			if(--PatternMultiplier == 0) {
				PatternMultiplier = Line->PatternMultiplier;
				if((PatternBit >>= 1) == 0)
					PatternBit = PatternFirstBit;
			}

			*Left = *Right - BrushX + 1;
			(*Right)++;
			if ((Sentinel -= DeltaY) < 0) {
				*Top += SignY;
				*Bottom += SignY;
				Sentinel += DeltaX;
			}
		}
		if(Pattern & PatternBit)
			blt->tile_ptr = SourceTile;
		else
			blt->tile_ptr = AltSourceTile;
		bitblt(blt);
		blt->tile_ptr = SourceTile;
	} else	{		/* line is more vertical */
		Sentinel = DeltaY >> 1;
		BitsLeftToDraw = DeltaY;

		while (BitsLeftToDraw--) {
			if(Pattern & PatternBit)
				blt->tile_ptr = SourceTile;
			else
				blt->tile_ptr = AltSourceTile;
			bitblt(blt);

			if(--PatternMultiplier == 0) {
				PatternMultiplier = Line->PatternMultiplier;
				if((PatternBit >>= 1) == 0)
					PatternBit = PatternFirstBit;
			}

			*Top = *Bottom - BrushY + SignY;
			*Bottom += SignY;
			if ((Sentinel -= DeltaX) < 0) {
				(*Left)++;
				(*Right)++;
				Sentinel += DeltaY;
			}
		}
		if(Pattern & PatternBit)
			blt->tile_ptr = SourceTile;
		else
			blt->tile_ptr = AltSourceTile;
		bitblt(blt);
		blt->tile_ptr = SourceTile;
	}
}
