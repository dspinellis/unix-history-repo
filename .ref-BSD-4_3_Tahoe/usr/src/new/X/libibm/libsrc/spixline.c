#ifndef lint
static char *rcsid_spixline_c = "$Header: spixline.c,v 10.1 86/11/19 10:44:12 jg Exp $";
#endif	lint
/* spixline.c - single pixel wide line drawing routine
 *
 *	SinglePixelLine	draws solid, dashed and patterned lines
 *			that are only one pixel wide
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
#include "spixline.h"

/*
 * Draw single pixel wide line
 */

SinglePixelLine (BitMap, StartX, StartY, StopX, StopY, Clip, Rule, Mode,
		 SrcPix, AltPix, Pattern, PatternLength, PatternMultiplier)
	BITMAP *BitMap;
	int StartX, StartY;
	int StopX, StopY;
	CLIP *Clip;
	int Rule, Mode;
	int SrcPix, AltPix;
	u_short Pattern;
	int PatternLength;
	int PatternMultiplier;
{
	register u_short MaskBit, *Destination, Source;
	register BitsLeftToDraw, Sentinel, NumberOfBytes;
	int DeltaX, DeltaY, Temp, RepeatCount;
	u_short AltSource, PatternFirstBit, PatternBit;
	int NumberOfShorts;
#if (defined(AED) || defined(APA8) || defined(APA8C))
	struct Blt_Rectangle Rect;
#endif (AED || APA8 || APA8C)
#if (defined(APA8) || defined(APA8C))
	long DestinationIncrement;
#endif (APA8 || APA8C)
#ifdef AED
	int EchoLine = 1;

	/*
	 * If the destination is the screen and were not drawing a patterned
	 * line use the AED microcode to draw it. If the microcode is used 
	 * then the blt does not have to be echoed to the AED to make visible.
	 *
	 * NOTE: If we are drawing a dashed line and the multiplier
	 *       is equal to one then use the AED microcode. Otherwise,
	 *       do it in software and then echo it to the AED.
	 */

	if(BitMap == SCREEN_BITMAP && Mode != DrawPatternedLine) {

		if (Mode == DrawSolidLine) {
			ClipToRect(Clip, &Rect);
			aed_draw_line(StartX, StartY, StopX, StopY, Rule, 1,
				(SrcPix & 1), (u_short)0, 0, &Rect);
			EchoLine = 0;
		} else	if (PatternMultiplier < 2) {
			ClipToRect(Clip, &Rect);
			aed_draw_line(StartX, StartY, StopX, StopY, Rule, 1,
				(SrcPix & 1), Pattern, PatternLength, &Rect);
			EchoLine = 0;
		}
	}
#endif AED

#ifdef TRACE_X
        fprintf(stderr, "In SinglePixelLine\n");
        fflush(stderr);
#endif TRACE_X

	/*
	 * Clip line. If nothing to draw after clip
	 * return to caller.
	 */

	ClipLine(StartX, StartY, StopX, StopY, Clip);

	/*
	 * Always draw left to right
	 */

	if (StopX < StartX) {
	    Temp = StopX,  StopX = StartX,  StartX = Temp;
	    Temp = StopY,  StopY = StartY,  StartY = Temp;
	}

	/*
	 * Compute delta values and bitmap width in shorts
	 */

	DeltaX = StopX - StartX;
	DeltaY = StopY - StartY;

	NumberOfShorts = (BitMap->width + 15) / 16;

#if (defined(APA8) || defined(APA8C))
	if (DeltaY > 0) {	/* drawing top to bottom */
	    if (BitMap == SCREEN_BITMAP)
	    	NumberOfBytes = NumberOfShorts << 2;
	    else
		NumberOfBytes = NumberOfShorts << 1;
	} else {		/* drawing bottom to top */
	    if (BitMap == SCREEN_BITMAP)
	    	NumberOfBytes = -(NumberOfShorts << 2);
	    else
	    	NumberOfBytes = -(NumberOfShorts << 1);
	    DeltaY = -DeltaY;
	}
#else
	if (DeltaY > 0) {	/* drawing top to bottom */
	    NumberOfBytes = NumberOfShorts << 1;
	} else {		/* drawing bottom to top */
	    NumberOfBytes = -(NumberOfShorts << 1);
	    DeltaY = -DeltaY;
	}
#endif (APA8 || APA8C)

	/*
	 * Get source and alternate tiles
	 */

	Source = *ConstantTiles[SrcPix & 1];
	if(Mode == DrawPatternedLine)
		AltSource = *ConstantTiles[AltPix & 1];

	/*
	 * Compute starting destination address
	 */

#if (defined(APA8) || defined (APA8C))
	if (BitMap == SCREEN_BITMAP) {
	    Destination = (u_short *) BitMap->data +
			((StartX / 16 + StartY * NumberOfShorts) << 1);
	    DestinationIncrement = 4;
	} else {
	    Destination = (u_short *) BitMap->data +
			StartX / 16 + StartY * NumberOfShorts;
	    DestinationIncrement = 2;
	}
#else
	Destination = (u_short *) BitMap->data +
			StartX / 16 + StartY * NumberOfShorts;
#endif (APA8 || APA8C)

	/*
	 * set mask to select starting bit
	 */

	MaskBit = 0x8000 >> (StartX & 0x0F);

	/*
	 * If this is not a solid line setup pattern values
	 */

	if (Mode != DrawSolidLine) {
		RepeatCount = PatternMultiplier;
            	PatternFirstBit = 1 << (PatternLength - 1);
            	PatternBit = PatternFirstBit;
	}

#ifdef SOFTWARE_CURSOR
	/*
	 * Save cursor if blt is to screen
	 */

	if (BitMap == SCREEN_BITMAP) {
		if (save_cursor(MIN(StopX, StartX), MIN(StopY, StartY),
		     MAX(StopX, StartX) + 1, MAX(StopY, StartY) + 1) < 0)
		{
		        DeviceError(
			    "SinglePixelLine: ioctl QIOCHIDECUR failed.\n"
			);
		}
	}
#endif SOFTWARE_CURSOR

	/*
	 * draw the line
	 */

    	if (DeltaX > DeltaY) {	/* This is a horizontal line */
	    Sentinel = DeltaX >> 1;
            BitsLeftToDraw = DeltaX;

	    switch (Rule) {
	    case (GXor):
		    HORIZONTAL_LINE (
			{*Destination |= (Source & MaskBit);},
			{*Destination |= (AltSource & MaskBit);}
		    );
		    break;
	    case (GXcopy):
		    HORIZONTAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				(Source & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				(AltSource & MaskBit);}
		    );
		    break;
	    case (GXxor):
	            HORIZONTAL_LINE (
			{*Destination ^= (Source & MaskBit);},
			{*Destination ^= (AltSource & MaskBit);}
		    );
		    break;
	    case (GXset):
	    	    HORIZONTAL_LINE (
			{*Destination |= MaskBit;},
			{*Destination |= MaskBit;}
		    );
		    break;
	    case (GXclear):
	    	    HORIZONTAL_LINE (
			{*Destination &= ~MaskBit;},
			{*Destination &= ~MaskBit;}
		    );
		    break;
	    case (GXinvert):
	            HORIZONTAL_LINE (
			{*Destination ^= MaskBit;},
			{*Destination ^= MaskBit;}
		    );
		    break;
	    case (GXcopyInverted):
		    HORIZONTAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				(~Source & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				(~AltSource & MaskBit);}
		    );
		    break;
	    case (GXandInverted):
		    HORIZONTAL_LINE (
			{*Destination &= ~(Source & MaskBit);},
			{*Destination &= ~(AltSource & MaskBit);}
		    );
		    break;
	    case (GXorReverse):
		    HORIZONTAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination | Source) & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination | AltSource) & MaskBit);}
		    );
		    break;
	    case (GXequiv):
		    HORIZONTAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				((*Destination ^ ~Source) & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				((*Destination ^ ~AltSource) & MaskBit);}
		    );
		    break;
	    case (GXnand):
		    HORIZONTAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination | ~Source) & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination | ~AltSource) & MaskBit);}
		    );
		    break;
	    case (GXnor):
		    HORIZONTAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination & ~Source) & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination & ~AltSource) & MaskBit);}
		    );
		    break;
	    case (GXandReverse):
		    HORIZONTAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination & Source) & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination & AltSource) & MaskBit);}
		    );
		    break;
	    case (GXand):
		    HORIZONTAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				((*Destination & Source) & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				((*Destination & AltSource) & MaskBit);}
		    );
		    break;
	    case (GXorInverted):
		    HORIZONTAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				((*Destination | ~Source) & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				((*Destination | ~AltSource) & MaskBit);}
		    );
	    }
    	} else {	/* This is a vertical line */
	    Sentinel = DeltaY >> 1;
            BitsLeftToDraw = DeltaY;

	    switch (Rule) {
	    case (GXor):
		    VERTICAL_LINE (
			{*Destination |= (Source & MaskBit);},
			{*Destination |= (AltSource & MaskBit);}
		    );
		    break;
	    case (GXcopy):
		    VERTICAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				(Source & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				(AltSource & MaskBit);}
		    );
		    break;
	    case (GXxor):
	            VERTICAL_LINE (
			{*Destination ^= (Source & MaskBit);},
			{*Destination ^= (AltSource & MaskBit);}
		    );
		    break;
	    case (GXset):
	    	    VERTICAL_LINE (
			{*Destination |= MaskBit;},
			{*Destination |= MaskBit;}
		    );
		    break;
	    case (GXclear):
	    	    VERTICAL_LINE(
			{*Destination &= ~MaskBit;},
			{*Destination &= ~MaskBit;}
		    );
		    break;
	    case (GXinvert):
	            VERTICAL_LINE (
			{*Destination ^= MaskBit;},
			{*Destination ^= MaskBit;}
		    );
		    break;
	    case (GXcopyInverted):
		    VERTICAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				(~Source & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				(~AltSource & MaskBit);}
		    );
		    break;
	    case (GXandInverted):
		    VERTICAL_LINE (
			{*Destination &= ~(Source & MaskBit);},
			{*Destination &= ~(AltSource & MaskBit);}
		    );
		    break;
	    case (GXorReverse):
		    VERTICAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination | Source) & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination | AltSource) & MaskBit);}
		    );
		    break;
	    case (GXequiv):
		    VERTICAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				((*Destination ^ ~Source) & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				((*Destination ^ ~AltSource) & MaskBit);}
		    );
		    break;
	    case (GXnand):
		    VERTICAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination | ~Source) & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination | ~AltSource) & MaskBit);}
		    );
		    break;
	    case (GXnor):
		    VERTICAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination & ~Source) & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination & ~AltSource) & MaskBit);}
		    );
		    break;
	    case (GXandReverse):
		    VERTICAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination & Source) & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				((~*Destination & AltSource) & MaskBit);}
		    );
		    break;
	    case (GXand):
		    VERTICAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				((*Destination & Source) & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				((*Destination & AltSource) & MaskBit);}
		    );
		    break;
	    case (GXorInverted):
		    VERTICAL_LINE (
			{*Destination = (*Destination & ~MaskBit) |
				((*Destination | ~Source) & MaskBit);},
			{*Destination = (*Destination & ~MaskBit) |
				((*Destination | ~AltSource) & MaskBit);}
		    );
	    }
	}

#ifdef SOFTWARE_CURSOR
	/*
	 * Restore cursor to screen
	 */

	if (BitMap == SCREEN_BITMAP) {
	    if (restore_cursor() < 0) {
	            DeviceError("SinglePixelLine: ioctl QIOCSHOWCUR failed.\n");
	    }
	}
#endif SOFTWARE_CURSOR

#ifdef AED
	/*
	 * Unable to use microcode to draw the line.
	 * Therefore, it must be echoed to the
	 * AED screen to become visible.
	 */

	if (EchoLine) {
	    changed_rect.origin_y = MIN(StopY, StartY);
            changed_rect.origin_x = MIN(StopX, StartX);
            changed_rect.corner_y = MAX(StopY, StartY) + 1;
            changed_rect.corner_x = MAX(StopX, StartX) + 1;
	    aed_echo_rect(&changed_rect);
	}
#endif AED
}
