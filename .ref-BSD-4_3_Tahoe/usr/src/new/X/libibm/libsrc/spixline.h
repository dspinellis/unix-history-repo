/* $Header: spixline.h,v 10.1 86/11/19 10:46:29 jg Exp $ */
/* spixline.h - macros used by the single pixel wide line drawing routine
 *              SinglePixelLine()
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

#if (defined(APA8) || defined(APA8C))
/*
 * Macro for incrementing destination address
 */

#define DESTINATION \
	Destination = (u_short *) ((long)Destination + DestinationIncrement)
#else
#define DESTINATION    Destination++
#endif (APA8 || APA8C)

/*
 * Macro used to advanced to the next scan line or row of the
 * destination bitmap
 */

#define NEXT_ROW \
	Destination = (u_short *) ((long)Destination + NumberOfBytes)

/*
 * Macro that draws horizontal solid, dashed and patterned lines
 */

#define HORIZONTAL_LINE(Rule1, Rule2) {			\
    switch (Mode) {					\
    case (DrawSolidLine):				\
        while (BitsLeftToDraw--) {			\
	    Rule1		    			\
	    if ((MaskBit >>= 1) == 0) { 		\
		    MaskBit = 0x8000;			\
		    DESTINATION;			\
	    }						\
	    if ((Sentinel -= DeltaY) < 0) {		\
	        NEXT_ROW;				\
	        Sentinel += DeltaX;			\
	    }						\
        } 						\
	Rule1			    			\
	break;						\
    case (DrawDashedLine):				\
        while (BitsLeftToDraw--) {			\
	    if(Pattern & PatternBit)			\
	        Rule1					\
	    if(--RepeatCount == 0) {			\
	        RepeatCount = PatternMultiplier;	\
	        if((PatternBit >>= 1) == 0)		\
	            PatternBit = PatternFirstBit;	\
	    }						\
	    if ((MaskBit >>= 1) == 0) { 		\
	        MaskBit = 0x8000;			\
		DESTINATION;				\
	    }						\
	    if ((Sentinel -= DeltaY) < 0) {		\
	        NEXT_ROW;				\
	        Sentinel += DeltaX;			\
	    }						\
        } 						\
	if(Pattern & PatternBit)			\
	    Rule1					\
	break;						\
    case (DrawPatternedLine):				\
        while (BitsLeftToDraw--) {			\
	    if(Pattern & PatternBit)			\
	        Rule1					\
	    else					\
	        Rule2					\
	    if(--RepeatCount == 0) {			\
	        RepeatCount = PatternMultiplier;	\
	        if((PatternBit >>= 1) == 0)		\
	            PatternBit = PatternFirstBit;	\
	    }						\
	    if ((MaskBit >>= 1) == 0) { 		\
	        MaskBit = 0x8000;			\
		DESTINATION;				\
	    }						\
	    if ((Sentinel -= DeltaY) < 0) {		\
	        NEXT_ROW;				\
	        Sentinel += DeltaX;			\
	    }						\
        } 						\
	if(Pattern & PatternBit)			\
	    Rule1					\
	else						\
	    Rule2					\
    }							\
}

/*
 * Macro that draws vertical solid, dashed and patterned lines
 */

#define VERTICAL_LINE(Rule1, Rule2) {			\
    switch (Mode) {					\
    case (DrawSolidLine):				\
        while (BitsLeftToDraw--) {			\
	    Rule1		    			\
	    NEXT_ROW;					\
	    if ((Sentinel -= DeltaX) < 0) {		\
	        if ((MaskBit >>= 1) == 0) { 		\
		        MaskBit = 0x8000;		\
			DESTINATION;			\
	        }					\
	        Sentinel += DeltaY;			\
	    }						\
        } 						\
        Rule1		    				\
	break;						\
    case (DrawDashedLine):				\
        while (BitsLeftToDraw--) {			\
	    if(Pattern & PatternBit)			\
	        Rule1					\
	    if(--RepeatCount == 0) {			\
	        RepeatCount = PatternMultiplier;	\
	        if((PatternBit >>= 1) == 0)		\
	            PatternBit = PatternFirstBit;	\
	    }						\
	    NEXT_ROW;					\
	    if ((Sentinel -= DeltaX) < 0) {		\
	        if ((MaskBit >>= 1) == 0) { 		\
	            MaskBit = 0x8000;			\
		    DESTINATION;			\
	        }					\
	        Sentinel += DeltaY;			\
	    }						\
        } 						\
        if(Pattern & PatternBit)			\
            Rule1					\
	break;						\
    case (DrawPatternedLine):				\
        while (BitsLeftToDraw--) {			\
	    if(Pattern & PatternBit)			\
	        Rule1					\
	    else					\
	        Rule2					\
	    if(--RepeatCount == 0) {			\
	        RepeatCount = PatternMultiplier;	\
	        if((PatternBit >>= 1) == 0)		\
	            PatternBit = PatternFirstBit;	\
	    }						\
	    NEXT_ROW;					\
	    if ((Sentinel -= DeltaX) < 0) {		\
	        if ((MaskBit >>= 1) == 0) { 		\
	            MaskBit = 0x8000;			\
		    DESTINATION;			\
	        }					\
	        Sentinel += DeltaY;			\
	    }						\
        } 						\
        if(Pattern & PatternBit)			\
	    Rule1					\
        else						\
	    Rule2					\
    }							\
}

/*
 * Cohen-Sutherland clipping algorithm
 *
 * See: Fundamentals of Interactive Computer Graphics
 *      J. D. Foley & A. Van Dam
 *      Pages 146 - 149
 */

#define OUTCODE_TOP		0x08
#define OUTCODE_BOTTOM		0x04
#define OUTCODE_RIGHT		0x02
#define OUTCODE_LEFT		0x01


#define OUTCODES(x, y, clip)					\
     (((x) < clip->left ? OUTCODE_LEFT :			\
        (x) >= clip->left + clip->width ? OUTCODE_RIGHT : 0)	\
	    +((y) < clip->top ? OUTCODE_TOP :			\
	(y) >= clip->top + clip->height ? OUTCODE_BOTTOM : 0))

#define ClipLine(x0, y0, x1, y1, clip) {				    \
									    \
    if (clip) {								    \
        int OutCode0 = OUTCODES(x0, y0, clip);				    \
        int OutCode1 = OUTCODES(x1, y1, clip);				    \
									    \
        while (OutCode0 | OutCode1) {	/* trivially accepted ? */	    \
            if (OutCode0 & OutCode1)	/* trivially rejected ? */	    \
                return;							    \
            if (OutCode0) {						    \
		if (OutCode0 & (OUTCODE_LEFT | OUTCODE_RIGHT)) {	    \
		    int Clip_X = (OutCode0 & OUTCODE_LEFT) ? clip->left :   \
				clip->left + clip->width - 1;		    \
									    \
	     	    y0 = y0 + (y1 - y0) * (Clip_X - x0) / (x1 - x0);	    \
	     	    x0 = Clip_X;					    \
		} else if (OutCode0 & (OUTCODE_TOP | OUTCODE_BOTTOM)) {	    \
		    int Clip_Y = (OutCode0 & OUTCODE_TOP) ? clip->top :	    \
				clip->top + clip->height - 1;		    \
									    \
	     	    x0 = x0 + (x1 - x0) * (Clip_Y - y0) / (y1 - y0);	    \
	     	    y0 = Clip_Y;					    \
	    	}							    \
	 	OutCode0 = OUTCODES(x0, y0, clip);			    \
	    } else if (OutCode1) {					    \
		if (OutCode1 & (OUTCODE_LEFT | OUTCODE_RIGHT)) {	    \
		    int Clip_X = (OutCode1 & OUTCODE_LEFT) ? clip->left :   \
				clip->left + clip->width - 1; 		    \
     									    \
	     	    y1 = y0 + (y1 - y0) * (Clip_X - x0) / (x1 - x0);	    \
	     	    x1 = Clip_X; 					    \
	    	} else if (OutCode1 & (OUTCODE_TOP | OUTCODE_BOTTOM)) {     \
		    int Clip_Y = (OutCode1 & OUTCODE_TOP) ? clip->top :	    \
				clip->top + clip->height - 1; 		    \
									    \
	     	    x1 = x0 + (x1 - x0) * (Clip_Y - y0) / (y1 - y0);	    \
	     	    y1 = Clip_Y; 					    \
	    	} 							    \
	 	OutCode1 = OUTCODES(x1, y1, clip);			    \
	    } 								    \
	} 								    \
   } 									    \
}
