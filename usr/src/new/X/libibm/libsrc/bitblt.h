/* $Header: bitblt.h,v 10.1 86/11/19 10:45:27 jg Exp $ */
/* bitblt.h - typedefs, macros, and constants required to
 *	      interface to bitblt()
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

#include "../bitblt/bitblt_ext.h"

/*
 * Line structure
 */

typedef struct {
        short 	 StartX;
        short 	 StartY;
        short 	 StopX;
        short 	 StopY;
        short 	 BrushX;
        short 	 BrushY;
	u_short	 Pattern;
	short 	 PatternLength;
	short 	 PatternMultiplier;
	short	 DrawMode;
	Blt_Tile *AlternateTile;
        Blt	 blt;
}Blt_Line;

/*
 * Macros to determine if the given rule refers to a tile
 * or a source bitmap.
 */

#define IS_RULE_SRC(rule)       ((rule) <= GXset)
#define IS_RULE_TILE(rule)       (!IS_RULE_SRC(rule))

/*
 * macros to convert combination rules
 *
 * NOTE: These Macros depend on the order of the rules above.
 */

#define MAKE_TILE_RULE(src_rule)   ((src_rule) + GXset + 1)
#define MAKE_SRC_RULE(tile_rule)   ((tile_rule) - GXset - 1)

/*
 * Macros for fast min/max.
 */

#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

/*
 * misc constants
 */

#define	TRUE	1
#define	FALSE	0

/*
 * macros used to fill in blt structure for bitblt()
 */

#define BitimageToBitmap(bitimage, x, y, width, height, bitmap)    \
	{							   \
		(bitmap)->base = bitimage;			   \
		(bitmap)->rect.origin_y = y;			   \
		(bitmap)->rect.origin_x = x;	  		   \
		(bitmap)->rect.corner_y = y + height; 		   \
		(bitmap)->rect.corner_x = x + width;		   \
		(bitmap)->nshorts = ((width) + 15) / 16;	   \
	}

#define ClipToRect(clip, rect)					 \
	{							 \
		(rect)->origin_y = (clip)->top;			 \
		(rect)->origin_x = (clip)->left;		 \
		(rect)->corner_y = (clip)->top + (clip)->height; \
		(rect)->corner_x = (clip)->left + (clip)->width; \
	}

#define FillInRect(x, y, width, height, rect)      \
	{					   \
                (rect)->origin_y = y;              \
                (rect)->origin_x = x;              \
                (rect)->corner_y = (y) + (height); \
                (rect)->corner_x = (x) + (width);  \
	}

/*
 * test if rectangle one lies within the bounds of rectangle two
 */

#define InsideBounds(rect1, rect2)			\
	(((rect1)->origin_x >= (rect2)->origin_x &&	\
	  (rect1)->origin_x <= (rect2)->corner_x && 	\
	  (rect1)->origin_y >= (rect2)->origin_y &&	\
	  (rect1)->origin_y <= (rect2)->corner_y) &&	\
	((rect1)->corner_x >= (rect2)->origin_x &&	\
	  (rect1)->corner_x <= (rect2)->corner_x && 	\
	  (rect1)->corner_y >= (rect2)->origin_y &&	\
	  (rect1)->corner_y <= (rect2)->corner_y))

/*
 * NILS
 */

#define NILRECT ((Blt_Rectangle *) 0)
#define NILCLIP ((CLIP *) 0)
#define NILBITMAP ((BITMAP *) 0)
#define NILMASK ((u_short *) 0)
#define NILBITS ((u_short *) 0)
#define NIL 0

/*
 * Externs and statics used to interface to bitblt()
 */

static Blt_Rectangle SrcRect;
static Blt_Rectangle DstRect;
static Blt bltdata;
extern Blt_Rectangle changed_rect;
