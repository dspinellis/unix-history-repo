#ifndef lint
static char *rcsid_fill_c = "$Header: fill.c,v 10.2 86/11/25 16:22:07 jg Exp $";
#endif	lint
/* fill.c - Perform a simple raster operation on a section of the screen
 *
 *      PixFill 	Do a function on the screen
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
 * Simple area fill using a mask
 */

/*ARGSUSED*/
PixFill (srcpix, xymask, dstx, dsty, width, height, clips, clipcount,
         func, zmask)
        register srcpix, dstx, dsty, width, height;
	int clipcount, zmask, func;
        BITMAP *xymask;
        CLIP *clips;
{
	u_short *tile = ConstantTiles[srcpix & 1];
	u_short *clipmask = NILMASK;
	register Blt_Rectangle *dest = &DstRect;

#ifdef TRACE_X
	fprintf(stderr, "In PixFill\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 *  Limit fill to one plane
	 */

        if ((zmask & 1) == 0)
            return;

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

	FillInRect(dstx, dsty, width, height, dest);

	/*
	 * Fill destination with contstant tile using a mask
	 */

        CopyBits ((u_short *)tile, NIL, NIL, NILRECT,
                  (u_short *) pbm.data, pbm.width, pbm.height, dest,
		  clipmask, width, height, MAKE_TILE_RULE(func),
		  clipcount, clips);
}
extern int errno;
#include <errno.h>

int StippleFill (srcpix, xoff, yoff, stipmask, dstx, dsty, width, height,
	clips, clipcount, func, zmask)
	int srcpix;		/* source pixel */
	int xoff, yoff;		/* stipple origin */
	BITMAP *stipmask;	/* stipple mask */
	int dstx, dsty;		/* destination */
	int width, height;
	CLIP *clips;		/* clipping rectangles */
	int clipcount;
	int func;		/* GX display function */
	int zmask;		/* plane mask */
{
    	static char funcmap[16][2] = {
		{GXandInverted,	GXandInverted},	/* GXclear */
		{GXandInverted, GXnoop},	/* GXand */
		{GXandInverted,	GXxor},		/* GXandReverse */
		{GXandInverted, GXor},		/* GXcopy */
		{GXnoop,	GXandInverted},	/* GXandInverted */
		{GXnoop,	GXnoop},	/* GXnoop */
		{GXnoop,	GXxor},		/* GXxor */
		{GXnoop,	GXor},		/* GXor */
		{GXxor,		GXandInverted},	/* GXnor */
		{GXxor,		GXnoop},	/* GXequiv */
		{GXxor,		GXxor},		/* GXinvert */
		{GXxor,		GXor},		/* GXorReverse */
		{GXor,		GXandInverted},	/* GXcopyInverted */
		{GXor,		GXnoop},	/* GXorInverted */
		{GXor,		GXxor},		/* GXnand */
		{GXor,		GXor}		/* GXset */
	};
	int newfunc = funcmap [func][srcpix & 1];
	PIXMAP *tile, *MakePixmap();

	if ((stipmask->width != 16) || (stipmask->height != 16)) {
		errno = EINVAL;
		return (0);
	}
	tile = MakePixmap (stipmask, 1, 0);
	TileFill (tile, xoff, yoff, 0, dstx, dsty, width, height,
		clips, clipcount, newfunc, zmask);
	FreePixmap (tile);
	return (1);
}
