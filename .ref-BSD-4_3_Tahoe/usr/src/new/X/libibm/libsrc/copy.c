#ifndef lint
static char *rcsid_copy_c = "$Header: copy.c,v 10.1 86/11/19 10:40:31 jg Exp $";
#endif	lint
/* copy.c - Copy one rectangular section of the screen buffer to another
 *
 *      CopyArea        Copies a rectangular section of the screen buffer
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
 * Screen to screen copy
 */

CopyArea (srcx, srcy, width, height, dstx, dsty, clips, clipcount, func, zmask)
        register srcx, srcy, dstx, dsty;
	int clipcount, zmask, func;
	int width, height; 
        CLIP *clips;
{
	register Blt_Rectangle *source = &SrcRect;
	register Blt_Rectangle *dest = &DstRect;

#ifdef TRACE_X
	fprintf(stderr, "In CopyArea\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Limit copy to one plane
	 */

        if ((zmask & 1) == 0)
		return;

	/*
	 * Fill in source and destination rectangles
	 */

        FillInRect(srcx, srcy, width, height, source);
        FillInRect(dstx, dsty, width, height, dest);

	/*
	 * Copy screen source to screen destination
	 */

        CopyBits((u_short *) pbm.data, pbm.width, pbm.height, source,
                 (u_short *) pbm.data, pbm.width, pbm.height, dest,
		 NILMASK, NIL, NIL, func, clipcount, clips);
}
