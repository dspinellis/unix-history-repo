#ifndef lint
static char *rcsid_copybits_c = "$Header: copybits.c,v 10.1 86/11/19 10:40:50 jg Exp $";
#endif	lint
/* copybits.c -  Interface routine to bitblt()
 *
 *   	CopyBits	Places "X" supplied data into blt structure before
 *			calling bitblt() ounce for each clipping rectangle
 *			in the cliplist.
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
 * General interface routine to bitblt()
 */

CopyBits (srcbits, srcwidth, srcheight, srcrect, dstbits, dstwidth,
	  dstheight, dstrect, maskbits, maskwidth, maskheight, rule,
	  clipcount, cliplist)
	u_short *srcbits, *dstbits, *maskbits;
	register Blt_Rectangle *srcrect, *dstrect;
	int srcwidth, srcheight, dstwidth;
	int maskwidth, maskheight;
	int dstheight, rule;
	register clipcount;
	register CLIP *cliplist;
{
	register Blt *blt = &bltdata;
	register Blt_Bitmap *dstbitmap = &blt->dst_bitmap;
	register Blt_Rectangle *cliprect = &blt->clp_rect;

#ifdef FULL_TRACE_X
	fprintf(stderr, "In CopyBits\n");
	fflush(stderr);
#endif FULL_TRACE_X

	/*
	 * Clear blt structure
	 */

	bzero((char *) blt, sizeof(Blt));

#ifdef AED
	/*
	 * If this is the AED make sure this blt
	 * gets echoed
	 */

	blt->blt_flags |= BLT_ECHO;
#endif AED

	/*
	 * Set up source bitmap or tile
	 */

	if(IS_RULE_TILE(rule)) {
		/*
		 * Srcbits is a tile
		 */

		blt->tile_ptr = (Blt_Tile *)srcbits;
	} else	{
		register Blt_Bitmap *srcbitmap = &blt->src_bitmap;

		/*
		 * Srcbits is a bitimage. Convert bitimage to bitmap
		 */

		BitimageToBitmap(srcbits, 0, 0, srcwidth, srcheight, srcbitmap);

		/*
		 * Copy source rectangle to blt structure
		 */

		blt->src_rect = *srcrect;
	}

	/*
	 * Convert destination bitimage to a bitmap
	 */

	BitimageToBitmap(dstbits, 0, 0, dstwidth, dstheight, dstbitmap);

	/*
	 * Copy destination rectangle to blt structure
	 */

	blt->dst_rect = *dstrect;

	/*
	 * Set combination rule in blt structure
	 */

	blt->comb_rule = rule;

	/*
	 * Is there a clipping mask ?
	 */

	if(maskbits) {
		register Blt_Bitmap *mskbitmap = &blt->msk_bitmap;

		/*
		 * Indicate that there is a mask
		 */

		blt->blt_flags |= BLT_MASKON;

		/*
		 * Convert mask bitimage to bitmap
		 *
		 * NOTE: The clipping mask has the same origin
		 *       as the destination rectangle but can
		 *       be larger in width and height.
		 */

		BitimageToBitmap(maskbits, dstrect->origin_x, dstrect->origin_y,				 maskwidth, maskheight, mskbitmap);
	}

	/*
	 * No X clips specified ?
	 */

	if(clipcount == 0) {
                /*
                 * Turn clipping off during this blt
                 */

                blt->blt_flags &= ~BLT_CLIPON;

                /*
                 * No X clips so blt and run
                 */

                bitblt(blt);
                return;
        }


	/*
	 * Perform same blt for each X clip
	 */

	for (;;) {

		/*
		 * Convert X clip to clipping rectangle
		 */

		ClipToRect(cliplist, cliprect);

		/*
		 * If destination lies inside of clipping rectangle
		 * turn off clipping flag during this blt otherwise 
		 * turn it on.
		 */

		if(InsideBounds(dstrect, cliprect)) {
			blt->blt_flags &= ~BLT_CLIPON;
		} else	{
			blt->blt_flags |= BLT_CLIPON;
		}

		/*
		 * Lets go do the blt
		 */

		bitblt(blt);

		/*
		 * Need to blt again ?
		 */

		if (--clipcount <= 0) {
			/*
			 * No more clips so lets leave
			 */

			break;
		}

		/*
		 * point to next X clip
		 */

		cliplist++;
	}
}
