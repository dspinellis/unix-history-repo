#ifndef lint
static char *rcsid_bitblt_subr_c = "$Header: bitblt_subr.c,v 10.1 86/11/19 10:51:21 jg Exp $";
#endif	lint
/*
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 *
 * Written by Daniel Stone, Brown University/IRIS  (des@iris)
 */

/*
 * CURRENT HISTORY:
 *
 * $Log:	bitblt_subr.c,v $
 * Revision 10.1  86/11/19  10:51:21  jg
 * Device dependent code for IBM displays.  General blitter...
 * 
 * Revision 4.0  86/09/25  10:03:10  des
 * Version 4.0. Works with all IBM screens.
 * 
 * Revision 3.6  86/09/22  16:47:44  des
 * Added a third argument to blt_init.
 * 
 * Revision 3.5  86/09/22  13:37:29  des
 * Version 3.5. Removed all ifdef's that did not pertain to X.
 * 
 * Revision 3.0  86/09/17  10:29:59  des
 * Version 3.0.  Works but has ifdef's for all systems.
 * 
 * Revision 7.0  86/07/03  10:40:03  des
 * Release 7.0.
 * 
 * Revision 6.1  86/07/03  10:29:10  des
 * Replaced certain mask rules.
 * 
 * Revision 6.0  86/06/18  11:19:59  des
 * Changed unsigned short pointers to unsigned char pointers for efficiency
 * reasons.  This change ment multiplying by 2 every variable added to
 * these pointers.
 * 
 * Revision 5.0  86/05/21  14:26:02  des
 * Revision 5.0.  No longer uses unsigned short pointers.
 * 
 * Revision 4.7  86/05/15  16:03:45  des
 * Latest and faster version of the new copyBitPos loop.  Runs 5% to 7% faster
 * than the old loop.
 * 
 * Revision 4.6  86/05/14  10:54:46  des
 * Changed smallcopyBitPos_LOOP so that it checks for 1 or 2 words changed
 * and branches to that loop.
 * 
 * Revision 4.5  86/05/14  10:31:57  des
 * bitblt with NEW_LOOP (needs work.)
 * 
 * Revision 4.0  86/05/09  13:52:25  des
 * Revision 4.0.  Apa-16 hardware enhancements Version 0.6.
 * 
 * Revision 3.2  86/04/30  15:34:16  des
 * This version has smallcopyBitPos and the old copyBitPos.  The new copyBitPos
 * took off .1 second on a 111 second blt. (new: 111.1 old:111.2) so what right?
 * The old copyBitPos did not need smallcopyBitPos around so I figured it was
 * better.
 * 
 * Revision 2.2  86/04/29  16:31:53  des
 * Added the smallcopyBitPos routine for speeding up small blts.
 * 
 * Revision 3.0  86/04/28  16:59:20  des
 * Runs with X. Version 0.5.
 * 
 * Revision 2.2  86/04/09  09:36:41  des
 * Fixed APA-8 and APA-8C software locator problem.
 * 
 * Revision 2.1  86/04/04  12:19:38  des
 * Moved copyBitNeg,copyBitPos,copyTile,maskBitNeg,maskBitPos,maskTile,
 * clip_and_maskBitNeg,clip_and_maskBitPos and clip_and_maskTile to
 * bitblt_subr.c.
 */

#ifndef lint
static char rcsid[] =
"$Header: bitblt_subr.c,v 10.1 86/11/19 10:51:21 jg Exp $ (Brown/IRIS)";
#endif

#include "bitblt_int.h"

/*
 * Keep track of the first and one beyond the last address of the screen,
 * so that current_screen.firstaddr <= some_base < current_screen.lastaddr
 * is true if some_base is a pointer to a section of the screen.
 */
Blt_screen_info blt_cur_screen;
 
static Blt_Tile blt_black = {
	0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,
	0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff
};

extern Blt_Rectangle changed_rect;/* describes the area changed by the bltter */

/*
 * Set 'ans' to the largest value and set 'diff' equal to the difference
 * between 'ans' and 'a'.
 */
#define max_a(a,b,c,ans,diff) {		\
	if (a >= b) {			\
		if (a >= c) {		\
			diff = 0;	\
			ans = a;	\
		}			\
		else {			\
			diff = c - a;	\
			ans = c;	\
		}			\
	}				\
	else if (b > c) {		\
		diff = b - a;		\
		ans = b;		\
	}				\
	else {				\
		diff = c - a;		\
		ans = c;		\
	}				\
}

/*
 * Set 'ans' to the smallest value and set 'diff' equal to the difference
 * between 'ans' and 'a'.
 */
#define min_a(a,b,c,ans,diff) {		\
	if (a <= b) {			\
		if (a <= c) {		\
			diff = 0;	\
			ans = a;	\
		}			\
		else {			\
			diff = a - c;	\
			ans = c;	\
		}			\
	}				\
	else if (b < c) {		\
		diff = a - b;		\
		ans = b;		\
	}				\
	else {				\
		diff = a - c;		\
		ans = c;		\
	}				\
}

/* 
 * Initialize the bitblt so that it knows where in the address space the 
 * screen is and whether or not it uses a hardware or software cursor. 
 */
bitblt_init(screen_base,screen_wd,screen_ht,flags)
unsigned short *screen_base;	/* The first address of the bitmap */
long screen_wd;			/* Screen width in BITS (will round up) */
long screen_ht;			/* Screen height in scan lines */
unsigned long flags;		/* Indicates the type of cursor on the screen.*/
{
	blt_cur_screen.firstaddr = screen_base;
	blt_cur_screen.lastaddr = (unsigned short *)((long)screen_base +
				  (((screen_wd+7)/8) * screen_ht));
	blt_cur_screen.cursortype = flags;
}

/*
 * Blt_setup takes information from the user data and creates data
 * to be used internally.  Specificly it creates clipped source rectangle
 * if a source combination rule is given, a clipped destination rectangle
 * the number of destination words to be changed across a scanline
 * (nshorts) and masking bitmap rectangle masking is to be done.
 * Finally the width and height and rule are filled in as width,
 * height and rule.
 * 
 * If a bad pointer was encountered  (or something else is wrong) then
 * blt_setup returns -1. Returns 0 if combination rule was a no-op and
 * 1 if blt_setup finished ok.
 */
blt_setup(u_info,s_info)
register Blt_userdata *u_info;	/* User bitblt data */
register Blt_sysdata *s_info;	/* bitblt internal data (This is modified
				   bitblt user data depending on the clipping
				   to be done. */
{
	register int x_orig,y_orig;/* because the destination x,y origin
				      is used so much, it will be put in
				      registers. */
	register int Oxdiff,Cxdiff;/* If the destination rectangle is clipped
				      then these variables contain the
				      difference between the destination
				      rectangle and the rectangle used
				      to clip. */
	register int Oydiff,Cydiff;/* If the destination rectangle is clipped
				      then these variables contain the
				      difference between the destination
				      rectangle and the rectangle used
				      to clip. */

	DEBUGF((blt_debug > 1),
	       printf("Enter blt_setup.0x%x\r\n",u_info->blt_flags));

	/*
	 * Set up the internal bltters combination rule.
	 */
	s_info->rule = u_info->comb_rule;

	/*
	 * Set a flag indicating that the rule uses the source.
	 */
	s_info->is_src = IS_SRC(s_info->rule);

	/*
	 * Make sure s_info->tile_ptr = 0. 
	 */
	s_info->tile_ptr = (unsigned short *)0;

	/*
	 * Check the bltstruct to make sure every pointer is valid.
	 */
	if (u_info == (Blt_userdata *)0) {
		printf("Error: NULL bltstruct given!\n");
		return(-1);
	}

	if (u_info->dst_bitmap.base == (unsigned short *)0) {
		printf("Error: NULL destination bitmap!\n");
		return(-1);
	}

	/*
	 * If a source combination rule was given and no bitmap was given then
	 * we assume the user wants US to assume that the source is infinitely
	 * large and is set to all one's.  This is the same thing as using
	 * the coorisponding tile rule and an all black tile.
	 */
	if (s_info->is_src && u_info->src_bitmap.base == (unsigned short *)0) {
		s_info->is_src = 0;
		s_info->rule += SRC_TO_TILE;
		s_info->tile_ptr = &blt_black.tile[0];
	}

	/*
	 * Check the rule and if its a no-op then return immediately.
	 */
	if (s_info->rule == DstCopy || s_info->rule == TileDstCopy) {
		return(0);
	}

	if (u_info->blt_flags & BLT_CLIPON) {
		/* DEBUGF(blt_debug,printf("Clip.\r\n")); */
		/*
		 * Do some clipping.  We clip to the clipping rectangle,
		 * the destination bitmap's rect and the destination
		 * rectangle.  NOTE that if the destination rectangle
		 * is clipped in any way then the source rectangle must
		 * also be changed, thats why we keep track of the difference
		 * in Oxdiff.
		 *
		 * Set the origin to be a zero based coorinate system.
		 */
		max_a(u_info->dst_rect.origin_x,
		      u_info->clp_rect.origin_x,
		      u_info->dst_bitmap.rect.origin_x,
		      x_orig,Oxdiff);

		x_orig -= u_info->dst_bitmap.rect.origin_x;

		/*
		 * Take the min value of the clipping rectangle X corner
		 * coord, bitmap rectangle X corner coord. and the
		 * destination rectangle X corner coord.. Cxdiff
		 * contains the difference between dst_rect.corner_x and
		 * the value returned.  Then convert to a zero based
		 * coordinate system.
		 */
		min_a(u_info->dst_rect.corner_x,
		      u_info->clp_rect.corner_x,
		      u_info->dst_bitmap.rect.corner_x,
		      s_info->dst.rect.corner_x,Cxdiff);

		s_info->dst.rect.corner_x -= u_info->dst_bitmap.rect.origin_x;

		/*
		 * Take the max value of the clipping rectangle Y origin
		 * coord, bitmap rectangle Y origin coord. and the
		 * destination rectangle Y origin coord..  Oydiff
		 * contains the difference between dst_rect.origin_y and
		 * the value returned.  Then convert to a zero based
		 * coordinate system.
		 */
		max_a(u_info->dst_rect.origin_y,
		      u_info->clp_rect.origin_y,
		      u_info->dst_bitmap.rect.origin_y,
		      y_orig,Oydiff);

		y_orig -= u_info->dst_bitmap.rect.origin_y;

		/*
		 * Take the min value of the clipping rectangle Y corner
		 * coord, bitmap rectangle Y corner coord. and the
		 * destination rectangle Y corner coord..  Cydiff
		 * contains the difference between dst_rect.corner_y and
		 * the value returned.  Then convert to a zero based
		 * coordinate system.
		 */
		min_a(u_info->dst_rect.corner_y,
		      u_info->clp_rect.corner_y,
		      u_info->dst_bitmap.rect.corner_y,
		      s_info->dst.rect.corner_y,Cydiff);

		s_info->dst.rect.corner_y -= u_info->dst_bitmap.rect.origin_y;
/*
 *		printf("dst:\t%4d,%4d,%4d,%4d\n",x_orig,
 *			y_orig,s_info->dst.rect.corner_x,
 *			s_info->dst.rect.corner_y);
 *		printf("diffs:\t%4d,%4d,%4d,%4d\n",Oxdiff,Oydiff,Cxdiff,Cydiff);
 */

		/*
		 * Adjust source if necessary. (May only be using a tile.)
		 */
		if (s_info->is_src) {
			/*
			 * We may have done some clipping. If X was clipped
			 * then the section of the source that would have
			 * been mapped to the clipped section of the
			 * destination must also be clipped.
			 *      In other words if one clips the destination one
			 * must also clip the source.  Anyways 'Oxdiff' is set
			 * to the difference.  Then we subtract the rect
			 * of the source bitmap so as to get the source into
			 * the same coordinate system as the destination,
			 * a zero based system.
			 */ 
			s_info->src.rect.origin_x = u_info->src_rect.origin_x +
					  	    Oxdiff -
					       u_info->src_bitmap.rect.origin_x;

			/*
			 * If Y was clipped then the section of source
			 * that would have mapped to the clipped section
			 * of the destination must also be clipped.
			 */
			s_info->src.rect.origin_y = u_info->src_rect.origin_y +
						    Oydiff -
					       u_info->src_bitmap.rect.origin_y;

			/*
			 * Now clip to the source bitmap's origin which is
			 * now 0,0 because we subtracted its origin above.
			 */
			if (s_info->src.rect.origin_x < 0) {
				/*
				 * The source rectangle has been clipped so
				 * the destination rectangle must also be
				 * clipped.
				 */
				x_orig -= s_info->src.rect.origin_x;
				s_info->src.rect.origin_x = 0;
			}
			if (s_info->src.rect.origin_y < 0) {
				/*
				 * The source rectangle has been clipped so
				 * the destination rectangle must also be
				 * clipped.
				 */
				y_orig -= s_info->src.rect.origin_y;
				s_info->src.rect.origin_y = 0;
			}

			/*
			 * Calculate the source rectangle's corner rect.
			 * NOTE: We are not setting these rect to a zero
			 *       based system yet.
			 */
			s_info->src.rect.corner_x = u_info->src_rect.corner_x -
						    Cxdiff;
			s_info->src.rect.corner_y = u_info->src_rect.corner_y -
						    Cydiff;

			/*
			 * Clip these rect to the corner of the source
			 * bitmap corner rect.
			 */
			if (s_info->src.rect.corner_x >
					     u_info->src_bitmap.rect.corner_x) {
				s_info->src.rect.corner_x =
					       u_info->src_bitmap.rect.corner_x;
			}
			if (s_info->src.rect.corner_y >
					     u_info->src_bitmap.rect.corner_y) {
				s_info->src.rect.corner_y =
					       u_info->src_bitmap.rect.corner_y;
			}

			/*
			 * Now set these rect to a zero based coordinate
			 * system.
			 */
			s_info->src.rect.corner_x -=
					       u_info->src_bitmap.rect.origin_x;
			s_info->src.rect.corner_y -=
					       u_info->src_bitmap.rect.origin_y;

			/*
			 * Calculate the width in pixels of the blt area
			 * using the smaller of the two widths found in
			 * the source and destination rectangles.
			 */
			if ((s_info->src.rect.corner_x -
			     s_info->src.rect.origin_x) >
			    (s_info->dst.rect.corner_x - x_orig)) {
				s_info->width = s_info->dst.rect.corner_x -
						x_orig;
			}
			else {
				s_info->width = s_info->src.rect.corner_x -
						s_info->src.rect.origin_x;

				/*
				 * Must change the destination corner.
				 */
				s_info->dst.rect.corner_x = x_orig +
						            s_info->width;
			}

			/*
			 * Calculate the height in scanlines of the blt area
			 * using the smaller of the two heights found in 
			 * the source and destination rectangles.
			 */
			if ((s_info->src.rect.corner_y -
			     s_info->src.rect.origin_y) >
			    (s_info->dst.rect.corner_y - y_orig)) {
				s_info->height = s_info->dst.rect.corner_y -
						 y_orig;
			}
			else {
				s_info->height = s_info->src.rect.corner_y -
						 s_info->src.rect.origin_y;

				/*
				 * Must change the destination corner.
				 */
				s_info->dst.rect.corner_y = y_orig +
						            s_info->height;
			}

			/*
			 * Set up source corners to BE CLOSED CORNERS.
			 * This is done because alot of -1's are being used in
			 * calculations of pointers when copying bitmaps from
			 * corner_y to origin_y, corner_x to origin_x.
			 * (for copyBitNeg(), see calculateOffsets())
			 */
			s_info->src.rect.origin_x = s_info->src.rect.origin_x;
			s_info->src.rect.origin_y = s_info->src.rect.origin_y;
			s_info->src.rect.corner_x = s_info->src.rect.origin_x +
						    s_info->width - 1;
			s_info->src.rect.corner_y = s_info->src.rect.origin_y +
						    s_info->height - 1;
		}
		else {
			/*
			 * Calculate the width in pixels and the height in
			 * scanlines.
			 */
			s_info->width = s_info->dst.rect.corner_x - x_orig;
			s_info->height = s_info->dst.rect.corner_y - y_orig;
		}
	}
	else {
		/*
		 * Assume that the user has set everything up legally and
		 * all we have to do is set up our bltter (internal)
		 * coordinates.
		 *
		 * Set up the x origin and width.
		 */
		x_orig = u_info->dst_rect.origin_x;
		s_info->width = u_info->dst_rect.corner_x - x_orig;

		/*
		 * Set up y origin and height.
		 */
		y_orig = u_info->dst_rect.origin_y;
		s_info->height = u_info->dst_rect.corner_y - y_orig;

		/*
		 * Convert to zero based coordinate system.
		 */
		x_orig -= u_info->dst_bitmap.rect.origin_x;
		y_orig -= u_info->dst_bitmap.rect.origin_y;

		/*
		 * Set up the destination corners.
		 */
		s_info->dst.rect.corner_x = x_orig + s_info->width;
		s_info->dst.rect.corner_y = y_orig + s_info->height;


		/*
		 * Adjust source if necessary. (May only be using a tile.)
		 */
		if (s_info->is_src) {
			/*
			 * We must have source and destination coordinates
			 * which are in the same coordinate system, a
			 * zero-based coordinate system in this case.
			 */
			s_info->src.rect.origin_x = u_info->src_rect.origin_x -
				               u_info->src_bitmap.rect.origin_x;
			s_info->src.rect.origin_y = u_info->src_rect.origin_y -
				               u_info->src_bitmap.rect.origin_y;

			/*
			 * Set up source corners to BE CLOSED CORNERS.
			 * This is done because alot of -1's are being used in
			 * calculations of pointers when copying bitmaps from
			 * corner_y to origin_y, corner_x to origin_x.
			 * (for copyBitNeg(), see calculateOffsets())
			 */
			s_info->src.rect.corner_x = s_info->src.rect.origin_x +
					            s_info->width - 1;
			s_info->src.rect.corner_y = s_info->src.rect.origin_y +
					            s_info->height - 1;
		}
	}

	/*
	 * Keep the final rectangle for outside use.  (Others can extern
	 * changed_rect.)
	 */
	changed_rect.origin_x = x_orig;
	changed_rect.origin_y = y_orig;
	changed_rect.corner_x = s_info->dst.rect.corner_x;
	changed_rect.corner_y = s_info->dst.rect.corner_y;

	/*
	 * Set up destination corners to BE CLOSED CORNERS.  This is done
	 * because alot of -1's are being used in calculations of pointers when 
	 * copying bitmaps from corner_y to origin_y, corner_x to
	 * origin_x. (for copyBitNeg(), see calculateOffsets())
	 */
	s_info->dst.rect.corner_x -= 1;
	s_info->dst.rect.corner_y -= 1;

	/*
	 * Set up the number of words across the destination that will be
	 * affected. (Because we subracted one we don't have to subtract one
	 * from dst.rect.corner_x.)
	 */
	if ((s_info->nshorts = (DIV_BPW(s_info->dst.rect.corner_x) -
	    DIV_BPW(x_orig)) + 1) < 1) {
		/*
		 * nshorts should always be positive.
		 */
		DEBUGF((blt_debug),
			printf("dst cx,ox: %d,%d nshorts = %d - %d\n",
				s_info->dst.rect.corner_x,x_orig,
				DIV_BPW(s_info->dst.rect.corner_x),
				DIV_BPW(x_orig)));
		return(0);
	}
	DEBUGF((blt_debug),
			printf("dst cx,ox: %d,%d nshorts = %d - %d, (IS %d)\n",
				s_info->dst.rect.corner_x,x_orig,
				DIV_BPW(s_info->dst.rect.corner_x),
				DIV_BPW(x_orig),s_info->nshorts));

	/*
	 * NOTE: This program ASSUMES that msk_bitmap's bounds are within
	 *	 the bounds of the msk_bitmap.  If not, bummer.
	 *
	 * Mask bitmap specified?? Set up mask corners.
	 */
	if (u_info->blt_flags & BLT_MASKON) {
		/*
		 * Currently, the msk_bitmap's rect is in destination
		 * coordinates and msk_bitmap.rect picks out the area of the
		 * mask to be used.
		 * 
		 * Align the mask bitmap with the destination rectangle.
		 * Because the x_orig is zero based and the
		 * msk_bitmap box is not we add back the dst_bitmap.rect.
		 * This has the effect of putting the mask corners into
		 * a zero based coordinate system.
		 */
		s_info->msk.rect.origin_x = x_orig +
				            u_info->dst_bitmap.rect.origin_x -
				            u_info->msk_bitmap.rect.origin_x;
		s_info->msk.rect.corner_x = s_info->msk.rect.origin_x +
				            s_info->width - 1;
		s_info->msk.rect.origin_y = y_orig +
				            u_info->dst_bitmap.rect.origin_y -
				            u_info->msk_bitmap.rect.origin_y;
		s_info->msk.rect.corner_y = s_info->msk.rect.origin_y +
					    s_info->height - 1;

		/*
		 * At some point it may be smart to check the clip flag and
		 * clip this also.
		 *
		 * if (u_info->blt_flags & BLT_CLIPON) {
		 * }
		 */
	}

	/*
	 * Set the temp varibles into the system data structure.
	 */
	s_info->dst.rect.origin_x = x_orig;
	s_info->dst.rect.origin_y = y_orig;

	return(1);
}

/*
 * This part of this file should be moved to another file someday.
 */

#define MSK_SKEW 0x01
#define SRC_SKEW 0x02

/*
 * Macros for the operations.
 */

#define SrcCopy_MASK(src,dst,mask)    dst = ((dst) & ~(mask)) |  \
					    ((src) & mask)

/*
 * Old SrcOr rule.
 * #define SrcOr_MASK(src,dst,mask)      dst = ((dst) & ~(mask)) |  \
 *					    (((dst) | (src)) & mask)
 */
#define SrcOr_MASK(src,dst,mask)      dst = ((dst) | ((src) & mask))

/*
 * Old SrcXor rule.
 * #define SrcXor_MASK(src,dst,mask)     dst = ((dst) & ~(mask)) |  \
 *					    (((dst) ^ (src)) & mask)
 */
#define SrcXor_MASK(src,dst,mask)     dst = ((dst) ^ ((src) & mask))

#define SrcAnd_MASK(src,dst,mask)      dst = ((dst) & ~(mask)) |  \
					    (((dst) & (src)) & mask)

#define NotSrcCopy_MASK(src,dst,mask) dst = ((dst) & ~(mask)) |  \
					    (~(src) & mask)

/*
 * Old NotSrcOr rule.
 * #define NotSrcOr_MASK(src,dst,mask)   dst = ((dst) & ~(mask)) |  \
 *					    (((dst) | ~(src)) & mask)
 */
#define NotSrcOr_MASK(src,dst,mask)   dst = ((dst) | (~(src) & mask))

/*
 * Old NotSrcXor rule.
 * #define NotSrcXor_MASK(src,dst,mask)  dst = ((dst) & ~(mask)) |  \
 *					    (((dst) ^ ~(src)) & mask)
 */
#define NotSrcXor_MASK(src,dst,mask)  dst = ((dst) ^ (~(src) & mask))

#define NotSrcAnd_MASK(src,dst,mask)  dst = ((dst) & ~(mask)) |  \
					    (((dst) & ~(src)) & mask)
/*
 * DstCopy is a no-op.
 */

#define SrcAndNotDst_MASK(src,dst,mask) dst = ((dst) & ~(mask)) |  \
					      ((~(dst) & (src)) & mask)

#define SrcOrNotDst_MASK(src,dst,mask)     dst = ((dst) & ~(mask)) |  \
					         ((~(dst) | (src)) & mask)

#define NotDstCopy_MASK(src,dst,mask)      dst = ((dst) & ~(mask)) | \
						 (~(dst) & (mask))

#define NotSrcAndNotDst_MASK(src,dst,mask) dst = ((dst) & ~(mask)) |  \
					         ((~(dst) & ~(src)) & mask)

#define NotSrcOrNotDst_MASK(src,dst,mask)  dst = ((dst) & ~(mask)) |  \
					         ((~(dst) | ~(src)) & mask)

#define SrcCopy_OP(src,dst)	dst = (src)
#define SrcOr_OP(src,dst)	dst = (dst) | (src)
#define SrcXor_OP(src,dst)	dst = (dst) ^ (src)
#define SrcAnd_OP(src,dst)	dst = (dst) & (src)
#define NotSrcCopy_OP(src,dst)	dst = ~(src)
#define NotSrcOr_OP(src,dst)	dst = (dst) | ~(src)
#define NotSrcXor_OP(src,dst)	dst = (dst) ^ ~(src)
#define NotSrcAnd_OP(src,dst)	dst = (dst) & ~(src)
/*
 * DstCopy is a no-op (dst = (dst)).
 */
#define SrcOrNotDst_OP(src,dst)     dst = (~(dst) | (src))
#define SrcAndNotDst_OP(src,dst)    dst = (~(dst) & (src))
#define NotDstCopy_OP(src,dst)      dst = ~(dst)
#define NotSrcOrNotDst_OP(src,dst)  dst = (~(dst) | ~(src))
#define NotSrcAndNotDst_OP(src,dst) dst = (~(dst) & ~(src))

/*
 * Make the mask and operation macros for tiles.
 */
#define TileCopy_MASK(tile,dst,mask)    dst = ((dst) & ~(mask)) |  \
					      ((tile) & mask)

#define TileOr_MASK(tile,dst,mask)      dst = ((dst) & ~(mask)) |  \
					      (((dst) | (tile)) & mask)

#define TileXor_MASK(tile,dst,mask)     dst = ((dst) & ~(mask)) |  \
					      (((dst) ^ (tile)) & mask)

#define NotTileAnd_MASK(tile,dst,mask)  dst = ((dst) & ~(mask)) |  \
					      (((dst) & ~(tile)) & mask)

#define NotTileCopy_MASK(tile,dst,mask) dst = ((dst) & ~(mask)) |  \
					      (~(tile) & mask)

#define NotTileOr_MASK(tile,dst,mask)   dst = ((dst) & ~(mask)) |  \
					      (((dst) | ~(tile)) & mask)

#define NotTileXor_MASK(tile,dst,mask)  dst = ((dst) & ~(mask)) |  \
					      (((dst) ^ ~(tile)) & mask)

#define TileAnd_MASK(tile,dst,mask)     dst = ((dst) & ~(mask)) |  \
					      (((dst) & (tile)) & mask)

#define TileOrNotDst_MASK(tile,dst,mask)      dst = ((dst) & ~(mask)) |  \
					            ((~(dst) | (tile)) & mask)

#define NotTileAndNotDst_MASK(tile,dst,mask)  dst = ((dst) & ~(mask)) |  \
					            ((~(dst) & ~(tile)) & mask)

#define NotTileOrNotDst_MASK(tile,dst,mask)   dst = ((dst) & ~(mask)) |  \
					            ((~(dst) | ~(tile)) & mask)

#define TileAndNotDst_MASK(tile,dst,mask)     dst = ((dst) & ~(mask)) |  \
					            ((~(dst) & (tile)) & mask)

#define TileCopy_OP(tile,dst)	 dst = (tile)
#define TileOr_OP(tile,dst)	 dst = (dst) | (tile)
#define TileXor_OP(tile,dst)	 dst = (dst) ^ (tile)
#define NotTileAnd_OP(tile,dst)	 dst = (dst) & ~(tile)
#define NotTileCopy_OP(tile,dst) dst = ~(tile)
#define NotTileOr_OP(tile,dst)	 dst = (dst) | ~(tile)
#define NotTileXor_OP(tile,dst)	 dst = (dst) ^ ~(tile)
#define TileAnd_OP(tile,dst)	 dst = (dst) & (tile)

#define TileOrNotDst_OP(tile,dst)	dst = ~(dst) | (tile)
#define NotTileAndNotDst_OP(tile,dst)	dst = ~(dst) & ~(tile)
#define NotTileOrNotDst_OP(tile,dst)	dst = ~(dst) | ~(tile)
#define TileAndNotDst_OP(tile,dst)	dst = ~(dst) & (tile)

#if (APA8 || APA8C)
/*
 * Defines for how a pointer is incremented.  This is needed because one must
 * increment a pointer to the APA-8 bitmap by 2.
 */
#define DECR_SRC(src)	src -= src_plus
#define DECR_DST(dst)	dst -= dst_plus
#define INCR_SRC(src)	src += src_plus
#define INCR_DST(dst)	dst += dst_plus
#define MINUS_SRC(src)	((unsigned short *)(src - src_plus))
#define PLUS_SRC(src)	((unsigned short *)(src + src_plus))
#else no APA8 stuff
/*
 * Defines for how normal pointers are incremented.
 */
#define DECR_SRC(src)	(src -= 2)
#define DECR_DST(dst)	(dst -= 2)
#define INCR_SRC(src)	(src += 2)
#define INCR_DST(dst)	(dst += 2)
#define MINUS_SRC(src)	((unsigned short *)(src - 2))
#define PLUS_SRC(src)	((unsigned short *)(src + 2))
#endif

#define SRC	((unsigned short *)src)
#define DST	((unsigned short *)dst)
#define TILE	((unsigned short *)tile)
#define MSK	((unsigned short *)msk)
#define CLP	((unsigned short *)clp)

#define DECR_MSK(msk)	(msk -= 2)
#define INCR_MSK(msk)	(msk += 2)
#define MINUS_MSK(msk)	((unsigned short *)(msk - 2))
#define PLUS_MSK(msk)	((unsigned short *)(msk + 2))

#define DECR_CLP(clp)	(clp -= 2)
#define INCR_CLP(clp)	(clp += 2)
#define MINUS_CLP(clp)	((unsigned short *)(clp - 2))
#define PLUS_CLP(clp)	((unsigned short *)(clp + 2))

/*
 * Macro to increment the tile pointer.  If last_ptr is reached then the
 * pointer is decremented back to the beginning of the tile struct.
 */
#define INCR_TILE_PTR(ptr,last_ptr) ((ptr==last_ptr) ? (ptr -= 30) : (ptr += 2))

/*
 * This loop copies bits from right to left, bottom to top.  It takes as
 * arguments 1 of 8 masking macros and the coorisponding generic macro.
 * The general scheme is this:
 *	Foreach row do
 *		Take care of the first word priming if necessary. (Done because
 *			the first word uses the RIGHT edge mask.)
 *
 *		If there is a skew then (Means parts of 2 source words make one
 *			destination word.)
 *			For each word between the first and last word do
 *				Shift the source word and source word -1 and
 *					write the word to the destination.
 *			endfor
 *
 *			Take care of the last word. (Done here and not in 
 *				the loop because the last word uses the 
 *				LEFT edge mask.)
 * 
 *		else  The skew is zero meaning 1 source word per destination
 *		      word.
 * 
 *			For each word between the first and last word do
 *				Write the source word into the destination word.
 *			endfor
 *
 *			Take care of the last word. (Done here and not in 
 *				the loop because the last word uses the 
 *				LEFT edge mask.)
 *		endif
 *
 *		DECREMENT the source and destination pointers to point to the
 *			next row.
 *	endfor
 *
 */
#define copyBitNeg_LOOP(EDGE_OP,OP) {					\
	while (--s_info->height) {					\
		wordnum = s_info->nshorts;				\
		if (s_info->preload_src) {				\
			EDGE_OP((*SRC >> skew_src) |			\
				(*(MINUS_SRC(src)) << (inv_skew_src)),	\
				*DST,s_info->right_mask);		\
			DECR_SRC(src);					\
		}							\
		else {							\
			EDGE_OP((*SRC << (inv_skew_src)),*DST,		\
				s_info->right_mask);			\
		}							\
		if (--wordnum != 0) {					\
			DECR_DST(dst);					\
			if (skew_src & 0xf) {				\
				while (wordnum > 1) {			\
					OP((*SRC >> skew_src) |		\
					   (*(MINUS_SRC(src))<<(inv_skew_src)),\
					   *DST);			\
					DECR_SRC(src);			\
					DECR_DST(dst);			\
					wordnum--;			\
				}					\
				EDGE_OP((*SRC >> skew_src) |		\
					(*(MINUS_SRC(src)) << (inv_skew_src)),\
					*DST,s_info->left_mask);	\
				DECR_SRC(src);				\
			}						\
			else {						\
				while (wordnum > 1) {			\
					DECR_SRC(src);			\
					OP(*SRC,*DST);			\
					DECR_DST(dst);			\
					wordnum--;			\
				}					\
				DECR_SRC(src);				\
				EDGE_OP(*SRC,*DST,s_info->left_mask);	\
			}						\
		}							\
	src -= s_info->src.nextline;					\
	dst -= s_info->dst.nextline;					\
	}								\
}

/*
 * Do setup and dereferencing of 5 of the most important Blt_sysdata variables.
 * Then do a switch on the combination rule to do the correct loop.  Each loop
 * copies from right to left, bottom to top.
 */
copyBitNeg(s_info)
register Blt_sysdata *s_info;
{
	register unsigned char *src; /* Register pointers are declared as  */
				     /* unsigned char pointers. This is an */
	register unsigned char *dst; /* optimization to get around extra   */
				     /* addressing in the assembler.       */
#if (APA8 || APA8C)
	register int src_plus;
	register int dst_plus;
#endif
	register int wordnum;
	register int skew_src;
	register int inv_skew_src;

#if (APA8 || APA8C)
	src_plus = s_info->src_plus;
	dst_plus = s_info->dst_plus;
#endif

	src = (unsigned char *)s_info->src.data;
	dst = (unsigned char *)s_info->dst.data;

	/*
	 * Set up the inverted skew number which is used to shift stuff the
	 * other direction.
	 */
	skew_src = s_info->skew_src;
	inv_skew_src = BPW - skew_src;

	DEBUGF(blt_debug,
		printf("Enter copyBitNeg s:%d,p:%d,ws:%d r:%d\r\n",
			s_info->skew_src,s_info->preload_src,
			s_info->nshorts,s_info->rule));

	/*
	 * Compiler optimization: a pre-decrement in a while loop saves a test
	 * in assembly language for every iteration.
	 */
	s_info->height += 1;
	switch (s_info->rule) {
		case SrcCopy:
		     copyBitNeg_LOOP(SrcCopy_MASK,SrcCopy_OP);
		     break;

		case SrcOr:
		     copyBitNeg_LOOP(SrcOr_MASK,SrcOr_OP);
		     break;

		case SrcXor:
		     copyBitNeg_LOOP(SrcXor_MASK,SrcXor_OP);
		     break;

		case NotSrcAnd:   
		     copyBitNeg_LOOP(NotSrcAnd_MASK,NotSrcAnd_OP);
		     break;

		case NotSrcCopy:
		     copyBitNeg_LOOP(NotSrcCopy_MASK,NotSrcCopy_OP);
		     break;

		case NotSrcOr:
		     copyBitNeg_LOOP(NotSrcOr_MASK,NotSrcOr_OP);
		     break;

		case NotSrcXor:
		     copyBitNeg_LOOP(NotSrcXor_MASK,NotSrcXor_OP);
		     break;

		case SrcAnd:
		     copyBitNeg_LOOP(SrcAnd_MASK,SrcAnd_OP);
		     break;

		case SrcAndNotDst:
		     copyBitNeg_LOOP(SrcAndNotDst_MASK,SrcAndNotDst_OP);
		     break;

		case NotSrcAndNotDst:
		     copyBitNeg_LOOP(NotSrcAndNotDst_MASK,NotSrcAndNotDst_OP);
		     break;

		case SrcOrNotDst:
		     copyBitNeg_LOOP(SrcOrNotDst_MASK,SrcOrNotDst_OP);
		     break;

		case NotSrcOrNotDst:
		     copyBitNeg_LOOP(NotSrcOrNotDst_MASK,NotSrcOrNotDst_OP);
		     break;

		default:
		     break;
	}
}

#if (APA8 || APA8C)
#define I_SRC		src_plus
#define I_DST		dst_plus
#else no APA8 stuff
#define I_SRC		2
#define I_DST		2
#endif

/*
 * This loop copies bits from left to right, top to bottom.  It takes as
 * arguments 1 of 8 masking macros and the coorisponding generic macro.
 * The general scheme is this:
 *	Foreach row do
 *		Take care of the first word priming if necessary. (Done because
 *			the first word uses the LEFT edge mask.)
 *
 *		If there is a skew then (Means parts of 2 source words make one
 *			destination word.)
 *			For each word between the first and last word do
 *				Shift the source word and source word +1 and
 *					write the word to the destination.
 *			endfor
 *
 *			Take care of the last word. (Done here and not in 
 *				the loop because the last word uses the 
 *				RIGHT edge mask.)
 * 
 *		else  The skew is zero meaning 1 source word per destination
 *		      word.
 * 
 *			For each word between the first and last word do
 *				Write the source word into the destination word
 *			endfor
 *
 *			Take care of the last word. (Done here and not in 
 *				the loop because the last word uses the 
 *				RIGHT edge mask.)
 *		endif
 *
 *		INCREMENT the source and destination pointers to point to the
 *			next row.
 *	endfor
 *
 */
#define copyBitPos_LOOP(EDGE_OP,OP) {					\
	while (--s_info->height) {					\
		wordnum = s_info->nshorts;				\
		if (s_info->preload_src) {				\
			EDGE_OP((*SRC << skew_src) |			\
				(*(PLUS_SRC(src)) >> (inv_skew_src)),*DST,\
				s_info->left_mask);			\
			INCR_SRC(src);					\
		}							\
		else {							\
			EDGE_OP((*SRC>>(inv_skew_src)),*DST,s_info->left_mask);\
		}							\
		if (--wordnum != 0) {					\
			INCR_DST(dst);					\
			if (skew_src & 0xf) {				\
				while (wordnum-- > 1) {			\
					OP((*SRC << skew_src) |		\
					  (*(PLUS_SRC(src)) >> (inv_skew_src)),\
					   *DST);			\
					INCR_DST(dst);			\
					INCR_SRC(src);			\
				}					\
				EDGE_OP((*SRC << skew_src) |		\
					(*(PLUS_SRC(src)) >> (inv_skew_src)),\
					*DST,s_info->right_mask);	   \
				INCR_SRC(src);				   \
			}					   	   \
			else {			   			   \
				while (wordnum-- > 1) {			   \
					INCR_SRC(src);			   \
					OP(*SRC,*DST);			   \
					INCR_DST(dst);			   \
				}				   	   \
				INCR_SRC(src);				   \
				EDGE_OP(*SRC,*DST,s_info->right_mask);	   \
			}						   \
		}							   \
		src += s_info->src.nextline;				   \
		dst += s_info->dst.nextline;				   \
	}								   \
}

/*
 * Do setup and dereferencing of 5 of the most important Blt_sysdata variables.
 * Then do a switch on the combination rule to do the correct loop.  Each loop
 * copies from left to right, top to bottom .
 */
copyBitPos(s_info)
register Blt_sysdata *s_info;
{
	register unsigned char *src; /* Register pointers are declared as  */
				     /* unsigned char pointers. This is an */
	register unsigned char *dst; /* optimization to get around extra   */
				     /* addressing in the assembler.       */
#if (APA8 || APA8C)
	register int src_plus;
	register int dst_plus;
#endif
	register int wordnum;
	register int skew_src;
	register int inv_skew_src;

#if (APA8 || APA8C)
	src_plus = s_info->src_plus;
	dst_plus = s_info->dst_plus;
#endif
	src = (unsigned char *)s_info->src.data;
	dst = (unsigned char *)s_info->dst.data;

	skew_src = s_info->skew_src;
	inv_skew_src = BPW - skew_src;

	DEBUGF(blt_debug,
		printf("Enter copyBitPos:%d,%d,%d,%d\r\n",s_info->skew_src,
			inv_skew_src,s_info->preload_src,s_info->rule));

	/*
	 * foreach row do.
	 *
	 * Compiler optimization: a pre-decrement in a while loop saves a test
	 * in assembly language for every iteration.
	 */
	s_info->height += 1;
	switch (s_info->rule) {
		case SrcCopy:
		     copyBitPos_LOOP(SrcCopy_MASK,SrcCopy_OP);
		     break;

		case SrcOr:
		     copyBitPos_LOOP(SrcOr_MASK,SrcOr_OP);
		     break;

		case SrcXor:
		     copyBitPos_LOOP(SrcXor_MASK,SrcXor_OP);
		     break;

		case NotSrcAnd:   
		     copyBitPos_LOOP(NotSrcAnd_MASK,NotSrcAnd_OP);
		     break;

		case NotSrcCopy:
		     copyBitPos_LOOP(NotSrcCopy_MASK,NotSrcCopy_OP);
		     break;

		case NotSrcOr:
		     copyBitPos_LOOP(NotSrcOr_MASK,NotSrcOr_OP);
		     break;

		case NotSrcXor:
		     copyBitPos_LOOP(NotSrcXor_MASK,NotSrcXor_OP);
		     break;

		case SrcAnd:
		     copyBitPos_LOOP(SrcAnd_MASK,SrcAnd_OP);
		     break;

		case SrcAndNotDst:
		     copyBitPos_LOOP(SrcAndNotDst_MASK,SrcAndNotDst_OP);
		     break;

		case NotSrcAndNotDst:
		     copyBitPos_LOOP(NotSrcAndNotDst_MASK,NotSrcAndNotDst_OP);
		     break;

		case SrcOrNotDst:
		     copyBitPos_LOOP(SrcOrNotDst_MASK,SrcOrNotDst_OP);
		     break;

		case NotSrcOrNotDst:
		     copyBitPos_LOOP(NotSrcOrNotDst_MASK,NotSrcOrNotDst_OP);
		     break;

		default:
		     break;
	}
}

#define MYDEBUG_PR(n,s)
#define smallcopyBitPos_LOOP(EDGE_OP,OP) {				\
	if (s_info->preload_src) {					\
		if (s_info->nshorts == 2) {				\
			MYDEBUG_PR(blt_debug,printf("preload 2\r\n"));	\
			while (--height) {				\
				EDGE_OP((*SRC << skew_src) |		\
					(*(PLUS_SRC(src)) >> (inv_skew_src)),\
                                        *DST,s_info->left_mask);	\
				INCR_SRC(src);				\
				INCR_DST(dst);				\
				EDGE_OP((*SRC << skew_src) |		\
					(*(PLUS_SRC(src)) >> (inv_skew_src)),\
					*DST,s_info->right_mask);	\
				src += (s_info->src.nextline + I_SRC);	\
				dst += s_info->dst.nextline;		\
			}						\
		}							\
		else {							\
			MYDEBUG_PR(blt_debug,printf("preload 1\r\n"));	\
			while (--height) {				\
				EDGE_OP((*SRC << skew_src) |		\
					(*(PLUS_SRC(src)) >> (inv_skew_src)),\
                                        *DST,s_info->left_mask);	\
				src += (s_info->src.nextline + I_SRC);	\
				dst += s_info->dst.nextline;		\
			}						\
		}							\
	}								\
	else if (skew_src & 0xf) {					\
		if (s_info->nshorts == 2) {				\
			MYDEBUG_PR(blt_debug,printf("skew_src 2\r\n"));	\
			while (--height) {				\
				EDGE_OP((*SRC>>(inv_skew_src)),		\
					*DST,s_info->left_mask);	\
				INCR_DST(dst);				\
				EDGE_OP((*SRC << skew_src) |		\
					(*(PLUS_SRC(src)) >> (inv_skew_src)),\
					*DST,s_info->right_mask);	\
				src += (s_info->src.nextline + I_SRC);	\
				dst += s_info->dst.nextline;		\
			}						\
		}							\
		else {							\
			MYDEBUG_PR(blt_debug,printf("skew_src 1\r\n"));	\
			while (--height) {				\
				EDGE_OP((*SRC>>(inv_skew_src)),		\
					*DST,s_info->left_mask);	\
				src += s_info->src.nextline;		\
				dst += s_info->dst.nextline;		\
			}						\
		}							\
	}								\
	else if (s_info->nshorts == 2) {				\
		MYDEBUG_PR(blt_debug,printf("lines up 2\r\n"));		\
		while (--height) {					\
			EDGE_OP(*SRC,*DST,s_info->left_mask);		\
			INCR_DST(dst);					\
			INCR_SRC(src);					\
			EDGE_OP(*SRC,*DST,s_info->right_mask);		\
			src += s_info->src.nextline;			\
			dst += s_info->dst.nextline;			\
		}							\
	}								\
	else {								\
		MYDEBUG_PR(blt_debug,printf("lines up 1\r\n"));		\
		while (--height) {					\
			EDGE_OP(*SRC,*DST,s_info->left_mask);		\
			src += s_info->src.nextline;			\
			dst += s_info->dst.nextline;			\
		}							\
	}								\
}

/*
 * Do setup and dereferencing of 5 of the most important Blt_sysdata variables.
 * Then do a switch on the combination rule to do the correct loop.  Each loop
 * copies from left to right, top to bottom .
 */
smallcopyBitPos(s_info)
register Blt_sysdata *s_info;
{
	register unsigned char *src; /* Register pointers are declared as  */
				     /* unsigned char pointers. This is an */
	register unsigned char *dst; /* optimization to get around extra   */
				     /* addressing in the assembler.       */
#if (APA8 || APA8C)
	register int src_plus;
	register int dst_plus;
#endif
	register long height;
	register int skew_src;
	register int inv_skew_src;

#if (APA8 || APA8C)
	src_plus = s_info->src_plus;
	dst_plus = s_info->dst_plus;
#endif
	src = (unsigned char *)s_info->src.data;
	dst = (unsigned char *)s_info->dst.data;

	height = s_info->height;
	skew_src = s_info->skew_src;
	inv_skew_src = BPW - skew_src;

	DEBUGF(blt_debug,
		printf("Enter smallcopyBitPos:%d,%d,%d,%d\r\n",skew_src,
			inv_skew_src,s_info->preload_src,s_info->rule));

	/*
	 * foreach row do.
	 *
	 * Compiler optimization: a pre-decrement in a while loop saves a test
	 * in assembly language for every iteration.
	 */
	height += 1;
	switch (s_info->rule) {
		case SrcCopy:
		     smallcopyBitPos_LOOP(SrcCopy_MASK,SrcCopy_OP);
		     break;

		case SrcOr:
		     smallcopyBitPos_LOOP(SrcOr_MASK,SrcOr_OP);
		     break;

		case SrcXor:
		     smallcopyBitPos_LOOP(SrcXor_MASK,SrcXor_OP);
		     break;

		case NotSrcAnd:   
		     smallcopyBitPos_LOOP(NotSrcAnd_MASK,NotSrcAnd_OP);
		     break;

		case NotSrcCopy:
		     smallcopyBitPos_LOOP(NotSrcCopy_MASK,NotSrcCopy_OP);
		     break;

		case NotSrcOr:
		     smallcopyBitPos_LOOP(NotSrcOr_MASK,NotSrcOr_OP);
		     break;

		case NotSrcXor:
		     smallcopyBitPos_LOOP(NotSrcXor_MASK,NotSrcXor_OP);
		     break;

		case SrcAnd:
		     smallcopyBitPos_LOOP(SrcAnd_MASK,SrcAnd_OP);
		     break;

		case SrcAndNotDst:
		     smallcopyBitPos_LOOP(SrcAndNotDst_MASK,SrcAndNotDst_OP);
		     break;

		case NotSrcAndNotDst:
		     smallcopyBitPos_LOOP(NotSrcAndNotDst_MASK,NotSrcAndNotDst_OP);
		     break;

		case SrcOrNotDst:
		     smallcopyBitPos_LOOP(SrcOrNotDst_MASK,SrcOrNotDst_OP);
		     break;

		case NotSrcOrNotDst:
		     smallcopyBitPos_LOOP(NotSrcOrNotDst_MASK,NotSrcOrNotDst_OP);
		     break;

		default:
		     break;
	}
}

/*
 * This loop copies the tile to the destination. It takes as
 * arguments 1 of 8 masking tile macros and the coorisponding generic
 * tile  macro. The general scheme is this:
 *	Foreach row do
 *		Copy the tile to the first word using the left edge mask.
 *
 *		For each word between the first and last word do
 *			Copy the tile to the destination word.
 *		endfor
 *
 *		Copy the tile to the first word using the right edge mask.
 *
 *		INCREMENT the tile and destination pointers to point to the
 *			next row.
 *	endfor
 *
 */
#define copyTile_LOOP(EDGE_OP,OP) {			\
	while (--height) {				\
		wordnum = s_info->nshorts;		\
		EDGE_OP(*TILE,*DST,s_info->left_mask);	\
		if (--wordnum != 0) {			\
			INCR_DST(dst);			\
			while (wordnum > 1) {		\
				OP(*TILE,*DST);		\
				INCR_DST(dst);	 	\
				wordnum--;		\
			}				\
			EDGE_OP(*TILE,*DST,s_info->right_mask);\
		}					\
		INCR_TILE_PTR(tile,last_tile);		\
		dst += s_info->dst.nextline;		\
	}						\
}

/*
 * Tiles the destination.
 */
copyTile(s_info)
register Blt_sysdata *s_info;
{
	/*
	 * For optimization, these registers are placed in order of importance.
	 */
	register unsigned char *dst; /* All register pointers are declared as */
				     /* unsigned char pointers. This is an    */
				     /* optimization to get around extra      */
				     /* addressing in the assembler.          */
	register unsigned char *tile;
#if (APA8 || APA8C)
	register int dst_plus;
#endif
	register int wordnum;
	register unsigned char *last_tile;
	register long height;

#if (APA8 || APA8C)
	DEBUGF(blt_debug,
	 	printf("Enter copyTile r:%d p:%d, i:%d dw:%d\r\n",s_info->rule,
			s_info->dst_plus,s_info->dst.nextline,s_info->nshorts));
#else
	DEBUGF(blt_debug,
	 	printf("Enter copyTile r:%d i:%d dw:%d\r\n",s_info->rule,
			s_info->dst.nextline,s_info->nshorts));
#endif

	dst = (unsigned char *)s_info->dst.data;
	tile = (unsigned char *)&s_info->tile_ptr[
					    MOD_BPW(s_info->dst.rect.origin_y)];
	last_tile = (unsigned char *)&s_info->tile_ptr[15];

#if (APA8 || APA8C)
	dst_plus = s_info->dst_plus;
#endif

	/*
	 * Compiler optimization: a pre-decrement in a while loop saves a test
	 * in assembly language for every iteration.
	 */
	height = s_info->height + 1;
	switch (s_info->rule) {
		case DstClear:
		case DstSet:
		case TileDstClear:
		case TileDstSet:
		case TileCopy:
		     copyTile_LOOP(TileCopy_MASK,TileCopy_OP);
		     break;

		case TileOr:
		     copyTile_LOOP(TileOr_MASK,TileOr_OP);
		     break;

		case TileXor:
		     copyTile_LOOP(TileXor_MASK,TileXor_OP);
		     break;

		case NotTileAnd:
		     copyTile_LOOP(NotTileAnd_MASK,NotTileAnd_OP);
		     break;

		case NotTileCopy:
		     copyTile_LOOP(NotTileCopy_MASK,NotTileCopy_OP);
		     break;

		case NotTileOr:
		     copyTile_LOOP(NotTileOr_MASK,NotTileOr_OP);
		     break;

		case NotTileXor:
		     copyTile_LOOP(NotTileXor_MASK,NotTileXor_OP);
		     break;

		case TileAnd:
		     copyTile_LOOP(TileAnd_MASK,TileAnd_OP);
		     break;

		case TileNotDstCopy:
		case NotDstCopy:
		     copyTile_LOOP(NotDstCopy_MASK,NotDstCopy_OP);
		     break;

		case TileAndNotDst:
		     copyTile_LOOP(TileAndNotDst_MASK,TileAndNotDst_OP);
		     break;

		case NotTileAndNotDst:
		     copyTile_LOOP(NotTileAndNotDst_MASK,NotTileAndNotDst_OP);
		     break;

		case TileOrNotDst:
		     copyTile_LOOP(TileOrNotDst_MASK,TileOrNotDst_OP);
		     break;

		case NotTileOrNotDst:
		     copyTile_LOOP(NotTileOrNotDst_MASK,NotTileOrNotDst_OP);
		     break;

		default:
		     break;
	}
}

/*
 * This loop copies bits from right to left, bottom to top.  It takes as
 * arguments 1 of 8 masking macros.  This loop has the same general structure
 * as copyBitNeg_LOOP except that work must be done on the mask bitmap.
 * This adds complexity because for optimization purposes, there are 4 loops
 * instead of 2.
 * The general scheme is this:
 *	Foreach row do
 *		Calculate the first MASK word priming if necessary.
 *
 *		Take care of the first word priming if necessary and using
 *			the mask just calculated. (Done because
 *			the first word uses the RIGHT edge mask.)
 *
 *		If the source and the MASK is skewed then
 *			(Means parts of 2 source words make one
 *			destination word and parts of 2 mask words are used
 *			to mask this one destination word.)
 *
 *			For each word between the first and last word do
 *				Calculate the MASK word.
 *				Shift the source word and source word -1 and
 *					write the word to the destination using
 *					the mask just calculated.
 *			endfor
 *
 *			Calculate the MASK word.
 *			Take care of the last word using the mask and left
 * 				edge mask. (Done here and not in 
 *				the loop because the last word uses the 
 *				LEFT edge mask.)
 * 
 *		else if the MASK is skewed then
 *			For each word between the first and last word do
 *				Calculate the MASK word.
 *				Write the source word to the destination using
 *					the mask just calculated.
 *			endfor
 *
 *			Calculate the MASK word.
 *			Take care of the last word using the mask and left
 * 				edge mask.
 *			
 *		else if the source is skewed then
 *
 *			For each word between the first and last word do
 *				Shift the source word and source word -1 and
 *					write the word to the destination using
 *					the mask.
 *			endfor
 *
 *			Take care of the last word using the mask and left
 * 				edge mask.
 *
 *		else  The source and mask skew are zero meaning 1 source word,
 *		      one mask word are used per destination word.
 * 
 *			For each word between the first and last word do
 *				Write the source word into the destination
 *					using the mask word.
 *			endfor
 *
 *			Take care of the last word using the mask word and
 *				the left edge mask.
 *		endif
 *
 *		DECREMENT the source,destination and mask pointers to point
 *			to the next row in their respective bitmaps.
 *	endfor
 *
 */
#define maskBitNeg_LOOP(MASK_OP) {					\
	while (--s_info->height) {					\
		wordnum = s_info->nshorts;				\
		if (s_info->preload_msk) {				\
			mask = (*MSK >> s_info->skew_msk) |		\
			       (*MINUS_MSK(msk) << inv_skew_msk);	\
			DECR_MSK(msk);					\
		}							\
		else {							\
			mask = (*MSK << inv_skew_msk);			\
		}							\
		if (s_info->preload_src) {				\
			MASK_OP((*SRC >> s_info->skew_src) |		\
				(*(MINUS_SRC(src)) << (inv_skew_src)),	\
				*DST,s_info->right_mask & mask);	\
			DECR_SRC(src);					\
		}							\
		else {							\
			MASK_OP((*SRC << (inv_skew_src)),*DST,		\
				s_info->right_mask & mask);		\
		}							\
		if (--wordnum != 0) {					\
			DECR_DST(dst);					\
			if (write_mode == (MSK_SKEW | SRC_SKEW)) {	\
				while (wordnum-- > 1) {			\
					mask = (*MSK >> s_info->skew_msk) |\
					       (*MINUS_MSK(msk)<<inv_skew_msk);\
					MASK_OP((*SRC >> s_info->skew_src) |\
					        (*(MINUS_SRC(src)) <<	\
						inv_skew_src),		\
						*DST,mask);		\
					DECR_SRC(src); DECR_MSK(msk);	\
					DECR_DST(dst);			\
				}					\
				mask = (*MSK >> s_info->skew_msk) |	\
				       (*MINUS_MSK(msk)<<inv_skew_msk);	\
				MASK_OP((*SRC >> s_info->skew_src) |	\
					(*(MINUS_SRC(src))<<inv_skew_src),\
					*DST,s_info->left_mask & mask);  \
				DECR_SRC(src); DECR_MSK(msk);		\
			}						\
			else if (write_mode & MSK_SKEW) {		\
				while (wordnum-- > 1) {			\
					mask = (*MSK >> s_info->skew_msk) |\
					       (*MINUS_MSK(msk)<<inv_skew_msk);\
					DECR_SRC(src); DECR_MSK(msk);	\
					MASK_OP(*SRC,*DST,mask);	\
					DECR_DST(dst);			\
				}					\
				mask = (*MSK >> s_info->skew_msk) |	\
				       (*MINUS_MSK(msk)<<inv_skew_msk);	\
				DECR_SRC(src); DECR_MSK(msk);		\
				MASK_OP(*SRC,*DST,s_info->left_mask&mask);\
			}						\
			else if (write_mode & SRC_SKEW) {		\
				while (wordnum-- > 1) {			\
					DECR_MSK(msk);			\
		  			MASK_OP((*SRC >> s_info->skew_src) |\
					        (*(MINUS_SRC(src)) <<	\
						inv_skew_src),		\
					        *DST,*MSK);		\
		    			DECR_SRC(src);			\
					DECR_DST(dst);			\
				}					\
				DECR_MSK(msk);				\
				MASK_OP((*SRC >> s_info->skew_src) |	\
					(*(MINUS_SRC(src))<<inv_skew_src),\
					*DST,s_info->left_mask & *MSK);	\
				DECR_SRC(src);				\
			}						\
			else {						\
				while (wordnum-- > 1) {			\
					DECR_SRC(src); DECR_MSK(msk);	\
					MASK_OP(*SRC,*DST,*MSK);	\
					DECR_DST(dst);			\
				}					\
				DECR_SRC(src); DECR_MSK(msk);		\
				MASK_OP(*SRC,*DST,s_info->left_mask & *MSK);\
			}						\
		}							\
		src -= s_info->src.nextline;				\
		dst -= s_info->dst.nextline;				\
		msk -= s_info->msk.nextline;				\
	}								\
}

/*
 * Copies bits from bottom to top, right to left but only changes those bits
 * that have corrisponding ones in the mask bitmap.
 */
maskBitNeg(s_info)
register Blt_sysdata *s_info;
{
	register unsigned char *src; /* Register pointers are declared as  */
				     /* unsigned char pointers. This is an */
	register unsigned char *dst; /* optimization to get around extra   */
				     /* addressing in the assembler.       */
	register unsigned char *msk; /* register pointer to the mask */
#if (APA8 || APA8C)
	register int src_plus;
	register int dst_plus;
#endif
	register int wordnum;	     /* number of shorts per scanline */
	register int inv_skew_src;   /* BPW - skew_src */
	register int inv_skew_msk;   /* BPW - skew_msk */
	register int write_mode;     /* One of 4 modes: 0. no skew.
							1. skew just the msk.
							2. skew just the src.
							3. skew both. */
	register unsigned short mask;/* The local mask produced by shifting
					*MSK and *MINUS_MASK() */

#if (APA8 || APA8C)
	src_plus = s_info->src_plus;
	dst_plus = s_info->dst_plus;
#endif

	DEBUGF(blt_debug,
	 	printf("Enter maskBitNeg. rule:%d\r\n",s_info->rule));

	src = (unsigned char *)s_info->src.data;
	dst = (unsigned char *)s_info->dst.data;
	msk = (unsigned char *)s_info->msk.data;

	inv_skew_src = BPW - s_info->skew_src;
	inv_skew_msk = BPW - s_info->skew_msk;

	/*
	 * Set the write mode so that the correct inner loop is picked.
	 */
	write_mode = 0;
	if (s_info->skew_src & 0xf)
		write_mode |= SRC_SKEW;
	if (s_info->skew_msk & 0xf)
		write_mode |= MSK_SKEW;

	/*
	 * Compiler optimization: a pre-decrement in a while loop saves a test
	 * in assembly language for every iteration.
	 */
	s_info->height += 1;
	switch (s_info->rule) {
		case SrcCopy:
		     maskBitNeg_LOOP(SrcCopy_MASK);
		     break;

		case SrcOr:
		     maskBitNeg_LOOP(SrcOr_MASK);
		     break;

		case SrcXor:
		     maskBitNeg_LOOP(SrcXor_MASK);
		     break;

		case NotSrcAnd:   
		     maskBitNeg_LOOP(NotSrcAnd_MASK);
		     break;

		case NotSrcCopy:
		     maskBitNeg_LOOP(NotSrcCopy_MASK);
		     break;

		case NotSrcOr:
		     maskBitNeg_LOOP(NotSrcOr_MASK);
		     break;

		case NotSrcXor:
		     maskBitNeg_LOOP(NotSrcXor_MASK);
		     break;

		case SrcAnd:
		     maskBitNeg_LOOP(SrcAnd_MASK);
		     break;

		case SrcAndNotDst:
		     maskBitNeg_LOOP(SrcAndNotDst_MASK);
		     break;

		case NotSrcAndNotDst:
		     maskBitNeg_LOOP(NotSrcAndNotDst_MASK);
		     break;

		case SrcOrNotDst:
		     maskBitNeg_LOOP(SrcOrNotDst_MASK);
		     break;

		case NotSrcOrNotDst:
		     maskBitNeg_LOOP(NotSrcOrNotDst_MASK);
		     break;

		default:
		     break;
	}
}

/*
 * This loop copies bits from left to right, top to bottom.  It takes as
 * arguments 1 of 8 masking macros.  This loop has the same general structure
 * as copyBitPos_LOOP except that work must be done on the mask bitmap.
 * This adds complexity because, for optimization purposes, there are 4 loops
 * instead of 2.
 * The general scheme is this:
 *	Foreach row do
 *		Calculate the first MASK word priming if necessary.
 *
 *		Take care of the first word priming if necessary and using
 *			the mask just calculated. (Done because
 *			the first word uses the RIGHT edge mask.)
 *
 *		If the source and the MASK is skewed then
 *			(Means parts of 2 source words make one
 *			destination word and parts of 2 mask words are used
 *			to mask this one destination word.)
 *
 *			For each word between the first and last word do
 *				Calculate the MASK word.
 *				Shift the source word and source word -1 and
 *					write the word to the destination using
 *					the mask just calculated.
 *			endfor
 *
 *			Calculate the MASK word.
 *			Take care of the last word using the mask and left
 * 				edge mask. (Done here and not in 
 *				the loop because the last word uses the 
 *				LEFT edge mask.)
 * 
 *		else if the MASK is skewed then
 *			For each word between the first and last word do
 *				Calculate the MASK word.
 *				Write the source word to the destination using
 *					the mask just calculated.
 *			endfor
 *
 *			Calculate the MASK word.
 *			Take care of the last word using the mask and left
 * 				edge mask.
 *			
 *		else if the source is skewed then
 *
 *			For each word between the first and last word do
 *				Shift the source word and source word -1 and
 *					write the word to the destination using
 *					the mask.
 *			endfor
 *
 *			Take care of the last word using the mask and left
 * 				edge mask.
 *
 *		else  The source and mask skew are zero meaning 1 source word,
 *		      one mask word are used per destination word.
 * 
 *			For each word between the first and last word do
 *				Write the source word into the destination
 *					using the mask word.
 *			endfor
 *
 *			Take care of the last word using the mask word and
 *				the left edge mask.
 *		endif
 *
 *		INCREMENT the source,destination and mask pointers to point
 *			to the next row in their respective bitmaps.
 *	endfor
 *
 */
#define maskBitPos_LOOP(MASK_OP) {					\
	while (--s_info->height) {					\
		wordnum = s_info->nshorts;				\
		if (s_info->preload_msk) {				\
			mask = (*MSK << s_info->skew_msk) |		\
			       (*PLUS_MSK(msk) >> inv_skew_msk);	\
			INCR_MSK(msk);					\
		}							\
		else {							\
			mask = (*MSK >> inv_skew_msk);			\
		}							\
		if (s_info->preload_src) {				\
			MASK_OP((*SRC << s_info->skew_src) |		\
				(*(PLUS_SRC(src)) >> (inv_skew_src)),*DST,\
			        s_info->left_mask & mask);		\
			INCR_SRC(src);					\
		}							\
		else {							\
			 MASK_OP((*SRC >> (inv_skew_src)),*DST,		\
				s_info->left_mask & mask);		\
		}							\
		if (--wordnum != 0) {					\
			INCR_DST(dst);					\
			if (write_mode == (MSK_SKEW | SRC_SKEW)) {	\
				while (wordnum-- > 1) {			\
					mask = (*MSK << s_info->skew_msk) |\
					       (*PLUS_MSK(msk)>>inv_skew_msk);\
					MASK_OP((*SRC << s_info->skew_src) |\
						(*(PLUS_SRC(src)) >>	\
						(inv_skew_src)),	\
						*DST,mask);		\
					INCR_SRC(src); INCR_MSK(msk);	\
					INCR_DST(dst);			\
				}					\
				mask = (*MSK << s_info->skew_msk) |	\
				       (*PLUS_MSK(msk) >> inv_skew_msk);\
				MASK_OP((*SRC << s_info->skew_src) |	\
					(*(PLUS_SRC(src))>>(inv_skew_src)),\
					*DST, s_info->right_mask & mask);\
				INCR_SRC(src); INCR_MSK(msk);		\
			}						\
			else if (write_mode & MSK_SKEW) {		\
				while (wordnum-- > 1) {			\
					mask = (*MSK << s_info->skew_msk) |\
					       (*PLUS_MSK(msk) >>	\
						   inv_skew_msk);	\
					INCR_SRC(src); INCR_MSK(msk);	\
					MASK_OP(*SRC,*DST,mask);	\
					INCR_DST(dst);			\
				}					\
				mask = (*MSK << s_info->skew_msk) |	\
					  (*PLUS_MSK(msk) >> inv_skew_msk);\
				INCR_SRC(src); INCR_MSK(msk);		\
				MASK_OP(*SRC,*DST, s_info->right_mask & mask);\
			}						\
			else if (write_mode & SRC_SKEW) {		\
				while (wordnum-- > 1) {			\
					INCR_MSK(msk);			\
					MASK_OP((*SRC << s_info->skew_src) |\
						(*(PLUS_SRC(src)) >>	\
						(inv_skew_src)),	\
						*DST,*MSK);		\
					INCR_SRC(src);			\
					INCR_DST(dst);			\
				}					\
				INCR_MSK(msk);				\
				MASK_OP((*SRC << s_info->skew_src) |	\
					(*(PLUS_SRC(src))>>(inv_skew_src)),\
					*DST, s_info->right_mask & *MSK);\
					INCR_SRC(src);			\
			}						\
			else {						\
				while (wordnum-- > 1) {			\
					INCR_SRC(src); INCR_MSK(msk);	\
			        	MASK_OP(*SRC,*DST,*MSK);	\
			        	INCR_DST(dst);			\
				}					\
				INCR_SRC(src); INCR_MSK(msk);		\
				MASK_OP(*SRC,*DST,s_info->right_mask & *MSK);\
			}						\
		}							\
		src += s_info->src.nextline;				\
		dst += s_info->dst.nextline;				\
		msk += s_info->msk.nextline;				\
	}								\
}

/*
 * Copies bits from top to bottom , left to right but only changes those bits
 * that have corrisponding ones in mask bitmap given.
 */
maskBitPos(s_info)
register Blt_sysdata *s_info;
{
	register unsigned char *src; /* Register pointers are declared as  */
				     /* unsigned char pointers. This is an */
	register unsigned char *dst; /* optimization to get around extra   */
				     /* addressing in the assembler.       */
	register unsigned char *msk; /* register pointer to the mask */
#if (APA8 || APA8C)
	register int src_plus;
	register int dst_plus;
#endif
	register int wordnum;	     /* number of shorts per scanline */
	register int inv_skew_src;   /* BPW - skew_src */
	register int inv_skew_msk;   /* BPW - skew_msk */
	register int write_mode;     /* One of 4 modes: 0. no skew.
							1. skew just the msk.
							2. skew just the src.
							3. skew both. */
	register unsigned short mask;/* The local mask produced by shifting
					*MSK and *PLUS_MASK() */

#if (APA8 || APA8C)
	src_plus = s_info->src_plus;
	dst_plus = s_info->dst_plus;
#endif

	DEBUGF(blt_debug,printf("Enter maskBitPos. rule:%d\r\n",s_info->rule));

	src = (unsigned char *)s_info->src.data;
	dst = (unsigned char *)s_info->dst.data;
	msk = (unsigned char *)s_info->msk.data;

	inv_skew_msk = BPW - s_info->skew_msk;
	inv_skew_src = BPW - s_info->skew_src;

	/*
	 * Set the write mode so that the correct inner loop is picked.
	 */
	write_mode = 0;
	if (s_info->skew_src & 0xf)
		write_mode |=  SRC_SKEW;
	if (s_info->skew_msk & 0xf)
		write_mode |= MSK_SKEW;

	/*
	 * Compiler optimization: a pre-decrement in a while loop saves a test
	 * in assembly language for every iteration.
	 */
	s_info->height += 1;
	switch (s_info->rule) {
		case SrcCopy:
		     maskBitPos_LOOP(SrcCopy_MASK);
		     break;

		case SrcOr:
		     maskBitPos_LOOP(SrcOr_MASK);
		     break;

		case SrcXor:
		     maskBitPos_LOOP(SrcXor_MASK);
		     break;

		case NotSrcAnd:   
		     maskBitPos_LOOP(NotSrcAnd_MASK);
		     break;

		case NotSrcCopy:
		     maskBitPos_LOOP(NotSrcCopy_MASK);
		     break;

		case NotSrcOr:
		     maskBitPos_LOOP(NotSrcOr_MASK);
		     break;

		case NotSrcXor:
		     maskBitPos_LOOP(NotSrcXor_MASK);
		     break;

		case SrcAnd:
		     maskBitPos_LOOP(SrcAnd_MASK);
		     break;

		case SrcAndNotDst:
		     maskBitPos_LOOP(SrcAndNotDst_MASK);
		     break;

		case NotSrcAndNotDst:
		     maskBitPos_LOOP(NotSrcAndNotDst_MASK);
		     break;

		case SrcOrNotDst:
		     maskBitPos_LOOP(SrcOrNotDst_MASK);
		     break;

		case NotSrcOrNotDst:
		     maskBitPos_LOOP(NotSrcOrNotDst_MASK);
		     break;

		default:
		     break;
	}
}

/*
 * This loop copies the tile to the destination only changing those bits
 * in the destination that have coorisponding one's in the mask bitmap.
 * It takes as arguments 1 of 8 masking tile macros. The general scheme
 * is this:
 *	Foreach row do
 *		Calculate the MASK shifting if necessary.
 *		Copy the tile to the first word using the left edge mask and
 *		MASK.
 *
 *		if the MASK is skewed then (2 mask words are needed for one
 *			destination word.)
 *			For each word between the first and last word do
 *				Calculate the MASK.
 *				Copy the tile to the destination word using
 *					the mask calculated.
 *			endfor
 *
 *		Calculate the MASK.
 *		Copy the tile to the first word using the right edge mask and
 *			the mask calculated.
 *
 *		else
 *			For each word between the first and last word do
 *				Copy the tile to the destination word using
 *					the mask.
 *			endfor
 *
 *			Copy the tile to the first word using the
 *				right edge mask and the mask.
 *		endif
 *
 *		INCREMENT the tile and destination pointers to point to the
 *			next row.
 *	endfor
 */
#define maskTile_LOOP(MASK_OP) {					\
	while (--s_info->height) {					\
		wordnum = s_info->nshorts;				\
		if (s_info->preload_msk) {				\
			mask = (*MSK << s_info->skew_msk) |		\
			       (*PLUS_MSK(msk) >> inv_skew_msk);	\
			INCR_MSK(msk);					\
		}							\
		else {							\
			mask = (*MSK >> inv_skew_msk);			\
		}							\
		MASK_OP(*TILE,*DST,s_info->left_mask & mask);		\
		if (--wordnum != 0) {					\
			INCR_DST(dst);					\
			if (s_info->skew_msk & 0xf) {			\
				while (wordnum-- > 1) {			\
					mask = (*MSK << s_info->skew_msk) |\
					       (*PLUS_MSK(msk)>>inv_skew_msk);\
			        	MASK_OP(*TILE,*DST,mask);	\
					INCR_DST(dst); INCR_MSK(msk);	\
				}					\
				mask = (*MSK << s_info->skew_msk) |	\
				       (*PLUS_MSK(msk) >> inv_skew_msk);\
				MASK_OP(*TILE,*DST,			\
					s_info->right_mask & mask);	\
				INCR_MSK(msk);				\
			}						\
			else {						\
				while (wordnum-- > 1) {			\
					INCR_MSK(msk);			\
					MASK_OP(*TILE,*DST,*MSK);	\
					INCR_DST(dst);			\
				}					\
				INCR_MSK(msk);				\
				MASK_OP(*TILE,*DST,s_info->right_mask & *MSK);\
			}						\
		}							\
		INCR_TILE_PTR(tile,last_tile);				\
		dst += s_info->dst.nextline;				\
		msk += s_info->msk.nextline;				\
	}								\
}

/*
 * Copies a tile to the destination but only changes those bits
 * that have corrisponding ones in the mask bitmap given.
 */
maskTile(s_info)
register Blt_sysdata *s_info;
{
	register unsigned char *dst; /* Register pointers are declared as  */
				     /* unsigned char pointers. This is an */
	register unsigned char *msk; /* optimization to get around extra   */
				     /* addressing in the assembler.       */
	register unsigned char *tile;/* register pointer to the tile */
#if (APA8 || APA8C)
	register int src_plus;
	register int dst_plus;
#endif
	register unsigned char *last_tile;/* points to the last word in a tile*/
	register int wordnum;	     /* number of shorts per scanline */
	register int inv_skew_msk;   /* BPW - skew_msk */
	register unsigned short mask;/* The local mask produced by shifting
					*MSK and *PLUS_MASK() */
#if (APA8 || APA8C)
	src_plus = s_info->src_plus;
	dst_plus = s_info->dst_plus;
#endif
	dst = (unsigned char *)s_info->dst.data;
	msk = (unsigned char *)s_info->msk.data;
	tile = (unsigned char *)&s_info->tile_ptr[
					    MOD_BPW(s_info->dst.rect.origin_y)];
	last_tile = (unsigned char *)(&s_info->tile_ptr[15]);


	inv_skew_msk = BPW - s_info->skew_msk;

	DEBUGF(blt_debug,
	 	printf("Enter maskTile%d,%d %d %s %d\r\n",s_info->skew_msk,
			inv_skew_msk,s_info->preload_msk,
			((s_info->tile_ptr[0] == 0xffff &&
			 s_info->tile_ptr[5] == 0xffff) ?
			"black" : "not black"),s_info->rule));

	/*
	 * Compiler optimization: a pre-decrement in a while loop saves a test
	 * in assembly language for every iteration.
	 */
	s_info->height += 1;
	switch (s_info->rule) {
		case DstClear:
		case DstSet:
		case TileDstClear:
		case TileDstSet:
		case TileCopy:
		     maskTile_LOOP(TileCopy_MASK);
		     break;

		case TileOr:
		     maskTile_LOOP(TileOr_MASK);
		     break;

		case TileXor:
		     maskTile_LOOP(TileXor_MASK);
		     break;

		case NotTileAnd:
		     maskTile_LOOP(NotTileAnd_MASK);
		     break;

		case NotTileCopy:
		     maskTile_LOOP(NotTileCopy_MASK);
		     break;

		case NotTileOr:
		     maskTile_LOOP(NotTileOr_MASK);
		     break;

		case NotTileXor:
		     maskTile_LOOP(NotTileXor_MASK);
		     break;

		case TileAnd:
		     maskTile_LOOP(TileAnd_MASK);
		     break;

		case TileNotDstCopy:
		case NotDstCopy:
		     maskTile_LOOP(NotDstCopy_MASK);
		     break;

		case TileAndNotDst:
		     maskTile_LOOP(TileAndNotDst_MASK);
		     break;

		case NotTileAndNotDst:
		     maskTile_LOOP(NotTileAndNotDst_MASK);
		     break;

		case TileOrNotDst:
		     maskTile_LOOP(TileOrNotDst_MASK);
		     break;

		case NotTileOrNotDst:
		     maskTile_LOOP(NotTileOrNotDst_MASK);
		     break;

		default:
		     break;
	}
}
