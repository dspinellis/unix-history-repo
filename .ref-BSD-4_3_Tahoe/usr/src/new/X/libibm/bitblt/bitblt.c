#ifndef lint
static char *rcsid_bitblt_c = "$Header: bitblt.c,v 10.1 86/11/19 10:50:19 jg Exp $";
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
 * Written by Daniel Stone, Brown University/IRIS November 21, 1985 and
 * "enhanced" a number of times since.
 *
 * ABOUT BITBLT.C:  This program and related 'C' files (bitblt_int.h,
 * bitblt_subr.c, bitblt_apa16.c, bitblt_apa16.h) were originally written to go
 * into the Unix 4.2 kernel as a "bitblt" driver.  Later we were able
 * to put in some ifdef's and use it for Smalltalk and X.  Whatever
 * version you have, there may be some strange ifdef's that may not
 * make any sense at all.  We tried to remove all ifdef's not relevant
 * to the system being compiled but then no one's perfect.
 * Also some strange things are done to support the APA8, APA8C and the AED
 * (viking).  You will encounter ifdef's for all these things.  if you define
 * all the screens (-DAED -DAPA8 -DAPA16 ... on the compile line) the bitblt
 * routine will work on every screen.
 */

#include "bitblt_int.h"

#ifdef USE_APA16_HDWR
#include "bitblt_apa16.h"
#endif

#ifdef BLT_DEBUG
int blt_debug = 0;
#endif

#define MSK_SKEW 0x01
#define SRC_SKEW 0x02

Blt_sysdata blt_info;		/* internal variables used by bitblt() */
Blt_Rectangle changed_rect;	/* describes the area changed by the bltter */

/*
 * Array of short masks used to compute the masks to mask out the edges
 * of the destination that should not be affected.  A one means that bit
 * in the destination can be changed.  The values in leftMasks are set up 
 * to be used with the left edge and the values in rightMasks are to be used
 * in right edge. 
 */
static unsigned short leftMasks[] = {
	0xffff,0x7fff,0x3fff,0x1fff,0x0fff,0x07ff,0x03ff,0x01ff,
	0x00ff,0x007f,0x003f,0x001f,0x000f,0x0007,0x0003,0x0001,
	0x0000
};

static unsigned short rightMasks[] = {
	0xffff,0x8000,0xc000,0xe000,0xf000,0xf800,0xfc00,0xfe00,
	0xff00,0xff80,0xffc0,0xffe0,0xfff0,0xfff8,0xfffc,0xfffe,
	0x0000
};

static Blt_Tile white = {
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000
};
static Blt_Tile black = {
	0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff,
	0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff
};
 
/*
 * Defines used to indicate whether 1 or 2 source words are needed to make
 * the first destination word.
 * NOTE: Changing the value of MUST_PRIME may have bad side effects because
 *	 the variable "preload_src" is added to something in calculateOffsets.
 *	 THIS IS SEMI-HOOKY BUT IT WORKS.
 */
#define MUST_PRIME 1
#define DONT_PRIME 0

#define SAVE_LOCATOR() {						\
	if (blt_cur_screen.cursortype & SOFT_CURSOR) {			\
		saved_loc = 0;						\
		if (IS_SCREEN(u_info->dst_bitmap)) {			\
			if (sys_info->is_src && IS_SCREEN(u_info->src_bitmap)){\
				saved_loc = 1;				\
				save_cursor(MIN(sys_info->dst.rect.origin_x,  \
					       sys_info->src.rect.origin_x),  \
					    MIN(sys_info->dst.rect.origin_y,  \
					       sys_info->src.rect.origin_y),  \
					    MAX(sys_info->dst.rect.corner_x,  \
					       sys_info->src.rect.corner_x)+1,\
					    MAX(sys_info->dst.rect.corner_y,  \
					       sys_info->src.rect.corner_y)+1);\
			}						\
			else {						\
				saved_loc = 1;				\
				save_cursor(sys_info->dst.rect.origin_x,     \
					    sys_info->dst.rect.origin_y,     \
					    sys_info->dst.rect.corner_x + 1, \
					    sys_info->dst.rect.corner_y + 1);\
			}						\
		}							\
		else if (sys_info->is_src && IS_SCREEN(u_info->src_bitmap)) { \
			saved_loc = 1;					\
			save_cursor(sys_info->src.rect.origin_x,	\
				    sys_info->src.rect.origin_y,	\
				    sys_info->src.rect.corner_x + 1,	\
				    sys_info->src.rect.corner_y + 1);	\
		}							\
	}								\
}

#define RESTORE_LOCATOR() {						\
	if ((blt_cur_screen.cursortype & SOFT_CURSOR) && saved_loc) {	\
		restore_cursor();					\
	}								\
}

/*
 * Macros to calculate the skew and preload flag depending on the direction
 * of the blt.
 */
#define computeSkewPos(SI) {					\
	if ((MOD_BPW(SI->dst.rect.origin_x))<(MOD_BPW(SI->src.rect.origin_x))){\
		SI->skew_src = MOD_BPW(SI->src.rect.origin_x) -	\
			       MOD_BPW(SI->dst.rect.origin_x);	\
		SI->preload_src = MUST_PRIME;			\
	}							\
	else {							\
		SI->skew_src = BPW - (MOD_BPW(SI->dst.rect.origin_x) -	\
			       MOD_BPW(SI->src.rect.origin_x));		\
                SI->preload_src = DONT_PRIME;			\
	}							\
}

#define computeSkewMskPos(SI) {						\
	if ((MOD_BPW(SI->dst.rect.origin_x))<(MOD_BPW(SI->msk.rect.origin_x))){\
		SI->skew_msk = MOD_BPW(SI->msk.rect.origin_x) -		\
			       MOD_BPW(SI->dst.rect.origin_x);		\
		SI->preload_msk = MUST_PRIME;				\
	}								\
	else {								\
		SI->skew_msk = BPW - (MOD_BPW(SI->dst.rect.origin_x) -	\
			       MOD_BPW(SI->msk.rect.origin_x));		\
                SI->preload_msk = DONT_PRIME;			   	\
	}								\
}

#define computeSkewNeg(SI) {						\
	if ((MOD_BPW(SI->dst.rect.corner_x))>(MOD_BPW(SI->src.rect.corner_x))){\
		SI->skew_src = MOD_BPW(SI->dst.rect.corner_x) -		\
			       MOD_BPW(SI->src.rect.corner_x);		\
		    SI->preload_src = MUST_PRIME;			\
	}								\
	else {								\
		SI->skew_src = BPW - (MOD_BPW(SI->src.rect.corner_x) -	\
			       MOD_BPW(SI->dst.rect.corner_x));		\
		SI->preload_src = DONT_PRIME;				\
	}								\
}

#define computeSkewMskNeg(SI) {						\
	if ((MOD_BPW(SI->dst.rect.corner_x))>(MOD_BPW(SI->msk.rect.corner_x))){\
		SI->skew_msk = MOD_BPW(SI->dst.rect.corner_x) -		\
			       MOD_BPW(SI->msk.rect.corner_x);		\
		SI->preload_msk = MUST_PRIME;				\
	}								\
	else {								\
		SI->skew_msk = BPW - (MOD_BPW(SI->msk.rect.corner_x) -	\
			       MOD_BPW(SI->dst.rect.corner_x));		\
		SI->preload_msk = DONT_PRIME;				\
	}								\
}

/*
 * Make a mask for the first and last destination word in each scanline.
 * 1's mean those bits can be changed in the destination.
 * Combine the masks if only one destination word is effected and set
 * BOTH masks equal to it so as not to screw up copyBitNeg. 
 *
 * NOTE: The +1 with dst.rect.corner_x. This is because dst.rect.corner_x was
 *       set to the rectangle size -1. (see blt_setup())
 */
#define computeMasks(SI) {						\
	if (SI->nshorts == 1) {						\
		SI->right_mask = SI->left_mask = 			\
				 leftMasks[MOD_BPW(SI->dst.rect.origin_x)] &\
				 rightMasks[MOD_BPW(SI->dst.rect.corner_x+1)];\
	}								\
	else {								\
		SI->left_mask = leftMasks[MOD_BPW(SI->dst.rect.origin_x)];\
		SI->right_mask = rightMasks[MOD_BPW(SI->dst.rect.corner_x+1)];\
	}								\
}

/*
 * CheckOverlap checks for overlapping source and destination. 
 * (See checkOverlap in Smalltalk-80)  It set SI->top_to_bot to
 * non-zero if we must copy from bottom to top, right to left
 * instead of top to bottom, left to right.
 *
 * NOTE: The +1 for both src.rect.corner_y and src.rect.corner_x this is because
 *       these local variables have "closed" corners. (See blt_setup())
 *
 * IF the destination and source have the same bitmap
 *                        AND 
 *           TEST AREA 1  OR  TEST AREA 2
 * THEN
 *             (We must copy from right to left bottom to top)
 *
 *    TEST AREA 1:  The destination top is GREATER than or EQUAL to the
 *                  source top and its less than the source bottom
 *            (src.rect.origin_y <= dst.rect.origin_y < src.rect.corner_y+1) AND
 *		    the destination left is greater than or equal to
 *                  the source left and its less than the source right.
 *	      (src.rect.origin_x <= dst.rect.origin_x < src.rect.corner_x+1)
 *		    IN OTHER WORDS, the top left corner of the destination
 *		    is in the source area.
 *
 *    TEST AREA 2:  The destination top is GREATER than the source top 
 *                  and its less than the source bottom:
 *                 (src.rect.origin_y < dst.rect.origin_y < src.rect.corner_y+1)
 *                  AND the source left is greater than the destination left
 *                  and its less than the destination right.
 *                 (dst.rect.origin_x < src.rect.origin_x < dst.rect.corner_x+1)
 *		    IN OTHER WORDS, the top horizontal line of the destination
 *		    intersects the left vertical line of the source, causing
 *		    the right top corner of the destination to be in the
 *		    source.
 *
 *			         src.rect.origin_x
 *	                         src.rect.origin_y +----------------------------
 *   src.rect.origin_y - 1  -----------------------|
 *                                                 |
 *			      TEST AREA 2	   |    TEST AREA 1
 *                                                 |
 *                                                 |
 *    src.rect.corner_y+1   ------------------------------------------------
 */
#define checkOverlap(UI,SI) \
	SI->top_to_bot = ((UI->src_bitmap.base == UI->dst_bitmap.base)	\
		  && (((SI->src.rect.origin_y <= SI->dst.rect.origin_y &&\
			SI->dst.rect.origin_y < (SI->src.rect.corner_y+1))\
		  && (SI->src.rect.origin_x <= SI->dst.rect.origin_x &&	\
			SI->dst.rect.origin_x < (SI->src.rect.corner_x+1)))\
	          || ((SI->src.rect.origin_y < SI->dst.rect.origin_y &&	\
			SI->dst.rect.origin_y < (SI->src.rect.corner_y+1))\
		  && (SI->dst.rect.origin_x < SI->src.rect.origin_x &&	\
			SI->src.rect.origin_x < (SI->dst.rect.corner_x+1)))))

/*
 * CalculateOffsetDst sets up pointers to a word in the destination and
 * possibly to the mask bitmap using the dst_x,dst_y points given.
 * It calculates the increment for each pointer from the end of one 
 * rectangle scanline to the beginning of the next.
 */
calculateOffsetDst(u_info,s_info,dst_x,dst_y)
register Blt_userdata *u_info;
register Blt_sysdata *s_info;
short	dst_x,dst_y; /* top,left or bottom,right */
{
	/*
	 * Set up the destination pointer and increment.
	 * NOTE that (dst_y * u_info->dst_bitmap.nshorts) + DIV_BPW(dst_x)
	 * gives the WORD offset to base.  Since we need the byte address
	 * this quantity must be multiplied by 2 (MUL_2).
	 *
	 * Set up destination to point to a word offset from the base address.
	 */
#if (APA8 || APA8C)
	if (IS_APA8orAPA8C(u_info->dst_bitmap)) {
		s_info->dst_plus = 4;
		s_info->dst.data = (unsigned short *)(
				   (long)u_info->dst_bitmap.base +
				   MUL_4((dst_y *
					u_info->dst_bitmap.nshorts) +
				        DIV_BPW(dst_x)));
		s_info->dst.nextline = MUL_4(u_info->dst_bitmap.nshorts -
					     s_info->nshorts + 1);
	}
	else {
		s_info->dst_plus = 2;
		s_info->dst.data = (unsigned short *)(
				   (long)u_info->dst_bitmap.base +
				   MUL_2((dst_y * u_info->dst_bitmap.nshorts) +
				         DIV_BPW(dst_x)));
		s_info->dst.nextline = MUL_2(u_info->dst_bitmap.nshorts -
				             s_info->nshorts + 1);
	}
#else no APA8 stuff
	s_info->dst.data = (unsigned short *)((long)u_info->dst_bitmap.base +
			    MUL_2((dst_y * u_info->dst_bitmap.nshorts) +
			          DIV_BPW(dst_x)));

	/*
	 * NOTE: This increment shows the increment for the number of BYTES.
	 */
	s_info->dst.nextline = MUL_2(u_info->dst_bitmap.nshorts -
				     s_info->nshorts + 1);
#endif APA8

	/*
	 * Set up the masking bitmap (if given) to point to a word
	 * offset from the base address.
	 */
	if (u_info->blt_flags & BLT_MASKON) {
		/*
	      	 * Calculate the shift amount (skew) that is needed
		 * to align a word in the mask bitmap with a word
		 * in the destination.
		 */
		if (s_info->top_to_bot) {
			computeSkewMskNeg(s_info);

			/*
			 * Calculate the mask pointer.
			 */
			s_info->msk.data = (unsigned short *)(
					   (long)(u_info->msk_bitmap.base) +
					   MUL_2((s_info->msk.rect.corner_y *
					         u_info->msk_bitmap.nshorts) +
					   DIV_BPW(s_info->msk.rect.corner_x)));
		}
		else {
			computeSkewMskPos(s_info);

			/*
			 * Calculate the mask pointer.
			 */
	       		s_info->msk.data = (unsigned short *)(
					   (long)(u_info->msk_bitmap.base) + 
					   MUL_2((s_info->msk.rect.origin_y *
					          u_info->msk_bitmap.nshorts) +
					   DIV_BPW(s_info->msk.rect.origin_x)));
		}

		/*
		 * Calculate the mask increment.
		 */
		s_info->msk.nextline = MUL_2( u_info->msk_bitmap.nshorts -
				             (s_info->nshorts +
					      s_info->preload_msk) + 1);
	}
	else {
		s_info->msk.data = (unsigned short *)0;
		s_info->msk.nextline = 0;
	}
}

/*
 * CalculateOffsetSrcNeg sets up pointers to the last word in the
 * source.  It also determines if "priming" must be done.
 * (i.e. 2 words of source are needed for the first word of the destination.)
 * When going from right to left this happens when for example you have 
 * 3 bits in the source (at the end of the line) (0-2) and 5 bits to fill
 * in the destination (0-4).  One must take those 3 bits from the source
 * and put them in 2-4 in the destination and increment the source.
 *
 * It then calculates the increment for the source.
 */

#if (APA8 || APA8C)
#define calculateOffsetSrcNeg(UI,SI) {					\
	if (IS_APA8orAPA8C(UI->src_bitmap)) {				\
		SI->src_plus = 4;					\
		SI->src.data = (unsigned short *)((long)(UI->src_bitmap.base) +\
			     MUL_4((SI->src.rect.corner_y *	\
					  UI->src_bitmap.nshorts) +	\
					 DIV_BPW(SI->src.rect.corner_x)));\
		SI->src.nextline = MUL_4(UI->src_bitmap.nshorts - 	\
				         (SI->nshorts+SI->preload_src) + 1);\
	}								\
	else {								\
		SI->src_plus = 2;					\
		SI->src.data = (unsigned short *)((long)(UI->src_bitmap.base) +\
	       		    MUL_2((SI->src.rect.corner_y *		\
				   UI->src_bitmap.nshorts) +		\
	                           DIV_BPW(SI->src.rect.corner_x)));	\
		SI->src.nextline = MUL_2(UI->src_bitmap.nshorts -	\
			           (SI->nshorts + SI->preload_src) + 1);\
	}								\
}
#else no APA8 stuff
#define calculateOffsetSrcNeg(UI,SI) {					\
	SI->src.data = (unsigned short *)((long)(UI->src_bitmap.base) +	\
                    MUL_2((SI->src.rect.corner_y * UI->src_bitmap.nshorts) +\
                          DIV_BPW(SI->src.rect.corner_x)));		\
	SI->src.nextline = MUL_2(UI->src_bitmap.nshorts -		\
			   (SI->nshorts+SI->preload_src) + 1);		\
}
#endif

/*
 * CalculateOffsetSrcPos sets up pointers to the first word in the
 * source.  It also determines if "priming" must be done. (i.e. 2 words 
 * of source are needed for the first word of the destination.)
 * When going from left to right this happens when for example you have 
 * 3 bits in the source (13-15) and 5 bits to fill in the destination (11-15).
 * One must take those 3 bits from the source and put them in 11-13
 * and then increment the source.
 *
 * It then calculates the increment for the source.
 */
#if (APA8 || APA8C)
#define calculateOffsetSrcPos(UI,SI) {					\
	if (IS_APA8orAPA8C(UI->src_bitmap)) {				\
		SI->src_plus = 4;					\
		SI->src.data = (unsigned short *)((long)UI->src_bitmap.base +  \
	                       MUL_4((SI->src.rect.origin_y *		\
				      UI->src_bitmap.nshorts) +	\
	                              DIV_BPW(SI->src.rect.origin_x)));\
		SI->src.nextline = MUL_4(UI->src_bitmap.nshorts -	\
			                 (SI->nshorts + SI->preload_src) + 1);\
	}								\
	else {								\
		SI->src_plus = 2;					\
		SI->src.data = (unsigned short *)((long)UI->src_bitmap.base +\
	                        MUL_2((SI->src.rect.origin_y *		\
			  	      UI->src_bitmap.nshorts) +		\
	                              DIV_BPW(SI->src.rect.origin_x)));	\
		SI->src.nextline = MUL_2(UI->src_bitmap.nshorts -	\
			           (SI->nshorts+SI->preload_src) + 1);	\
	}								\
}
#else no APA8 stuff
#define calculateOffsetSrcPos(UI,SI) {					\
	SI->src.data = (unsigned short *)((long)UI->src_bitmap.base +	\
                    MUL_2((SI->src.rect.origin_y * UI->src_bitmap.nshorts) +\
                          DIV_BPW(SI->src.rect.origin_x)));		\
	SI->src.nextline = MUL_2(UI->src_bitmap.nshorts -		\
			   (SI->nshorts+SI->preload_src) + 1);		\
}
#endif

/*
 * Set up destination offsets.
 */
#define calculateOffsetDstNeg(UI,SI) calculateOffsetDst(UI,SI,		\
							SI->dst.rect.corner_x,\
							SI->dst.rect.corner_y)
#define calculateOffsetDstPos(UI,SI) calculateOffsetDst(UI,SI,		\
							SI->dst.rect.origin_x, \
							SI->dst.rect.origin_y)

/*
 * If a rule specifies the type of tile to be used then set tile_ptr.
 * If tile_ptr has already been set then don't change it.
 */
#define calculateOffsetTile(UI,SI) {				\
	if (SI->rule == DstClear || SI->rule == TileDstClear) {	\
		SI->tile_ptr = &white.tile[0];			\
	}							\
	else if (SI->rule == DstSet || SI->rule == TileDstSet	\
		|| SI->rule == NotDstCopy) {			\
		SI->tile_ptr = &black.tile[0];			\
	}							\
	else if (SI->tile_ptr == (unsigned short *)0){		\
		SI->tile_ptr = (unsigned short *)UI->tile_ptr;	\
	}							\
}

/*
 * Do the bit block transfer using the information in u_info. Return -1 if
 * bad pointers are given, 0 if the area clipped down to a null blt and 1 if
 * the "bitblt" was done.
 */
bitblt(u_info)
register Blt_userdata *u_info;
{
	register Blt_sysdata *sys_info = &blt_info;
	register tmp;
	register saved_loc;
	int blt_status;

	DEBUGF((blt_debug > 1),printf("Enter bitblt.\r\n"));

	/*
	 * Set up to do this blt.  blt_setup does clipping if necessary, sets
	 * up either the source or tile (depending on the combination rule
	 * being used) and sets the width and height of the blt.  It returns
	 * zero if the blt is to be done.
	 */
	if ((blt_status = blt_setup(u_info,sys_info)) != 1) {
		/*
		 * Bad pointers or the combination rule was such that nothing
		 * has to be done.
		 */
		return(blt_status);
	}
	else if (sys_info->width <= 0 || sys_info->height <= 0) {
		/*
		 * Nothing to do.
		 */
		return(0);
	}

#ifdef USE_APA16_HDWR
	/*
	 * Are we using hardware??  This is for debugging purposes, we
	 * know the software works so to test the hardware stuff we have this
	 * flag in blt_flags.  However if we are using a mask bitmap then
	 * the hardware can not be used.
	 */
	if ((u_info->blt_flags & BLT_MASKON) == 0) {
		register int ret_flag;	/* Hardware return flag */

		/*
		 * One can only use the hardware assist if the destination
		 * bitmap is the screen.
		 */
		ret_flag = 1;
		if (IS_APA16(u_info->dst_bitmap)) {
			extern unsigned short execute_cmd[];

			/*
			 * Check and see if there is a hardware function
			 * (execute command) for this rule.
			 */
			if (execute_cmd[sys_info->rule]) {
				if (sys_info->is_src) {
					if (IS_APA16(u_info->src_bitmap)) {
						/*
						 * Pass in the pointers to
						 * the screen because if they
						 * are greater than the first
						 * byte address in the APA16
						 * buffer things will not
						 * work.
						 */
						sys_info->dst.data =
							u_info->dst_bitmap.base;
						sys_info->src.data =
							u_info->src_bitmap.base;
						ret_flag = apa16_StoS(sys_info);
					}
					else {
						/*
						 * The APA-16 does memory to
						 * screen stuff.  But first
						 * The source address and
						 * and the source "nextline"
						 * field must also be
						 * calculated.
						 *
						 * NOT IMPLEMENTED YET.
						 *
						 * computeSkewPos(sys_info);
						 * calculateOffsetSrcPos(u_info,
						 *		      sys_info);
						 * ret_flag =
						 *	   apa16_MtoS(sys_info);
						 */
					}
				}
				else {
					/*
					 * The tile ptr has not been set up yet.
					 */
					sys_info->tile_ptr = (unsigned short *)
							     u_info->tile_ptr;
					ret_flag = apa16_copyTile(sys_info);
				}

				/*
				 * Check the status of the hardware blt, if
				 * its 0 then everything was done successfully.
				 */
				if (ret_flag == 0) {

					/*
					 * Indicate that a blt was done.
					 */
					return(1);
				}
			}
		}
	}

	/*
	 * Regardless of the hardware flag, if we get here it means
	 * that we are going to use the software to blt.  If the screen
	 * is to be changed then we must wait for the rasterop engine to finish
	 * with the screen if it is still using the screen.
	 */
	if (*QUE_COUNT_R != 0) {
		if (IS_APA16(u_info->dst_bitmap) || 
		    (IS_APA16(u_info->src_bitmap))) {
			WAIT_QUE(tmp,0);
		}
	}

#endif USE_APA16_HDWR

#ifdef AED
	/*  
	 * If this is a screen to screen blt on the AED have the microcode
	 * make the screen changes while the bltter alters the 
	 * offscreen bitmap.
	 */ 

	if (IS_AED(u_info->src_bitmap) && IS_AED(u_info->dst_bitmap) &&
	    (u_info->blt_flags & BLT_ECHO)) {
		aed_screen_copy(sys_info->src.rect.origin_x,
				 sys_info->src.rect.origin_y,
				 sys_info->dst.rect.origin_x,
				 sys_info->dst.rect.origin_y,
				 (short) sys_info->width,
				 (short) sys_info->height,
				 sys_info->rule);
		blt_status = 0;
	}
#endif AED

	/*
	 * Remove the locator from the screen if it is a software locator that
	 * is actually in (on) the source or destination bitmap.  Hardware
	 * locators (as in the APA-16) are not removed.
	 */
	SAVE_LOCATOR();

	/*
	 * Calculate the edge masks for the destination. (See computeMasks
	 * in Smalltalk-80)
	 */
	computeMasks(sys_info);

	/*
	 * If the source is to be used then things will be done differently
	 * than if a tile is used.
	 */
	if (sys_info->is_src) {
		/*
		 * Check for overlapping source and destination. (See
		 * checkOverlap in Smalltalk-80)  Sets sys_info->top_to_bot
		 * non-zero if we must copy from bottom to top, right to
		 * left instead of top to bottom, left to right.
		 */
		checkOverlap(u_info,sys_info);

		if (sys_info->top_to_bot) {
			/*
			 * Calculate the shift amount (skew) that is needed
			 * to align a word in the source with a word in the
			 * destination.
			 */
			computeSkewNeg(sys_info);

			/*
			 * Set up pointers to the source, destination 
			 * and possibly to the mask bitmap. Also set up
			 * shift amounts (skews) for the mask bitmap if needed.
			 * Calculate the increment for each pointer from
			 * the end of one rectangle scanline to the beginning
			 * of the next.
			 */
			calculateOffsetSrcNeg(u_info,sys_info);
			calculateOffsetDstNeg(u_info,sys_info);

			/*
			 * Do the blt from bottom to top, right to left!!
			 */
			if (u_info->blt_flags & BLT_MASKON) {
				maskBitNeg(sys_info);
			}
			else {
				/*
				 * Just copy from source to destination
				 * going bottom to top, right to left.
				 * No mask bitmap to worry about.
				 */
				copyBitNeg(sys_info);
			}
		}
		else {
			/*
			 * Calculate the shift amount (skew) that is needed
			 * to align a word in the source with a word in the
			 * destination.
			 */
			computeSkewPos(sys_info);

			/*
			 * Set up pointers to the source, destination 
			 * and possibly to the mask bitmap.
			 * Also set up shift amounts (skews) for the mask
			 * bitmap if needed.  Calculate the increment for
			 * each pointer from the end of one rectangle
			 * scanline to the beginning of the next.
			 */
			calculateOffsetSrcPos(u_info,sys_info);
			calculateOffsetDstPos(u_info,sys_info);

			/*
			 * Do the blt from top to bottom, left to right!!
			 */
			if (u_info->blt_flags & BLT_MASKON) {
				/*
				 * Copy from source to destination going
				 * top to bottom, left to right. Use the
				 * mask bitmap.
				 */
				maskBitPos(sys_info);
			}
			else {
				/*
				 * Just copy from source to destination
				 * going top to bottom, left to right.
				 */
				if (sys_info->nshorts < 3) {
					/*
					 * Optimize this very important routine
					 * for one and two shorts in the dst
					 * changed.
					 */
					smallcopyBitPos(sys_info);
				}
				else {
					copyBitPos(sys_info);
				}
			}
		}
	}
	else { 
		/*
		 * We have a TILE rule.
		 */
		sys_info->top_to_bot = 0;

		/*
		 * Set up pointers to the tile and destination and possibly
		 * to the mask bitmap.  Calculate the increment for
		 * each pointer from the end of one rectangle scanline to
		 * the beginning of the next.
		 * NOTE: The pointer to the tile may have been set in
		 *	 blt_setup.
		 */
		calculateOffsetTile(u_info,sys_info);
		calculateOffsetDstPos(u_info,sys_info);

		/*
		 * Do the tile blt!
		 */
		if (u_info->blt_flags & BLT_MASKON) {
			maskTile(sys_info);
		}
		else {
			copyTile(sys_info);
		}
	}

	/*
	 * Restore the locator if the screen was dealt with.
	 */
	RESTORE_LOCATOR();

#ifdef AED
	/*  
	 * If we are echoing to AED, keep track of bounding
	 * rectangle of the updated screen area.
	 */ 

	if (IS_AED(u_info->dst_bitmap) && blt_status &&
	    (u_info->blt_flags & BLT_ECHO)) {
		aed_echo_rect(&changed_rect);
	}
#endif AED

	/*
	 * Indicate that a blt was done.
	 */
	return(1);
}
