/* $Header: text.h,v 10.1 86/11/19 10:46:56 jg Exp $ */
/* text.h - macros and global data used by X text functions
 *
 *	Author:
 *		Dan Stone & Scott Bates
 *		Brown University
 *		IRIS, Box 1946
 *		Providence, RI 02912
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

/*
 * Macros for all the combination rules which can be used.
 */

#define GXcopy_OP(src, dst)	    dst = (src)
#define GXor_OP(src, dst)	    dst = (dst) | (src)
#define GXxor_OP(src, dst)	    dst = (dst) ^ (src)
#define GXand_OP(src, dst)	    dst = (dst) & (src)
#define GXcopyInverted_OP(src, dst) dst = ~(src)
#define GXorInverted_OP(src, dst)   dst = (dst) | ~(src)
#define GXequiv_OP(src, dst)	    dst = (dst) ^ ~(src)
#define GXandInverted_OP(src, dst)  dst = (dst) & ~(src)
#define GXorReverse_OP(src, dst)    dst = (~(dst) | (src))
#define GXandReverse_OP(src, dst)   dst = (~(dst) & (src))
#define GXinvert_OP(src, dst)	    dst = ~(dst)
#define GXnand_OP(src, dst)	    dst = (~(dst) | ~(src))
#define GXnor_OP(src, dst)	    dst = (~(dst) & ~(src))
#define GXclear_OP(src, dst)	    dst = 0
#define GXset_OP(src, dst)	    dst = 0xFFFF
#define GXnoop_OP(src, dst)	    dst = dst

#define GXcopy_MASK(src, dst, mask)	    dst = ((dst) & ~(mask)) |	       \
						  ((src) & mask)
#define GXor_MASK(src, dst, mask)	    dst = ((dst) | ((src) & mask))
#define GXxor_MASK(src, dst, mask)	    dst = ((dst) ^ ((src) & mask)) 
#define GXand_MASK(src, dst, mask)	    dst = ((dst) & ~(mask)) |	       \
						  (((dst) & (src)) & mask)
#define GXcopyInverted_MASK(src, dst, mask) dst = ((dst) & ~(mask)) |	       \
						  (~(src) & mask)
#define GXorInverted_MASK(src, dst, mask)   dst = ((dst) | (~(src) & mask))
#define GXequiv_MASK(src, dst, mask)	    dst = ((dst) ^ (~(src) & mask))
#define GXandInverted_MASK(src, dst, mask)  dst = ((dst) & ~(mask)) |	       \
						  (((dst) & ~(src)) & mask)
#define GXandReverse_MASK(src, dst, mask)   dst = ((dst) & ~(mask)) |	       \
						  ((~(dst) & (src)) & mask)
#define GXorReverse_MASK(src, dst, mask)    dst = ((dst) & ~(mask)) |	       \
						  ((~(dst) | (src)) & mask)
#define GXinvert_MASK(src, dst, mask)	    dst = ((dst) & ~(mask)) |	       \
						  (~(dst) & (mask))
#define GXnor_MASK(src, dst, mask)	    dst = ((dst) & ~(mask)) |	       \
						  ((~(dst) & ~(src)) & mask)
#define GXnand_MASK(src, dst, mask)	    dst = ((dst) & ~(mask)) |	       \
						  ((~(dst) | ~(src)) & mask)
#define GXclear_MASK(src, dst, mask)	    dst = 0
#define GXset_MASK(src, dst, mask)	    dst = 0xFFFF
#define GXnoop_MASK(src, dst, mask)	    dst = dst

#ifndef BPL
#define BPL 32
#define LOG2_BPL 5
#define MOD_BPL(value)	((value) & (BPL - 1))
#define DIV_BPL(value)	((value) >> LOG2_BPL)
#define MUL_BPL(value)	((value) << LOG2_BPL)

/*
 * Macro to convert Bits to Longs.
 */

#define BTOL(bits)  (DIV_BPL((bits) + (BPL - 1)))

#define MUL_4(n)    ((n) << 2)
#define DIV_4(n)    ((n) >> 2)
#endif BPL

#define SRC	((u_short *)src)
#define SRCPL	((u_short *)(src + 2))
#define DST	((u_short *)dst)
#define DSTPL	((u_short *)(dst + 2))

/*
 * Masks for to protect the left portion of a short.  That portion that should
 * not be changed.
 */

static u_short leftmask_lib[] = {
    0xFFFF,0x7FFF,0x3FFF,0x1FFF,
    0x0FFF,0x07FF,0x03FF,0x01FF,
    0x00FF,0x007F,0x003F,0x001F,
    0x000F,0x0007,0x0003,0x0001,
    0x0000
};

/*
 * Masks for to protect the right portion of a short.  That portion that should
 * not be changed.
 */

static u_short rightmask_lib[] = {
    0xFFFF,0x8000,0xC000,0xE000,
    0xF000,0xF800,0xFC00,0xFE00,
    0xFF00,0xFF80,0xFFC0,0xFFE0,
    0xFFF0,0xFFF8,0xFFFC,0xFFFE,
    0x0000
};

/*
 * This macro is used to copy characters from the fonts character bitmaps
 * to the offscreen bitmap. Later the offscreen bitmap is copied to the screen
 * using the bltter.
 *
 * The CopyText_LOOP is an optimization, of a more general bitblt loop.
 * Because most text characters are less than 16 bits wide the CopyText_LOOP
 * has 2 special inner loops for this size character image.  A character
 * image thats less than 16 bits wide can only span at most 2 destination
 * shorts, so the 2 special inner loops are designed for 1 destination and
 * 2 destination shorts changing per line.  IF more than 2 destination words
 * are to change then the general purpose loop is used.
 *
 * Some special notes:
 *
 * bitptr - A long integer that "points to" or "indexes into" one offscreen
 *	    buffer scanline.  As if one scanline was an array of bits.
 *
 * src,dst - Are unsigned CHAR pointers and are cast to unsigned shorts
 *	     when used as pointers.  Why?  IF src was an unsigned SHORT
 *	     pointer and src += n was done, the current RT compiler
 *	     would generate 2 extra assembly language instructions,
 *	     one to shift n left (multiply by 2) and the other to
 *	     compute the address.
 *
 * The offscreen buffer is the destination.
 *
 * Here is the algorithm for the CopyText_LOOP:
 * 
 * FOREACH character DO
 *     IF the character is not printable THEN
 *	   continue
 *     IF the character is wider than zero THEN
 *	   set start_dst = To the destination short which contains the first
 *			   bit to be changed.
 *	   set shift = The first bit within start_dst to be changed.
 *	   Increment bitptr past the current character.
 *	   IF bitptr is beyond the last bit in the destination THEN
 *	       Decrement bitptr back to its previous value.
 *	       Decrement the character count.
 *	       Break out of the loop.
 *	   ENDIF
 *
 *	   Set the right mask using the right mask library and the NEW bitptr.
 *	   Calculate the source pointer.
 *	   Calculate the destination pointer.
 *	   Calculate the number of shorts changed in the destination.
 *
 *	   IF no shifting needed THEN
 *	       IF 1 destination short changed THEN
 *		   FOREACH scanline DO
 *		       Use the edge macro (see above) to combine src, dst and
 *			   mask.
 *		       Increment the source to the next scanline.
 *		       Increment the destination to the next scanline.
 *		   ENDFOR
 *	       ELSE IF 2 destination shorts changed THEN
 *		   FOREACH scanline DO
 *		       Use the macro (see above) to combine src and dst.
 *		       Use the edge macro (see above) to combine src+2, dst+2
 *			   and mask.
 *		       Increment the source to the next scanline.
 *		       Increment the destination to the next scanline.
 *		   ENDFOR
 *	       ELSE
 *		   Calculate the amount to decrement the dst to get back
 *		       to the first scanline next short(the + 2).
 *		   Calculate the amount to decrement the src to get back
 *		       to the first scanline next short(the + 2).
 *	   FOREACH short in the destination DO
 *		       FOREACH scanline DO
 *			   Use the macro (see above) to combine src and dst.
 *			   Increment the source to the next scanline.
 *			   Increment the destination to the next scanline.
 *		       ENDFOR
 *		       Decrement dst back to the first scanline, next short.
 *		       Decrement src back to the first scanline, next short.
 *		       Reset the number of scanlines (height).
 *		   ENDFOR
 *		   FOREACH scanline DO
 *		       Use the edge macro to combine src, dst and mask.
 *		       Increment the source to the next scanline.
 *		       Increment the destination to the next scanline.
 *		   ENDFOR
 *	       ENDIF
 *	   ELSE
 *	       Get the left mask from the left mask library using shift.
 *	       IF 1 destination short changed THEN
 *		   Combine left and right masks.
 *		   FOREACH scanline DO
 *		       Use the edge macro (see above) to combine src shifted,
 *			   with dst and mask.
 *		       Increment the source to the next scanline.
 *		       Increment the destination to the next scanline.
 *		   ENDFOR
 *	       ELSE IF 2 destination shorts changed THEN
 *		   Set the inverse of shift = 16 - shift.
 *		   IF the src bitmap is only 1 short wide THEN
 *		       FOREACH scanline DO
 *			   Use the edge macro (see above) to combine src
 *			       shifted, with dst and left mask.
 *			   Use the edge macro (see above) to combine src
 *			       inversely shifted with dst+2 and right mask.
 *			   Increment the source to the next scanline.
 *			   Increment the destination to the next scanline.
 *		       ENDFOR
 *		   ELSE	 # src bitmap is wider than 1 short and the 2nd short
 *			 # may have useful info in it.
 *		       FOREACH scanline DO
 *			   Use the edge macro (see above) to combine src
 *			       shifted with dst and left mask.
 *			   Use the edge macro (see above) to combine src
 *			       shifted, src+2 inversely shifted with 
 *			       dst+2 and right mask.
 *			   Increment the source to the next scanline.
 *			   Increment the destination to the next scanline.
 *		       ENDFOR
 *	       ELSE
 *		   Calculate the amount to decrement the dst to get back
 *		       to the first scanline next short(the + 2).
 *		   Calculate the amount to decrement the src to get back
 *		       to the first scanline next short(the + 2).
 *		   # Deal with the left edge.
 *		   FOREACH scanline DO
 *		       Use the edge macro (see above) to combine src shifted
 *			   with dst and left mask.
 *		       Increment the source to the next scanline.
 *		       Increment the destination to the next scanline.
 *		   ENDFOR
 *		   # Now deal with the shorts between the left edge and right
 *		   # edge.
 *	   FOREACH short in the destination DO
 *		       FOREACH scanline DO
 *			   Use the macro (see above) to combine src inversely
 *			       shifted or'ed with src+2 shifted with dst.
 *			   Increment the source to the next scanline.
 *			   Increment the destination to the next scanline.
 *		       ENDFOR
 *		       Decrement dst back to the first scanline, next short.
 *		       Decrement src back to the first scanline, next short.
 *		       Reset the number of scanlines (height).
 *		   ENDFOR
 *		   IF the number of destination shorts == the number of
 *		       shorts wide the src is THEN
 *		       # We have incremented to the last short in the src
 *		       # scanline and should not use src+2.
 *		       FOREACH scanline DO
 *			   Use the edge macro to combine src inversely shifted,
 *			       with dst and right mask.
 *			   Increment the source to the next scanline.
 *			   Increment the destination to the next scanline.
 *			ENDFOR
 *		   ELSE
 *		       FOREACH scanline DO
 *			   Use the edge macro to combine src inversely shifted
 *			       or'ed with src+2 shifted, with dst and right
 *			       mask.
 *			   Increment the source to the next scanline.
 *			   Increment the destination to the next scanline.
 *			ENDFOR
 *		   ENDIF
 *	       ENDIF
 *	   ENDIF
 *     ENDIF
 * ENDFOR
 *
 * NOTE: Make sure ch_pad is not added to the last character copied.
 */

#define CopyText_LOOP(EDGE_OP, OP) {					    \
    for (i = ch_count + 1; --i; ) {					    \
	if ((c = (int)(*string++)) < font->first || c > font->last)	    \
	    continue;			   /* Character is unprintable. */  \
	if (char_widths[c] > 0) {					    \
	    shift = bitptr & 0x0F;					    \
	    start_dst = DIV_BPW(bitptr);				    \
	    if (((bitptr += char_widths[c]) + ch_pad) > maxbitptr){	    \
		bitptr -= char_widths[c];				    \
		ch_count -= i;						    \
		break;		       /* Overflow: Get out of the loop */  \
	    }								    \
	    rtmask = rightmask_lib[bitptr & 0x0F];			    \
	    src = (u_char *)(chrs_data) + (shorts_per_char *		    \
		  (c - font->first));					    \
	    dst = (u_char *)buf_bm->data + MUL_2(start_dst);		    \
	    ndst_shorts = DIV_BPW(bitptr - 1) - start_dst + 1;		    \
	    height = ht_plus1;						    \
	    if (shift == 0) {						    \
		if (ndst_shorts == 1) {					    \
		    while (--height) {					    \
			EDGE_OP(*SRC, *DST, rtmask);			    \
			dst += dst_nextline;				    \
			src += src_nextline;				    \
		    }							    \
		} else if (ndst_shorts == 2) {				    \
		    while (--height) {					    \
			OP(*SRC, *DST);					    \
			EDGE_OP(*SRCPL, *DSTPL, rtmask);		    \
			dst += dst_nextline;				    \
			src += src_nextline;				    \
		    }							    \
		} else {						    \
		    height = font->height;				    \
		    dst_nextcol = (height*dst_nextline) - 2;		    \
		    src_nextcol = (height*src_nextline) - 2;		    \
		    height++;						    \
		    while (--ndst_shorts > 0) {				    \
			while (--height) {				    \
			    OP(*SRC, *DST);				    \
			    dst += dst_nextline;			    \
			    src += src_nextline;			    \
			}						    \
			dst -= dst_nextcol;				    \
			src -= src_nextcol;				    \
			height = ht_plus1;				    \
		    }							    \
		    while (--height) {					    \
			EDGE_OP(*SRC, *DST, rtmask);			    \
			dst += dst_nextline;				    \
			src += src_nextline;				    \
		    }							    \
		}							    \
	    } else {							    \
		leftmask = leftmask_lib[shift];				    \
		if (ndst_shorts == 1) {					    \
		    leftmask &= rtmask;					    \
		    while (--height) {					    \
			EDGE_OP((*SRC >> shift), *DST, leftmask);	    \
			dst += dst_nextline;				    \
			src += src_nextline;				    \
		    }							    \
		} else if (ndst_shorts == 2) {				    \
		    inv_shift = BPW - shift;				    \
		    if (chrs_nshorts == 1) {				    \
			while (--height) {				    \
			    EDGE_OP((*SRC >> shift), *DST, leftmask);	    \
			    EDGE_OP(*(SRC) << inv_shift, *(DSTPL), rtmask); \
			    dst += dst_nextline;			    \
			    src += src_nextline;			    \
			}						    \
		    } else {						    \
			while (--height) {				    \
			    EDGE_OP((*SRC >> shift), *DST, leftmask);	    \
			    EDGE_OP((*SRC<<inv_shift)|(*(SRCPL)>>shift),    \
			        *(DSTPL), rtmask);			    \
			    dst += dst_nextline;			    \
			    src += src_nextline;			    \
			}						    \
		    }							    \
		} else {						    \
		    height = font->height;				    \
		    dst_nextcol = (height*dst_nextline) - 2;		    \
		    src_nextcol = (height*src_nextline) - 2;		    \
		    height++;						    \
		    while (--height) {					    \
			EDGE_OP((*SRC >> shift), *DST, leftmask);	    \
			dst += dst_nextline;				    \
			src += src_nextline;				    \
		    }							    \
		    dst -= dst_nextcol;					    \
		    src -= (src_nextcol + 2);				    \
		    inv_shift = BPW - shift;				    \
		    nsrc_shorts = ndst_shorts - 1;			    \
		    while (--ndst_shorts > 1) {				    \
			height = ht_plus1;				    \
			while (--height) {				    \
			    OP((*SRC<<inv_shift)|(*(SRCPL)>>shift),*DST);   \
			    dst += dst_nextline;			    \
			    src += src_nextline;			    \
			}						    \
			dst -= dst_nextcol;				    \
			src -= src_nextcol;				    \
		    }							    \
		    height = ht_plus1;					    \
		    if (nsrc_shorts == chrs_nshorts) {			    \
			while (--height) {				    \
			    EDGE_OP((*SRC << inv_shift), *DST, rtmask);	    \
			    dst += dst_nextline;			    \
			    src += src_nextline;			    \
			}						    \
		    } else {						    \
			while (--height) {				    \
			    EDGE_OP((*SRC << inv_shift) | (*(SRCPL) >>	    \
			    shift), *DST, rtmask);			    \
			    dst += dst_nextline;			    \
			    src += src_nextline;			    \
			}						    \
		    }							    \
		}							    \
	    }								    \
	    bitptr += ch_pad;						    \
	}								    \
	if (c == font->space) {						    \
	    if ((bitptr += sp_pad) > maxbitptr) {			    \
		bitptr -= sp_pad;					    \
		ch_count -= i;						    \
		break;		       /* Overflow: Get out of the loop. */ \
	    }								    \
	}								    \
    }									    \
}
