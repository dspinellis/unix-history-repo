/* $Header: bitblt_int.h,v 10.1 86/11/19 10:52:05 jg Exp $ */
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
 * BITBLT COMPILATION OPTIONS:
 *	define APA16  	 	Bltter handles the APA16 screen.
 *	define APA8   		Bltter handles the APA8 screen.
 *	define APA8C   		Bltter handles the APA8C screen.
 *	define AED   		Bltter handles the AED screen.
 *	define BLT_DEBUG   	Bltter debug messages on.
 *	define USE_APA16_HDWR   Bltter will use screen hardware where
 *				implemented.
 */

#ifdef BLT_DEBUG
#define DEBUGF(cond,stmt)	if (cond) { stmt; }
#else
#define DEBUGF(cond,stmt)
#endif

/*
 * bitblt_ext.h needs this defined because of XAddr.
 */
#include <machinecons/xio.h>
/*
 * Include the external interface routine to the bitblt routine, bitblt_ext.h.
 */
#include "bitblt_ext.h"

typedef Blt Blt_userdata;

typedef struct Blt_screen_info {
	unsigned short *firstaddr;	/* first address on the screen bitmap */
	unsigned short *lastaddr;	/* last address on the screen bitmap */
	unsigned long cursortype;	/* Indicate the type of cursor on this
					   screen. */
} Blt_screen_info;
	
typedef struct bitmap_info {
	unsigned short *data;	/* pointer to the starting place of the bit
				   blt. */
	Blt_Rectangle rect;	/* bounds of the area to be used or changed */
	long nextline;	/* The amount to be added to the pointer to
			   get to the next logical scanline */
} Blt_bitmap_info;

typedef struct Blt_sysdata {
	long height,width;	/* the height and width (in bits) of the
				   destination area to be changed */
	short	nshorts;	/* number of destination shorts to be changed */
	short	top_to_bot;	/* If non-zero it indicates the blt is to go
				   from right to left, bottom to top */
	short	is_src;		/* Indicates that the rule uses the source */
	short	rule;		/* combination rule or "mode" to be used with
				   the data */
	short	preload_src;	/* Indicates 2 source words are needed to make
				   the first destination word. */
	short	preload_msk;	/* Indicates 2 mask words are needed to make
				   the first destination word. */
	short	skew_src;	/* The amount the source must be shifted to
				   line up with the destination */
	short	skew_msk;	/* The amount the mask must be shifted to
				   line up with the destination */
	unsigned short left_mask;  /* left mask to protect those bits on the
				      left edge that are not to be changed */
	unsigned short right_mask; /* right mask to protect those bits on the
				      right edge that are not to be changed */

	unsigned short *tile_ptr;  /* Points to the Tile to be used to tile
				      the destination */
	Blt_bitmap_info dst;	/* Internal information for the destination 
				   bitmap. */
	Blt_bitmap_info src;	/* Internal information for the source bitmap */
	Blt_bitmap_info msk;	/* Internal information for the mask bitmap*/
#if (APA8 || APA8C)
	short	dst_plus;	/* the amount to increment the destination
				   pointer to get to the next word. (usually
				   its 1, with the APA-8 its 2)*/
	short	src_plus;	/* the amount to increment the source 
				   pointer to get to the next word. (usually
				   its 1, with the APA-8 its 2)*/
#endif
} Blt_sysdata;

#define SETRECT(dst,l,t,r,b) {			\
	(dst)->origin_x = l;  (dst)->origin_y = t;	\
	(dst)->corner_x = r; (dst)->corner_y = b;	\
}

/*
 * Macro to determine if a bitmap points to the APA-8 screen.
 */
#define APA8BASE	0xf4d00000
#define APA8_BYTE_SIZE	(128*512)

/*
 * Macro to determine if a bitmap points to the APA-8 screen.
 */
#define APA8CBASE	0xf4d20000
#define APA8C_BYTE_SIZE	(128*512)

/*
 * Macro to determine if a bitmap points to the APA-16 screen.
 */
#define APA16BASE	0xf4d80000
#define APA16_BYTE_SIZE	(128*1024)

/*
 * Macro to determine if a bitmap points to the AED's offscreen bitmap
 */
#define AEDBASE		0xEF100000
#define AED_BYTE_SIZE	(128*800)

/*
 * Macro to determine if a bitmap points to the screen.
 */
#define IS_APA8(BM)	(APA8BASE <= (long)BM.base && \
			 (long)BM.base < (APA8BASE+(2*APA8_BYTE_SIZE)))

#define IS_APA8C(BM)	(APA8CBASE <= (long)BM.base && \
			 (long)BM.base < (APA8CBASE+(2*APA8C_BYTE_SIZE)))

#define IS_APA8orAPA8C(BM)	(APA8BASE <= (long)BM.base && \
				(long)BM.base < (APA8CBASE+(2*APA8C_BYTE_SIZE)))

#define IS_APA16(BM)	(APA16BASE <= (long)BM.base && \
			(long)BM.base < (APA16BASE+APA16_BYTE_SIZE))

#define IS_AED(BM)	(AEDBASE <= (long)BM.base && \
			(long)BM.base < (AEDBASE+AED_BYTE_SIZE))

#define DstClear        0
#define SrcAnd          1
#define SrcAndNotDst    2
#define SrcCopy         3
#define NotSrcAnd       4
#define DstCopy         5
#define SrcXor          6
#define SrcOr           7
#define NotSrcAndNotDst 8
#define NotSrcXor       9
#define NotDstCopy      10
#define SrcOrNotDst     11
#define NotSrcCopy      12
#define NotSrcOr        13
#define NotSrcOrNotDst  14
#define DstSet          15
 
#define TileDstClear     16
#define TileAnd          17
#define TileAndNotDst    18
#define TileCopy         19
#define NotTileAnd       20
#define TileDstCopy      21
#define TileXor          22
#define TileOr           23
#define NotTileAndNotDst 24
#define NotTileXor       25
#define TileNotDstCopy   26
#define TileOrNotDst     27
#define NotTileCopy      28
#define NotTileOr        29
#define NotTileOrNotDst  30
#define TileDstSet       31

#define SRC_TO_TILE	TileDstClear

/*
 * Macros to determine if the given rule refers to a tile or a source
 * bitmap.
 * NOTE: This Macro depends on the order of the defines above.
 */
#define IS_SRC(rule)	((rule < TileDstClear) && (rule != DstSet) && \
			 (rule != DstClear) && (rule != NotDstCopy))

#define IS_SCREEN(bitmap) (blt_cur_screen.firstaddr <= bitmap.base && \
			   bitmap.base < blt_cur_screen.lastaddr)

#ifndef BPW
/*
 * Macros for division from words and bytes.
 */
#define BPW		16 /* Bits per Word */
#define LOG2_BPW	4
#define MOD_BPW(value)	((value) & (BPW-1))
#define DIV_BPW(value)	((value) >> LOG2_BPW)
#define MUL_BPW(value)	((value) << LOG2_BPW)

/*
 * Bits TO Words.
 */
#define BTOW(bits)	(DIV_BPW((bits) + (BPW-1)))
#endif BPW

/*
 * Macros for the division and multiplication by 2.
 */
#define DIV_2(n) ((n) >> 1)
#define MUL_2(n) ((n) << 1)
#define MOD_2(n) ((n) & 0x1)

/*
 * Need to multiply by 4 when using the APA-8.
 */
#define MUL_4(n) ((n) << 2)

/*
 * Bits Per Byte.
 */
#define BPB             8
#define LOG2_BPB        3
#define MOD_BPB(value)  ((value) & (BPB-1))
#define DIV_BPB(value)  ((value) >> LOG2_BPB)
#define MUL_BPB(value)  ((value) << LOG2_BPB)
/*
 * Bits TO Bytes.
 */
#define BTOB(bits)      (DIV_BPB((bits) + (BPB-1)))

extern Blt_screen_info blt_cur_screen;
#ifdef BLT_DEBUG
extern int blt_debug;
#endif

#define MIN(a,b)	((a < b) ? (a) : (b))
#define MAX(a,b)	((a > b) ? (a) : (b))
