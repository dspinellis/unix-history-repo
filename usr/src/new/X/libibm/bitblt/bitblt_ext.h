/* $Header: bitblt_ext.h,v 10.1 86/11/19 10:51:57 jg Exp $ */
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
 * Rectangle.
 */
typedef struct Blt_Rectangle {
	short origin_y;		/* top */
	short origin_x;		/* left */
	short corner_y;		/* bottom */
	short corner_x;		/* right */
} Blt_Rectangle;

/*
 * Bitmap.
 */
typedef struct Blt_Bitmap {
	unsigned short *base;	/* pointer to the actual bits */
	Blt_Rectangle rect;	/* bounding rectangle */
	short nshorts;		/* number of shorts wide the bitmap is */
} Blt_Bitmap;

/*
 * Tile. (Texture,Pattern have your pick.)
 */
#define BLT_TILE_SIZE	16
typedef struct Blt_Tile {
	unsigned short tile[BLT_TILE_SIZE];
} Blt_Tile;

/*
 * Bitblt user data structure.
 *
 * bitblt() is passed a pointer to a user structure which contains all the
 * necessary information to do the bit block transfer.
 *
 * NOTE: If the "comb_rule" does uses the source then tile_ptr does not have
 * to be set.  Otherwise comb_rule is a tile combination rule and the
 * src_bitmap and src_rect need not be filled in.  If the clip bit in
 * blt_flags is off (0) then clp_rect need not be filled in. If the mask
 * bit in blt_flags is off then msk_bitmap need not be filled in.
 */
typedef struct {
	Blt_Bitmap	src_bitmap;	/* Bitmap to be copied from */
	Blt_Rectangle	src_rect;	/* Specifies the area in the
					   src_bitmap */
	Blt_Bitmap	dst_bitmap;	/* Bitmap to be changed */
	Blt_Rectangle	dst_rect;	/* Specifies the area in the
					   dst_bitmap */
	Blt_Rectangle	clp_rect;	/* Another rectangle to clip against */
	Blt_Tile	*tile_ptr; 	/* The tile to be used if the
					   combination rule uses a tile */
	Blt_Bitmap	msk_bitmap;	/* Masking bitmap */
	short		comb_rule;	/* combination rule to be used */
	short		blt_flags;	/* A bit on means do a certain
					   operation.  For example if the first
					   bit is on then do clipping. */
} Blt;

/*
 * Flags blt_flags could be.
 */
#define BLT_CLIPON		0x1
#define BLT_MASKON		0x2
#define BLT_ECHO		0x4

#ifndef BPW
/*
 * Macros for division from words and bytes.
 */
#define BPW             16 /* Bits per Word */
#define LOG2_BPW        4
#define MOD_BPW(value)  ((value) & (BPW-1))
#define DIV_BPW(value)  ((value) >> LOG2_BPW)
#define MUL_BPW(value)  ((value) << LOG2_BPW)

/*
 * Bits TO Words.
 */
#define BTOW(bits)      (DIV_BPW((bits) + (BPW-1)))

/*
 * Macros for the division and multiplication by 2.
 */
#define DIV_2(n) ((n) >> 1)
#define MUL_2(n) ((n) << 1)
#define MOD_2(n) ((n) & 0x1)
#endif BPW

/*
 * Flags to indicate whether or not current screen is a hardware or
 * software cursor.
 */
#define SOFT_CURSOR	0x1
#define HARD_CURSOR	0x2

/*
 * externs for X access.
 */
extern int xdev;
extern XIoAddr *XAddr;
