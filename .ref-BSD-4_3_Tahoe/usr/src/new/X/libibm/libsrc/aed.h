/* $Header: aed.h,v 10.1 86/11/19 10:44:53 jg Exp $ */
/* aed.h - Defines and macros needed to support AED
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

#include "../bitblt/bitblt_aed.h"

#define SCREEN_BASE 	AED_BM_ADDR /* buffer address for bitmap structure */
#define SCREEN_BM_SIZE  AED_BM_SIZE /* size of the aed screen bitmap */
#define XDEV_ID 	XDEV_IBMAED /* device id for info structure */

#define X_SCREEN_WIDTH  AED_WIDTH   /* number of bits/line for aed display */
#define X_SCREEN_HEIGHT AED_HEIGHT  /* number of lines on the screen */

#define REAL_SCREEN_WIDTH  X_SCREEN_WIDTH
#define REAL_SCREEN_HEIGHT X_SCREEN_HEIGHT 

#define CURSOR_TYPE	HARD_CURSOR	/* aed uses hardware cursor */

#define DISPLAY_INIT()  aed_init()    	/* display initialization routine */

#define SCREEN_DEVICE	"/dev/aed"	/* device name of display */
#define MOUSE_DEVICE	"/dev/msaed"	/* device name of mouse for display */

/*
 * Defines for default foreground/background
 */

#define	WHITE_ON_BLACK	0
#define	BLACK_ON_WHITE	1

/*
 * gets color of constant tile (black or white)
 */

#define CONSTANT_TILE_COLOR(blt) ((blt->tile_ptr->tile[0]) & 0xFFFF ? 1 : 0)
