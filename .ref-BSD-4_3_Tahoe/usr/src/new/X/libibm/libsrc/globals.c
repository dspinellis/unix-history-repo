#ifndef lint
static char *rcsid_globals_c = "$Header: globals.c,v 10.2 86/12/17 18:06:15 swick Exp $";
#endif	lint
/* Copyright 1985 Massachusetts Institute of Technology */

/*
 * globals.c - Various global data
 *
 * 	global defintions for device dependent code
 *
 *  	Changes and additions by:
 *
 *		Scott Bates
 *		Brown University
 *		IRIS, Box 1946
 *     		Providence, RI 02912
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

/*
 * Foreground/background function map
 */

char FBMap[] = {
	0x00, 0x00, 0x00, 0x00, 0x05, 0x05, 0x05, 0x05,
        0x0A, 0x0A, 0x0A, 0x0A, 0x0F, 0x0F, 0x0F, 0x0F,

        0x00, 0x04, 0x08, 0x0C, 0x01, 0x05, 0x09, 0x0D,
        0x02, 0x06, 0x0A, 0x0E, 0x03, 0x07, 0x0B, 0x0F,

        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
        0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,

        0x00, 0x05, 0x0A, 0x0F, 0x00, 0x05, 0x0A, 0x0F,
        0x00, 0x05, 0x0A, 0x0F, 0x00, 0x05, 0x0A, 0x0F
};

/*
 * Single source function map
 */

char SSMap[] = {
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
	0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,

	0x00, 0x04, 0x08, 0x0C, 0x01, 0x05, 0x09, 0x0D,
	0x02, 0x06, 0x0A, 0x0E, 0x03, 0x07, 0x0B, 0x0F,
};

/*
 * Constant tiles
 */

u_short AllZeros[TILE_SIZE] = {
	0x0000, 0x0000, 0x0000, 0x0000, 
	0x0000, 0x0000, 0x0000, 0x0000, 
	0x0000, 0x0000, 0x0000, 0x0000, 
	0x0000, 0x0000, 0x0000, 0x0000, 
};

u_short AllOnes[TILE_SIZE] = {
	0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
	0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
	0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
	0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
};

u_short *ConstantTiles[2] = {
	AllZeros,
	AllOnes,
};

/*
 * Constant pixmaps
 */

PIXMAP constpix0 = {1, 1, 1, 1, ConstantPixmap, (caddr_t) AllZeros};
PIXMAP constpix1 = {1, 1, 1, 1, ConstantPixmap, (caddr_t) AllOnes};

/*
 * Bitmap representing the screen (frame buffer)
 */

BITMAP pbm;

/*
 * Bitmap for offscreen text.
 */

u_short TextBuffer[TEXT_BUFFER_SIZE];
BITMAP txtbm = {MAX_OFFSCR_WD, MAX_OFFSCR_HT, 0, 0, (caddr_t) TextBuffer};

/*
 * File descriptor for open display
 */

int xdev = -1;

/*
 * Pointer to shared memory area
 */

XIoAddrAddr XAddr;

/*
 * File descriptor for Mouse: RAM 12/12/86  
 */

int mdev = -1;
 
