/* $Header: font.h,v 10.1 86/11/19 10:45:35 jg Exp $ */
/* Copyright 1985 Massachusetts Institute of Technology */

/* font.h - Definitions required to access X fonts
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

#define CHARPERFONT 256

typedef short a_BitmapEntryPtr[2];

typedef struct _Bitmap {
        a_BitmapEntryPtr bm_address;
        short bm_width;
        short bm_height;
        short bm_bitsPerPixel;
} BitMap;

typedef short a_Bitmap[5];

typedef short a_FontWidthEntryPtr[2];

typedef struct _FontData {
        a_Bitmap f_characters;
        short f_firstChar;
        short f_lastChar;
        a_FontWidthEntryPtr f_leftArray;
        short f_baseline;
        short f_spaceIndex;
        short f_fixedWidth;
} FontData;
