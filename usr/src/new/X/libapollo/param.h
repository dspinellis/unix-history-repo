/* $Header: param.h,v 10.1 86/11/29 13:53:49 jg Rel $ */
/* $Header: param.h,v 10.1 86/11/29 13:53:49 jg Rel $ */
/* param.h	Definitions for primitive objects in Workstation Graphics
 *		Architecture
 *
 *	Each object has two definitions.  The more human-readable one
 *	has "reasonable" definitions, the one beginning with a_ expresses
 *	the structure as an array of shorts so that the C compiler doesn't
 *	move it around for silly alignment reasons.
 *
 * Author:	Paul J. Asente
 * 		Digital Equipment Corporation
 * 		Western Reseach Lab
 * Date:	June 1983
 */

/****************************************************************************
 *									    *
 *  Copyright (c) 1983, 1984 by						    *
 *  DIGITAL EQUIPMENT CORPORATION, Maynard, Massachusetts.		    *
 *  All rights reserved.						    *
 * 									    *
 *  This software is furnished on an as-is basis and may be used and copied *
 *  only with inclusion of the above copyright notice. This software or any *
 *  other copies thereof may be provided or otherwise made available to     *
 *  others only for non-commercial purposes.  No title to or ownership of   *
 *  the software is hereby transferred.					    *
 * 									    *
 *  The information in this software is  subject to change without notice   *
 *  and  should  not  be  construed as  a commitment by DIGITAL EQUIPMENT   *
 *  CORPORATION.							    *
 * 									    *
 *  DIGITAL assumes no responsibility for the use  or  reliability of its   *
 *  software on equipment which is not supplied by DIGITAL.		    *
 * 									    *
 *									    *
 ****************************************************************************/

#ifndef WGA_PARAMS
#define WGA_PARAMS

typedef short Constant;

typedef struct _Point {
	short p_x;
	short p_y;
} Point;
typedef Point *PointPtr;

typedef short a_Point[2];
typedef short a_PointPtr[2];

typedef struct _Extent {
	short e_width;
	short e_height;
} Extent;

typedef short a_Extent[2];

typedef struct _Rectangle {
	a_Point r_origin;
	a_Extent r_size;
} Rectangle;
typedef Rectangle *RectanglePtr;

typedef short a_Rectangle[4];
typedef short a_RectanglePtr[2];

#define r_x r_origin[0]
#define r_y r_origin[1]
#define r_width r_size[0]
#define r_height r_size[1]

typedef struct _RectangleList {
	a_RectanglePtr r_first;
	short r_count;
} RectangleList;

typedef short a_RectangleList[3];

typedef short BitmapEntry;
typedef BitmapEntry *BitmapEntryPtr;

typedef short a_BitmapEntryPtr[2];

typedef struct _Bitmap {
	a_BitmapEntryPtr bm_address;
	short bm_width;
	short bm_height;
	short bm_bitsPerPixel;
} BitMap;

typedef short a_Bitmap[5];

typedef struct _SubBitmap {
	a_Bitmap sb_base;
	a_Point sb_offset;
} SubBitmap;

typedef short a_SubBitmap[7];

#define sb_address sb_base
#define sb_width sb_base[2]
#define sb_height sb_base[3]
#define sb_bitsPerPixel sb_base[4]
#define sb_x sb_offset[0]
#define sb_y sb_offset[1]

typedef struct _Halftone {
	a_Bitmap ht_base;
	a_Point ht_offset;
} Halftone;

typedef short a_Halftone[7];

#define ht_address ht_base
#define ht_width ht_base[2]
#define ht_height ht_base[3]
#define ht_bitsPerPixel ht_base[4]
#define ht_x ht_offset[0]
#define ht_y ht_offset[1]

typedef long Map;
typedef Map *MapPtr;
typedef short a_Map[2];
typedef short a_MapPtr[2];

typedef struct _Segment {
	a_Point seg_p;
	short seg_flags;
} Segment;
typedef Segment *SegmentPtr;

typedef short a_Segment[3];
typedef short a_SegmentPtr[2];

#define seg_x seg_p[0]
#define seg_y seg_p[1]

/* Meanings of bits in seg_flags.  Bit on means the predicate is true */

#define seg_relative 0x0001
#define seg_dontDraw 0x0002
#define seg_curved 0x0004
#define seg_startClosed 0x0008
#define seg_endClosed 0x0010
#define seg_drawLastPoint 0x0020

typedef struct _SegmentList {
	a_SegmentPtr seg_first;
	short seg_count;
} SegmentList;

typedef short a_SegmentList[3];

typedef struct _PatternString {
	short p_length;
	unsigned short p_pattern;
	short p_multiplier;
} PatternString;

typedef short a_PatternString[3];

typedef struct _PatternState {
	short p_position;
	short p_count;
} PatternState;
typedef PatternState *PatternStatePtr;

typedef short a_PatternState[2];
typedef short a_PatternStatePtr[2];

typedef short FontWidthEntry;
typedef FontWidthEntry *FontWidthEntryPtr;
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
typedef FontData *FontPtr;

typedef short a_FontData[12];
typedef short a_FontPtr[2];

typedef char *CharPtr;
typedef short a_CharPtr[2];

typedef struct _TextString {
	a_CharPtr t_first;
	short t_count;
} TextString;

typedef short a_TextString[3];

typedef short ControlElement;
typedef ControlElement *ControlElementPtr;
typedef short a_ControlElementPtr[2];

typedef struct _ControlString {
	a_ControlElementPtr c_first;
	short c_count;
} ControlString;

typedef short a_ControlString[3];

typedef long MemSize;
typedef short a_MemSize[2];

typedef struct _MemArea {
	a_CharPtr m_base;
	a_MemSize m_size;
} MemArea;

typedef short a_MemArea[4];



#endif
