/* $XConsortium: XWDFile.h,v 1.14 91/01/06 12:18:26 rws Exp $ */
/* Copyright 1985, 1986, Massachusetts Institute of Technology */

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

/*
 * XWDFile.h	MIT Project Athena, X Window system window raster
 *		image dumper, dump file format header file.
 *
 *  Author:	Tony Della Fera, DEC
 *		27-Jun-85
 * 
 * Modifier:    William F. Wyatt, SAO
 *              18-Nov-86  - version 6 for saving/restoring color maps
 */


/*
 * This is not portable between machines of differing word sizes.  To make
 * it portable, do the following things:
 *
 *     o  #include <X11/Xmd.h>
 *     o  remove the typedef for xwdval
 *     o  replace all instances of xwdval with the appropriate CARD32 ... B32
 *     o  make sure that XWDFileHeader is padded to quadword boundaries
 *     o  make sure the window name is written out quadword aligned
 *     o  create an XWDColor structure that contains the same fields as XColor
 *        but which is defined in terms of CARD32 B32, CARD16 B16, and CARD8
 *     o  convert XColor structures to XWDColor structures in xwd
 *     o  remove all xwdval casts from xwd
 *     o  pack image data before writing out if necessary
 *     o  replace casts from xwdval objects in xwud with cvtINT macros
 *     o  convert XWDColor structures to XColor structures
 *     o  unpack data after reading in if necessary
 */


#include <X11/Xmd.h>

#define XWD_FILE_VERSION 7
#ifdef WORD64
#define sz_XWDheader 104
#else
#define sz_XWDheader 100
#endif
#define sz_XWDColor 12

typedef CARD32 xwdval;		/* for old broken programs */

typedef struct _xwd_file_header {
	CARD32 header_size B32;  /* Size of the entire file header (bytes). */
	CARD32 file_version B32;	/* XWD_FILE_VERSION */
	CARD32 pixmap_format B32;	/* Pixmap format */
	CARD32 pixmap_depth B32;	/* Pixmap depth */
	CARD32 pixmap_width B32;	/* Pixmap width */
	CARD32 pixmap_height B32;	/* Pixmap height */
	CARD32 xoffset B32;		/* Bitmap x offset */
	CARD32 byte_order B32;		/* MSBFirst, LSBFirst */
	CARD32 bitmap_unit B32;		/* Bitmap unit */
	CARD32 bitmap_bit_order B32;	/* MSBFirst, LSBFirst */
	CARD32 bitmap_pad B32;		/* Bitmap scanline pad */
	CARD32 bits_per_pixel B32;	/* Bits per pixel */
	CARD32 bytes_per_line B32;	/* Bytes per scanline */
	CARD32 visual_class B32;	/* Class of colormap */
	CARD32 red_mask B32;		/* Z red mask */
	CARD32 green_mask B32;		/* Z green mask */
	CARD32 blue_mask B32;		/* Z blue mask */
	CARD32 bits_per_rgb B32;	/* Log2 of distinct color values */
	CARD32 colormap_entries B32;	/* Number of entries in colormap */
	CARD32 ncolors B32;		/* Number of Color structures */
	CARD32 window_width B32;	/* Window width */
	CARD32 window_height B32;	/* Window height */
	CARD32 window_x B32;		/* Window upper left X coordinate */
	CARD32 window_y B32;		/* Window upper left Y coordinate */
	CARD32 window_bdrwidth B32;	/* Window border width */
#ifdef WORD64
	CARD32 header_end B32;		/* Pad to fill out word */
#endif
} XWDFileHeader;

typedef struct {
        CARD32	pixel B32;
        CARD16	red B16;
	CARD16	green B16;
	CARD16	blue B16;
        CARD8	flags;
        CARD8	pad;
} XWDColor;
