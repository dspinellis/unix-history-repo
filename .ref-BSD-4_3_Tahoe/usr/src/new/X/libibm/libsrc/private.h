/* $Header: private.h,v 10.1 86/11/19 10:46:15 jg Exp $ */
/* Copyright 1985 Massachusetts Institute of Technology */

/* private.h - Defines and macros to access private data structures
 *
 *  	Changes and modifications by:
 *
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

#include <sys/types.h>
#include <stdio.h>
#include <errno.h>
#include "vsinput.h"
#include "Xdev.h"
#include "X.h"
#include <sys/ioctl.h>
#include <sgtty.h>
#include <fcntl.h>
#include <sys/tbioctl.h>
#include <machineio/mouseio.h>
#include <machinecons/xio.h>

#define ConstantPixmap 	0	/* kinds of pixmaps, constant         */
#define BitmapPixmap 	1	/*   and ones with associated bitmaps */

#define CanBeTiled 	1	/* this pixmap can be tiled */
#define CannotBeTiled 	0	/* this pixmap cannont be tiled */

#define InvertFlag 	0x10	/* pixmap source should be inverted   */
#define PTYPE(x) 	(((int) (x)->kind) & 0xF)	/* get type */
#define PINVERT(x) 	(((int) (x)->kind) >> 4)	/* get invert flag */

#define TILE_SIZE	16	/* tile size in shorts */
#define TILE_WIDTH	16	/* tile width in bits */
#define TILE_HEIGHT	16	/* tile height in bits */

#define CURSOR_SIZE	16	/* cursor size in shorts */
#define CURSOR_WIDTH	16	/* cursor width in bits */
#define CURSOR_HEIGHT	16	/* cursor height in bits */

#define CDATA(x) ((CursPriv *) x->data)	  /* get pntr to private cursor data */

typedef struct _curspriv {	/* private data struct for cursor */
        u_short data[CURSOR_SIZE];	/* cursor image */
        u_short mask[CURSOR_SIZE];	/* cursor mask */
	struct {			/* cursor hot spot */
		short y;
		short x;
	} hotspot;
} CursPriv;

#define FDATA(x) ((FontPriv *) x->data)  /* get pntr to private font data */

typedef struct _fontpriv {	/* private font data */
        int maxwidth;		/* maximum width found in the font */
        short *widths;		/* width table (in pixels)         */
        short *leftarray;	/* leftarray                       */
        BITMAP *chrs;		/* individual character bitmaps    */
        BITMAP *offscr;		/* the font offscreen bitmap       */
} FontPriv;

#define BDATA(x) ((u_short *) x->data)  /* get pntr to private bitmap data */

#define PDATA(x) ((BITMAP *) x->data) /* get pntr to private pixmap data */

/*
 * Define for the global text offscreen array of unsigned shorts.
 * Each offscr BITMAP.data structure will point to this array.
 * When each font is opened, it uses this buffer which is designed to
 * be as wide as the widest display and 128 bits high.
 * This is roughly 20K.  (For all fonts.)
 * (We are assuming some day one will have a 1280x1024 screen.)
 */
#define MAX_OFFSCR_WD		1280
#define MAX_OFFSCR_HT		128
#define TEXT_BUFFER_SIZE 	((MAX_OFFSCR_WD * MAX_OFFSCR_HT) >> 4)
#define CH_THRESHOLD		3

/*
 * Path list type flags
 */

#define DRAW_PATH_LIST	0
#define FILL_PATH_LIST	1

/*
 * These macors are used in place of floating point
 * code to get precision to the second decimal place
 * using interger math.
 */

#define SHIFT_LEFT_16(x) \
	(((x) < 0) ? -(-(x) << 16) : (x) << 16)

#define ROUND_16(x) \
	(((x) < 0) ? -((-(x) + 32768) >> 16) : (((x) + 32768) >> 16))

#define PERCENT_16(x, n, d) \
	(((x) < 0) ? -((-(x) / d) * n) : (((x) / d) * n))

/*
 * Macro to obtain address of the screens
 * BITMAP structure
 */

#define SCREEN_BITMAP	&pbm

/*
 * Externs and declarations
 */

extern BITMAP pbm;
extern BITMAP txtbm;
extern u_short *ConstantTiles[];
extern PIXMAP constpix0;
extern PIXMAP constpix1;
extern char FBMap[];
extern char SSMap[];
extern int errno;

BITMAP *CharBitmap();
BITMAP *MakeMask();
PIXMAP *MakePixmap();
char *Xalloc(), *calloc();
char *strcpy(), *strcat();
char *getenv();
long lseek();
