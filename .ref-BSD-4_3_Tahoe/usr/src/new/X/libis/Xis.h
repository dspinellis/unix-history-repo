/*
 *	$Source: /u1/X/libis/RCS/Xis.h,v $
 *	$Header: Xis.h,v 1.1 86/11/17 14:34:54 swick Rel $
 */

#include "is-copyright.h"

/*
 *	Xis.h
 *
 *      Copyright (c) 1986, Integrated Solutions, Inc.
 */

#include <sys/types.h>
#include <stdio.h>
#include <vt/vt_hdrs.h>
#include <vt/vt_output.h>
#include "../X/vsinput.h"
#include "../X/Xdev.h"
#include "../X/X.h"
#define	NOGIPSTRUCTS
#include <gip.h>

/*
 *	misc useful stuff
 */
#ifndef FALSE

typedef short bool;
#define FALSE		(0)
#define TRUE		(!FALSE)

#endif

#define TILE_WIDTH	16
#define TILE_HEIGHT	16
#define CanBeTiled	1	/* this pixmap can be tiled		*/
#define CannotBeTiled	0	/* this pixmap cannont be tiled		*/

/*
 *	FontPriv
 */
#define FONTPIXMAPS	2

typedef struct _fontpriv {
    BITMAP *mask;
    short *xpos;
    short *widths;
    struct _font_pixmaps {
	int fore, back;
	PIXMAP *p;
    } font_pixmaps[FONTPIXMAPS];
    int next_pixmap;
} FontPriv;


/*
 *	CursPriv
 */
typedef struct _curspriv {
    PIXMAP *image;
    BITMAP *mask;
    PIXMAP *save;
    int func;
    int fore, back;
} CursPriv;

/*
 *	Macros to extract "private" data from system structs
 */
#define FDATA(x) ((FontPriv *) x->data)
#define PDATA(x) ((BITMAP *) x->data)
#define CDATA(x) ((CursPriv *) x->data)

extern CLIP	Intersection();
extern char	*Xalloc();
extern BITMAP	ScreenBitmap;
extern PIXMAP	ScreenPixmap;

#ifdef DEBUG
extern unsigned long	debug;
#define	D_None		(0L)
#define	D_All		(~0L)
#define	D_Misc		(1 << 0)
#define	D_Bitmaps	(1 << 1)
#define	D_Color		(1 << 2)
#define	D_CopyArea	(1 << 3)
#define	D_Cursor	(1 << 4)
#define	D_DrawCurve	(1 << 5)
#define	D_Font		(1 << 6)
#define	D_FontPixmap	(1 << 7)
#define	D_PixFill	(1 << 8)
#define	D_PixFill_data	(1 << 9)
#define	D_Pixmaps	(1 << 10)
#define	D_Text		(1 << 11)
#define	D_TileFill	(1 << 12)
#define	D_TileFill_data	(1 << 13)
#endif
