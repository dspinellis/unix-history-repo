/* Copyright    Massachusetts Institute of Technology    1985	*/

#include <sys/types.h>
#include <stdio.h>
#include "../X/vsinput.h"
#include "../X/Xdev.h"
#include "../X/X.h"

#define XSIZE 128		/* number of bytes/line for qvss display */
#define YSIZE 864		/* 864 visible lines on the screen	 */

#define CURSOR_WIDTH 16          /* hardware cursor width              */
#define CURSOR_HEIGHT 16         /* hardware cursor height             */

#define ConstantPixmap 0x0	/* kinds of pixmaps on qvss, constant */
#define BitmapPixmap 0x1	/* and ones with associated bitmaps   */
#define CanBeTiled 1		/* this pixmap can be tiled	      */
#define CannotBeTiled 0		/* this pixmap cannont be tiled	      */

#define InvertFlag 0x10		/* pixmap source should be inverted   */
#define PTYPE(x) (((int) (x)->kind) & 0xf)
#define PINVERT(x) (((int) (x)->kind) >> 4)

typedef struct _curspriv {
	short cbits[16];	/* braindamaged 16x16 cursor on qvss */
	short map;
} CursPriv;

#define CDATA(x) ((CursPriv *) x->data)

typedef struct _fontpriv {
	int maxwidth;		/* maximum width found in the font */
	int wpitch;		/* number of bytes/line in strike  */
	short *widths;		/* width table (in pixels)	   */
	short *leftarray;	/* leftarray			   */
	BITMAP *strike;		/* the font stike bitmap itself	   */
	long *chrs;		/* chars in independent bitmaps	   */
	char **fltable;		/* beginning of each strike line   */
} FontPriv;

#define FDATA(x) ((FontPriv *) x->data)

#define BDATA(x) ((VSArea *) x->data)

#define PDATA(x) ((BITMAP *) x->data)
