/*
 * The Sun X drivers are a product of Sun Microsystems, Inc. and are provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify these drivers without charge, but are not authorized
 * to license or distribute them to anyone else except as part of a product or
 * program developed by the user.
 * 
 * THE SUN X DRIVERS ARE PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND
 * INCLUDING THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE
 * PRACTICE.
 *
 * The Sun X Drivers are provided with no support and without any obligation
 * on the part of Sun Microsystems, Inc. to assist in their use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THE SUN X
 * DRIVERS OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

/*-
 * Copyright 1985, Massachusetts Institute of Technology
 * Copyright (c) 1986 by Sun Microsystems,  Inc.
 *
 * @(#)Xsun.h 2.1 86/01/28
 *
 */

#include <stdio.h>
#include <X/X.h>
#include <pixrect/pixrect_hs.h>
#include "../X/vsinput.h"
#include "../X/Xdev.h"


#define ConstantPixmap 0x0	/* kinds of pixmaps on sun, constant */
#define BitmapPixmap 0x1	/* and ones with associated bitmaps   */
#define	ZColorPixmap 0x2	/* and 8-bit color ones */
#define XYColorPixmap 0x3	/* in both styles */
#define CanBeTiled 1		/* this pixmap can be tiled	      */
#define CannotBeTiled 0		/* this pixmap cannont be tiled	      */

#define InvertFlag 0x10		/* pixmap source should be inverted   */
#define PTYPE(x) (((int) (x)->kind) & 0xf)
#define PINVERT(x) (((int) (x)->kind) >> 4)

typedef struct _fontpriv {
	int maxwidth;		/* maximum width found in the font */
	int wpitch;		/* number of bytes/line in strike  */
	short *widths;		/* width table (in pixels)	   */
	short *leftarray;	/* leftarray			   */
	BITMAP *strike;		/* the font stike bitmap itself	   */
	long *chrs;		/* chars in independent bitmaps	   */
	char **fltable;		/* beginning of each strike line   */
} FontPriv;

typedef struct _curspriv {
	struct pixrect *top;	/* The cursor image */
	BITMAP *top_bit;
	struct pixrect *bot;	/* The cursor image */
	BITMAP *bot_bit;
	struct pixrect *sv;	/* The pixels underneath */
	int fore, back, c_func;
} CursPriv;

#define CDATA(x) ((CursPriv *) x->data)

#define F_DATA(x) ((struct pixfont *) x->data)

#define BDATA(x) ((VSArea *) x->data)

#define PDATA(x) ((BITMAP *) x->data)

#define SUN_FROM_X_OP(func)	(Sun_From_X_Op[func])
#define SUN_FROM_X_OP_INVERT(func)    (Sun_From_X_Op[SSMap[func|0x10]])
extern char SSMap[];

short Sun_From_X_Op[];

#define CURSOR_WIDTH	16
#define CURSOR_HEIGHT	16

#define GetNextClip(clips, cleft, ctop, cwidth, cheight) \
    { \
	CLIP       *cl = clips++; \
	cleft = cl->left; \
	ctop = cl->top; \
	cwidth = cl->width; \
	cheight = cl->height; \
    }

#define	OverLap(x1, y1, w1, h1, x2, y2, w2, h2)	\
	((x1 < x2+w2) && (y1 < y2+h2) && (x2 < x1+w1) && (y2 < y1+h1))

#define imin(i,j)	(i<j?i:j)

#define imax(i,j)	(i>j?i:j)

#define SetZmask(pr, zm) { \
	extern private_czmask; \
	if (*(char *)(zm) != (char)private_czmask) { \
		pr_putattributes(pr, (zm)); \
		private_czmask = *(zm); \
	}}
