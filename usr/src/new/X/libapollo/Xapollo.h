/* $Header: Xapollo.h,v 10.1 86/11/29 13:53:26 jg Rel $ */
    /*

    Copyright 1986 by the University of Utah

    Permission to use, copy, modify, and distribute this
    software and its documentation for any purpose and without
    fee is hereby granted, provided that the above copyright
    notice appear in all copies and that both that copyright
    notice and this permission notice appear in supporting
    documentation, and that the name of the University of Utah
    not be used in advertising or publicity pertaining to 
    distribution of the software without specific, written 
    prior permission. The University of Utah makes no
    representations about the suitability of this software for
    any purpose.  It is provided "as is" without express or
    implied warranty.

    */

#include <stdio.h>
#include "/sys/ins/base.ins.c"
#include "/sys/ins/error.ins.c"
#include "/sys/ins/gpr.ins.c"
#include "/sys/ins/pad.ins.c"
#include "/sys/ins/streams.ins.c"
#include "/sys/ins/ms.ins.c"
#include <sys/types.h>

#include <X/X.h>
#include "../X/vsinput.h"
#include "../X/Xdev.h"

#define dprintf if(Xdbg) fprintf

#define ConstantPixmap 0x0	/* kinds of pixmaps on apollo, constant */
#define BitmapPixmap 0x1	/* and ones with associated bitmaps   */
#define	ZColorPixmap 0x2	/* and (up to) 8-bit color ones */
#define XYColorPixmap 0x3	/* in both styles (but they're not done yet) */
#define CanBeTiled 1		/* this pixmap can be tiled	      */
#define CannotBeTiled 0		/* this pixmap cannont be tiled	      */

#define InvertFlag 0x10		/* pixmap source should be inverted   */
#define PTYPE(x) (((int) (x)->kind) & 0xf)
#define PINVERT(x) (((int) (x)->kind) >> 4)

typedef enum { apollo_bitmap, memory_bitmap } BitmapType;
#define BM_TYPE(bm)     ( (BitmapType)((bm)->kind) )
#define A_BITMAP(bm)    ( (int)((bm)->data ) )
#define M_BITMAP(bm)    ( (short *)((bm)->data) )


typedef struct _fontpriv {
	int maxwidth;		/* maximum width found in the font */
	short *widths;		/* width table (in pixels)	   */
	short *leftarray;	/* leftarray			   */
	boolean ap_font;	/* true->Apollo format font	   */
	int ap_font_id;		/* the font id returned by gpr_$load_font */
	int ap_font_id_inv;	/* ditto for the inverse font      */
	BITMAP *strike;		/* the font bitmap (not for Apollo format) */
	long *chrs;		/* chars in independent bitmaps	   */
} FontPriv;

typedef struct _curspriv {
        BITMAP *bits;		/* the cursor image	*/
	BITMAP *mask;		/* the mask bitmap	*/
	BITMAP *save;		/* a place to save what the cursor covers */
	int fore, back;
} CursPriv;

#define CDATA(x) ((CursPriv *) x->data)

#define PDATA(x) ((BITMAP *) x->data)

#define CURSOR_WIDTH	16
#define CURSOR_HEIGHT	16

#define GetNextClip(clips, win) \
	win.x_coord = clips->left; \
	win.y_coord = clips->top; \
	win.x_size = clips->width; \
	win.y_size = clips->height; \
	clips++;

#define	OverLap(win, x2, y2, w2, h2)	\
	((win.x_coord < x2+w2) && (win.y_coord < y2+h2) && \
	 (x2 < win.x_coord+win.x_size) && (y2 < win.y_coord+win.y_size))

#define imin(i,j)	((i)<(j)?(i):(j))

#define imax(i,j)	((i)>(j)?(i):(j))

#define set_zmask( zmask ) \
        { extern long old_zmask; \
        if (old_zmask != zmask) { \
	  gpr_$set_plane_mask((gpr_$mask_t)(zmask & Screen.plane_mask), status); \
	  old_zmask = zmask; \
	  } \
	}

#define set_op( op ) \
    { extern int old_op; \
    if (old_op != op) { \
       for (i=0; i<Screen.depth; i++) \
	  gpr_$set_raster_op( (gpr_$plane_t)i, (gpr_$raster_op_t)op, status ); \
       old_op = op; \
       } \
     }

#define CheckCursor(basex, basey, w, h) \
    { extern int CursorDisplayed; \
      extern DEVICE *CurrentDevice; \
      extern CURSOR *CurrentCursor; \
      int mousex = CurrentDevice->mouse->x; \
      int mousey = CurrentDevice->mouse->y; \
      if (CursorDisplayed && \
	  (basex < mousex+CurrentCursor->width) && \
	  (basey < mousey+CurrentCursor->height) && \
	  (mousex < basex+w) && (mousey < basey+h)) \
	    DisplayCursor(NULL); \
    }

#define RestoreCursor() \
    { extern int CursorDisplayed; \
      extern CURSOR *CurrentCursor; \
      extern DisplayCursor(); \
      if (!CursorDisplayed) \
    	DisplayCursor(CurrentCursor); \
    }

extern struct Scr {
    gpr_$plane_t depth;
    gpr_$mask_t plane_mask;
    short width;                /*  screen width */
    short height;               /*  screen height */
    short line_width;           /*  halfwords between scan lines */
    caddr_t ptr;		/*  memory ptr to bitmap */
    stream_$id_t fd;            /*  file descriptor for screen pad */
    gpr_$bitmap_desc_t bm;      /*  bitmap descriptor */
    gpr_$attribute_desc_t ab;   /*  attribute descriptor */
} Screen;

BITMAP * make_bitmap();         

extern int Xdbg;


