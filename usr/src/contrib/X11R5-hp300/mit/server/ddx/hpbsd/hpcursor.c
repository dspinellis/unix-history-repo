/* $XConsortium: hpcursor.c,v 1.5 88/09/06 15:26:07 jim Exp $ */
/*
 * hpcursor.c : hp soft cursor routines
 * C Durland, C Amacher
 */

/*
Copyright (c) 1986, 1987 by Hewlett-Packard Company
Copyright (c) 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD
TO THIS SOFWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
PURPOSE.  Hewlett-Packard shall not be liable for errors 
contained herein or direct, indirect, special, incidental or 
consequential damages in connection with the furnishing, 
performance, or use of this material.

This software is not subject to any license of the American
Telephone and Telegraph Company or of the Regents of the
University of California.
*/

#define NEED_EVENTS	/* hack hack hack */

#include <stdio.h>
#include "X.h"
#include "Xproto.h"
#include "servermd.h"
#include "scrnintstr.h"
#include "cursorstr.h"
#include "hppriv.h"
#include "resource.h"
#include "inputstr.h"
#include "hpext.h"
#include "hildef.h"
#include "XHPproto.h"

extern int lastEventTime;
extern HPInputDevice *hpPointer;

#define MAXCX 64	/* max cursor rectangle width */
#define MAXCY 64	/* max cursor rectangle height */

static Bool	hpRealizeCursor(), hpUnrealizeCursor(), hpDisplayCursor();
static void	hpPointerNonInterestBox(), hpRecolorCursor();

extern Bool	hpSetCursorPosition();
extern void	hpCursorLimits(), hpConstrainCursor();

#ifdef XTESTEXT1
/*
 * defined in xtestext1di.c
 */
extern int	on_steal_input;
/*
 * defined in xtestext1di.c
 */
extern short	xtest_mousex;
/*
 * defined in xtestext1di.c
 */
extern short	xtest_mousey;
#endif /* XTESTEXT1 */

	/* restore screen from off screen mem */
static void
hpCursorOff(pScreen)
     ScreenPtr pScreen;
{
    register hpPrivScreenPtr cfb = getPrivScreenPtr(pScreen);

    if (cfb->cstate == CURSOR_OFF) return;

    cfb->cstate = CURSOR_OFF;
    (*cfb->MoveBits)(pScreen, ~0, GXcopy,
		     cfb->ssaveX,cfb->ssaveY,
		     cfb->saved.x1,cfb->saved.y1, cfb->w,cfb->h);
}

	/* copy area cursor covers to off screen mem, copy cursor to screen */
static void
hpCursorOn(pScreen, dx, dy)
     ScreenPtr pScreen;
     register int dx, dy;	/* the cursor hot spot */
{
    register hpPrivScreenPtr cfb = getPrivScreenPtr(pScreen);
    register int
	srcx = cfb->srcX, srcy = cfb->srcY,
	maskx = cfb->maskX, masky = cfb->maskY,
	w, h;

    if (cfb->cstate == CURSOR_ON) return; /* else cursor is offscreen */

    dx -= cfb->hoffX; dy -= cfb->hoffY;	/* hotspot to upper left corner */
		/* clip cursor rectangle */
    w = min(cfb->width,  pScreen->width -dx);
    h = min(cfb->height, pScreen->height -dy);
    if (dx<0) { srcx -= dx; maskx -= dx; w += dx; dx = 0; }
    if (dy<0) { srcy -= dy; masky -= dy; h += dy; dy = 0; }

    (*cfb->MoveBits)(pScreen, ~0, GXcopy,	/* save screen area */
		     dx,dy, cfb->ssaveX,cfb->ssaveY, w,h);
    cfb->saved.x1 = dx; cfb->saved.y1 = dy;
    cfb->saved.x2 = dx + w; cfb->saved.y2 = dy + h;
    cfb->w = w; cfb->h = h;

	/* mask out cursor shape, put cursor in hole */
    (*cfb->MoveBits)(pScreen, ~0, GXand, maskx,masky, dx,dy, w,h);
    (*cfb->MoveBits)(pScreen, ~0, GXor, srcx,srcy, dx,dy, w,h);

    cfb->cstate = CURSOR_ON;	/* cursor is on screen */
}

	/* move screen cursor hotspot to (hotX,hotY) from wherever it is */
static void
hpMoveMouse(pScreen, hotX,hotY, forceit)
     ScreenPtr pScreen; 
     register int hotX, hotY;
     int forceit;
{
#ifdef XTESTEXT1
    if (on_steal_input)
    {
	/*
	 * only call if the mouse position has actually moved
	 */
	if ((hotX != xtest_mousex) || (hotY != xtest_mousey))
	{
	    XTestStealMotionData((hotX - xtest_mousex),
				 (hotY - xtest_mousey),
				 MOUSE,
				 xtest_mousex,
				 xtest_mousey);
	}
    }
#endif /* XTESTEXT1 */

    if (forceit)
	hpCursorOff(pScreen);
    if (0<=hotX && 0<=hotY && hotX<pScreen->width && hotY<pScreen->height)
	hpCursorOn(pScreen, hotX,hotY);
}

extern void hpSpriteFindColors();

#define tcXY(x,y) /* address of offscreen mem */ \
	(unsigned char *)(cfb->bits +(y)*cfb->stride +(x))

static Bool
hpDisplayCursor(pScreen,pCursor)
     ScreenPtr pScreen;
     CursorPtr pCursor;
{
    register hpPrivScreenPtr cfb = getPrivScreenPtr(pScreen);
    register unsigned char *ctr, *mtr, *ptr, *qtr, z, startbit;
    register short int x,y, w,h;
    int xstart, ystart;
    Pixel fgcolor, bgcolor;

    w = pCursor->bits->width;
    h = pCursor->bits->height;
	/* scale cursor if bigger than max */
    cfb->width = min(w,MAXCX); cfb->height = min(h,MAXCY);
    xstart = (w<=MAXCX) ?
	0 : pCursor->bits->xhot - MAXCX + ((w - pCursor->bits->xhot)*MAXCX)/w;
    startbit = 0x80>>(xstart%8);
    cfb->hoffX = pCursor->bits->xhot - xstart;
    xstart /= 8;
    ystart = (h<=MAXCY) ?
	0 : pCursor->bits->yhot - MAXCY + ((h - pCursor->bits->yhot)*MAXCY)/h;
    cfb->hoffY = pCursor->bits->yhot - ystart;

	/* convert colors to display specific colors */
    hpSpriteFindColors(pScreen, pCursor, &fgcolor, &bgcolor);
    SET_REGISTERS_FOR_WRITING(pScreen,~0,GXcopy);	  /* setup hardware */
	/* bytes in each row of pixmaps (including padding) */
    w = (w+BITMAP_SCANLINE_PAD-1)/BITMAP_SCANLINE_PAD*(BITMAP_SCANLINE_PAD/8);
    for (y=0; y<cfb->height; y++)
    {
	ptr = pCursor->bits->source + w*(ystart+y) + xstart;
	qtr = pCursor->bits->mask + w*(ystart+y) + xstart;
	ctr = tcXY(cfb->srcX, y+cfb->srcY); /* address of offscreen cursor */
	mtr = tcXY(cfb->maskX,y+cfb->maskY); /* address of offscreen cursor mask */
	for (z = startbit, x=0; x<cfb->width; x++)
	{
	    /* cursor mask: all planes = xor(or(all planes)) */
	    *mtr++ = (*qtr & z) ? 0 : 0xFF;
	    /* squeeze cursor through mask */
	    *ctr++ = (*qtr & z) ? ((*ptr & z) ? fgcolor : bgcolor) : 0;
	    if ((z>>=1)==0)
	    {
		z = 0x80; ptr++; qtr++;
	    }
	}
    }
    hpMoveMouse(pScreen, hpPointer->coords[0],hpPointer->coords[1], 1);
    return TRUE;
}

static Bool
hpmDisplayCursor(pScreen,pCursor)
     ScreenPtr pScreen;
     CursorPtr pCursor;
{
    register hpPrivScreenPtr cfb = getPrivScreenPtr(pScreen);
    register unsigned char *ctr, *mtr, *ptr, *qtr, z, z1, startbit;
    register short int x,y, w,h;
    int xstart, ystart;
    Pixel fgcolor, bgcolor;

    w = pCursor->bits->width;
    h = pCursor->bits->height;
	/* scale cursor if bigger than max */
    cfb->width = min(w,MAXCX); cfb->height = min(h,MAXCY);
    xstart = (w<=MAXCX) ?
	0 : pCursor->bits->xhot - MAXCX + ((w - pCursor->bits->xhot)*MAXCX)/w;
    startbit = 0x80>>(xstart%8);
    cfb->hoffX = pCursor->bits->xhot - xstart;
    xstart /= 8;
    ystart = (h<=MAXCY) ?
	0 : pCursor->bits->yhot - MAXCY + ((h - pCursor->bits->yhot)*MAXCY)/h;
    cfb->hoffY = pCursor->bits->yhot - ystart;

	/* convert colors to display specific colors */
    hpSpriteFindColors(pScreen, pCursor, &fgcolor, &bgcolor);
    fgcolor = (fgcolor == 0) ? 0 : 0xFF;
    bgcolor = (bgcolor == 0) ? 0 : 0xFF;
    /* SET_REGISTERS_FOR_WRITING(pScreen,~0,GXcopy);	  /* setup hardware */
	/* bytes in each row of pixmaps (including padding) */
    w = (w+BITMAP_SCANLINE_PAD-1)/BITMAP_SCANLINE_PAD*(BITMAP_SCANLINE_PAD/8);
    for (y=0; y<cfb->height; y++)
    {
	ptr = pCursor->bits->source + w*(ystart+y) + xstart;
	qtr = pCursor->bits->mask + w*(ystart+y) + xstart;
	ctr = tcXY(cfb->srcX/8, y+cfb->srcY); /* address of offscreen cursor */
	mtr = tcXY(cfb->maskX/8,y+cfb->maskY); /* address of offscreen cursor mask */
	for (z = startbit, x=0; x<cfb->width; x+=8)
	{
	    *mtr = *ctr = 0;
	    for (z1=0x80; z1>0; z1>>=1)
	    {
		/* cursor mask: all planes = xor(or(all planes)) */
		*mtr |= ((*qtr & z) ? 0 : 0xFF) & z1;
		/* squeeze cursor through mask */
		*ctr |= ((*qtr & z) ? ((*ptr & z) ? fgcolor : bgcolor) : 0) & z1;
		if ((z>>=1)==0)
		{
		    z = 0x80; ptr++; qtr++;
		}
	    }
	    mtr++; ctr++;
	}
    }
    hpMoveMouse(pScreen, hpPointer->coords[0],hpPointer->coords[1], 1);
    return TRUE;
}

extern Bool hpSpriteInitialize();

Bool
hpInitCursor(pScreen, scrn_depth) 
     ScreenPtr pScreen;
     int scrn_depth;
{
    register hpPrivScreenPtr cfb = getPrivScreenPtr(pScreen);
    hpChunk *chunkie;

    cfb->MoveMouse = hpMoveMouse; cfb->CursorOff = hpCursorOff;
    cfb->cstate = CURSOR_OFF;
	/* allocate 3 max size cursor retangles: 
	 * cursor, cursor mask and screen save area
	 */
    chunkie = hpBufAlloc(pScreen, MAXCX,MAXCY);		/* save area */
    cfb->ssaveX = chunkie->x; cfb->ssaveY = chunkie->y;
    chunkie = hpBufAlloc(pScreen, MAXCX,MAXCY);		/* cursor */
    cfb->srcX = chunkie->x; cfb->srcY = chunkie->y;
    chunkie = hpBufAlloc(pScreen, MAXCX,MAXCY);		/* mask */
    cfb->maskX = chunkie->x; cfb->maskY = chunkie->y;

	/* create a dummy cursor to avoid trashing screen */
    cfb->hoffX = 0; cfb->hoffY = 0; cfb->width = 1; cfb->height = 1;

    pScreen->RealizeCursor = hpRealizeCursor;
    pScreen->UnrealizeCursor = hpUnrealizeCursor;
    if (scrn_depth == 1)
	pScreen->DisplayCursor = hpmDisplayCursor;
    else
	pScreen->DisplayCursor = hpDisplayCursor;
    pScreen->SetCursorPosition = hpSetCursorPosition;
    pScreen->CursorLimits = hpCursorLimits;
    pScreen->PointerNonInterestBox = hpPointerNonInterestBox;
    pScreen->ConstrainCursor = hpConstrainCursor;
    pScreen->RecolorCursor = hpRecolorCursor;

    return hpSpriteInitialize (pScreen);
}

static Bool
hpRealizeCursor(pScreen,pCursor)
     ScreenPtr pScreen; CursorPtr pCursor;
{
    return TRUE;
}

static Bool
hpUnrealizeCursor(pScreen,pCursor)
     ScreenPtr pScreen; CursorPtr pCursor;
{
    return TRUE;
}

	/* hpRecolorCursor -- Change the color of a cursor
	 * Do this by creating a new cursor that has the new colors
	 */
static void 
hpRecolorCursor(pScreen, pCursor, displayed)
     ScreenPtr pScreen; /* Screen for which the cursor is to be recolored */
     CursorPtr pCursor; /* Cursor to recolor */
     Bool displayed;	/* True if pCursor being displayed now */
{
    if (displayed) 
	(void) hpDisplayCursor(pScreen, pCursor);
}

/*
 * hpPointerNonInterestBox --
 *	Set up things to ignore uninteresting mouse events. Sorry.
 */
static void 
hpPointerNonInterestBox(pScreen,pBox)
     ScreenPtr pScreen;
     BoxPtr    pBox;	    /* Box outside of which motions are boring */
{
}
