#ifndef lint
static char *rcsid_put_c = "$Header: put.c,v 10.3 86/11/29 13:48:41 jg Rel $";
#endif	lint
#ifdef	sun
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

#ifndef	lint
static char sccsid[] = "@(#)put.c 2.1 86/01/28 Copyright 1986 Sun Micro";
#endif

/*-
 * Copyright (c) 1986 by Sun Microsystems,  Inc.
 */

/* put.c	Perform a raster operation with a source bitmap
 *
 *	PixmapPut	Puts a pixmap up on the screen
 *	PixmapBitsPut	Puts a pixmap up on the screen
 *	BitmapBitsPut	Puts a pixmap up on the screen
 *
 */

/*
 *	ToDo:
 *		XYColorPixmap putting & bitsputting
 */

#include "Xsun.h"

extern struct pixrect *PixRect;

char *Xalloc();

PixmapPut (src, srcx, srcy, width, height, dstx, dsty, clips, clipcount,
	   func, zmask)
	register PIXMAP *src;
	int srcx, srcy, width, height, dstx, dsty, clipcount, zmask;
	register int func;
	CLIP *clips;
{
    struct pixrect *Src;
    int op = SUN_FROM_X_OP(func);
    int allmask = -1;

    SetZmask(PixRect, &zmask);
    switch (PTYPE(src)) {
    case BitmapPixmap:
	{
    BITMAP     *bm = (BITMAP *) src->data;
	Src = mem_point(bm->width, bm->height, 1, bm->data);
	do {
	    int         cleft, ctop, cwidth, cheight;

	    GetNextClip(clips, cleft, ctop, cwidth, cheight);
	    if (OverLap(cleft, ctop, cwidth, cheight, dstx, dsty, width, height)) {
		int         tleft = (cleft > dstx ? cleft : dstx);
		int         ttop = (ctop > dsty ? ctop : dsty);
		int         twidth = (cleft + cwidth < dstx + width ? cleft + cwidth : dstx + width) - tleft;
		int         theight = (ctop + cheight < dsty + height ? ctop + cheight : dsty + height) - ttop;
		int         sx = (tleft - dstx) + srcx;
		int         sy = (ttop - dsty) + srcy;

		CheckCursor(tleft, ttop, twidth, theight);
		CheckCursor(sx, sy, twidth, theight);
		if (PINVERT(src)) {
			op = SUN_FROM_X_OP_INVERT(func);
		}
		pr_rop(PixRect, tleft, ttop, twidth, theight, op | PIX_DONTCLIP, Src, sx, sy);
	    }
	} while (--clipcount > 0);
	pr_destroy(Src);
	}
	break;
    case ConstantPixmap:
	/* spread constant from (dstx,dsty) by (width,height) */
	do {
	    int         cleft, ctop, cwidth, cheight;

	    GetNextClip(clips, cleft, ctop, cwidth, cheight);
	    if (OverLap(cleft, ctop, cwidth, cheight, dstx, dsty, width, height)) {
		int         tleft = (cleft > dstx ? cleft : dstx);
		int         ttop = (ctop > dsty ? ctop : dsty);
		int         twidth = (cleft + cwidth < dstx + width ? cleft + cwidth : dstx + width) - tleft;
		int         theight = (ctop + cheight < dsty + height ? ctop + cheight : dsty + height) - ttop;
		CheckCursor(tleft, ttop, twidth, theight);
		/* XXX - is this the right tile mode? */
		pr_rop(PixRect, tleft, ttop, twidth, theight, PIX_SRC | PIX_COLOR(PINVERT(src) ^ (int) src->data), NULL, 0, 0);
	    }
	} while (--clipcount > 0);
	break;
    case ZColorPixmap:
	Src = mem_point(src->width, src->height, PixRect->pr_depth, src->data);
	do {
	    int         cleft, ctop, cwidth, cheight;

	    GetNextClip(clips, cleft, ctop, cwidth, cheight);
	    if (OverLap(cleft, ctop, cwidth, cheight, dstx, dsty, width, height)) {
		int         tleft = (cleft > dstx ? cleft : dstx);
		int         ttop = (ctop > dsty ? ctop : dsty);
		int         twidth = (cleft + cwidth < dstx + width ? cleft + cwidth : dstx + width) - tleft;
		int         theight = (ctop + cheight < dsty + height ? ctop + cheight : dsty + height) - ttop;
		int         sx = (tleft - dstx) + srcx;
		int         sy = (ttop - dsty) + srcy;

		CheckCursor(tleft, ttop, twidth, theight);
		CheckCursor(sx, sy, twidth, theight);
		pr_rop(PixRect, tleft, ttop, twidth, theight, op | PIX_DONTCLIP, Src, sx, sy);
	    }
	} while (--clipcount > 0);
	pr_destroy(Src);
	break;
    case XYColorPixmap:
	/* XXX - not yet implemented - do a plane at a time */
	break;
    }
    RestoreCursor();
    SetZmask(PixRect, &allmask);
}


/*ARGSUSED*/
PixmapBitsPut (width, height, format, data, xymask, dstx, dsty,
	       clips, clipcount, func, zmask)
	char *data;
	int width, height, format, dstx, dsty, clipcount, zmask;
	BITMAP *xymask;
	CLIP *clips;
	int func;
{
    if (PixRect->pr_depth == 1)
	BitsPut(width, height, data, 1, 0, xymask, dstx, dsty,
		clips, clipcount, func, zmask, 1);
    else if (PixRect->pr_depth <= 8)
	switch (format) {
	case ZFormat: {
	    char *newdata;

	    if (width&1) {
		register int i;
		register char *old = data, *new;

		new = newdata = (char *) Xalloc((width+1)*height);
		for (i = 0; i < height; i++) {
		    bcopy(old, new, width);
		    old += width;
		    new += width+1;
	        }
	    } else 
		newdata = data;
	    BitsPut(width, height, newdata, 1, 0, xymask, dstx, dsty,
		    clips, clipcount, func, zmask, PixRect->pr_depth);
	    if (newdata != data)
		free (newdata);
	    }
	    break;
	case XYFormat:
	    /* XXX - not yet supported - do one plane at a time */
	    break;
	}
}

BitmapBitsPut (width, height, data, fore, back, xymask, dstx, dsty,
	       clips, clipcount, func, zmask)
	char *data;
	int width, height, fore, back, dstx, dsty, clipcount, zmask;
	register BITMAP *xymask;
	CLIP *clips;
	register int func;
{
	InvertPixelOrder((short *) data, BitmapSize(width, height) >> 1);
	BitsPut(width, height, data, fore, back, xymask, dstx, dsty,
	       clips, clipcount, func, zmask, 1);
}

static
BitsPut (width, height, data, fore, back, xymask, dstx, dsty,
	       clips, clipcount, func, zmask, srcdepth)
	char *data;
	int width, height, fore, back, dstx, dsty, clipcount, zmask;
	register BITMAP *xymask;
	CLIP *clips;
	register int func;
	int srcdepth;
{
    struct pixrect *Src;
    extern char FBMap[];
    int allmask = -1;
    int op;

    if ((PixRect->pr_depth == 1) && !(zmask & 1))
	return;
    SetZmask(PixRect, &zmask);
    if (fore & 1)
	func += 0x20;
    if (back & 1)
	func += 0x10;

    func = FBMap[func];
    op = SUN_FROM_X_OP(func);

    Src = mem_point(width, height, srcdepth, data);
    if (xymask == NULL) {
	do {
	    int         cleft, ctop, cwidth, cheight;

	    GetNextClip(clips, cleft, ctop, cwidth, cheight);
	    if (OverLap(cleft, ctop, cwidth, cheight, dstx, dsty, width, height)) {
		int         tleft = (cleft > dstx ? cleft : dstx);
		int         ttop = (ctop > dsty ? ctop : dsty);
		int         twidth = (cleft + cwidth < dstx + width ? cleft + cwidth : dstx + width) - tleft;
		int         theight = (ctop + cheight < dsty + height ? ctop + cheight : dsty + height) - ttop;
		int         sx = (tleft - dstx);
		int         sy = (ttop - dsty);

		CheckCursor(cleft, ctop, cwidth, cheight);
		pr_rop(PixRect, tleft, ttop, twidth, theight, op | PIX_DONTCLIP, Src, sx, sy);
	    }
	} while (--clipcount > 0);
    }
    else {
	struct pixrect *stencil;

	stencil = mem_point(xymask->width, xymask->height, 1, xymask->data);
	do {
	    int         cleft, ctop, cwidth, cheight;

	    GetNextClip(clips, cleft, ctop, cwidth, cheight);
	    if (OverLap(cleft, ctop, cwidth, cheight, dstx, dsty, width, height)) {
		CheckCursor(cleft, ctop, cwidth, cheight);
		pr_stencil(PixRect, dstx, dsty, width, height, op,
		    stencil, 0, 0, Src, 0, 0);
	    }
	} while (--clipcount > 0);
	pr_destroy(stencil);
    }
    RestoreCursor();
    pr_destroy(Src);
    SetZmask(PixRect, &allmask);
    return;
}
#endif	sun
