#ifndef lint
static char *rcsid_tile_c = "$Header: tile.c,v 10.2 86/02/01 16:21:30 tony Rel $";
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
static char sccsid[] = "@(#)tile.c 2.1 86/01/28 Copyright 1986 Sun Micro";
#endif

/*-
 * Copyright (c) 1986 by Sun Microsystems,  Inc.
 */

/* tile.c	Perform a raster operation involving a pattern
 *
 *	TileFill	Patterns a portion of the screen
 *	DrawFilled	Draw a filled generalized line/polygon/combination
 *
 */

/*
 *	ToDo:
 *		Implement draw filled
 *		Implement tile fill with xymap
 *		Use static pixrects
 */

#include "Xsun.h"

extern struct pixrect *PixRect;

char *Xalloc();

static
PixrectFill(Tile, xymask, dstx, dsty, width, height, op, clips, clipcount, xoff, yoff)
    struct pixrect *Tile;
    BITMAP *xymask;
    int dstx, dsty, width, height;
    unsigned op;
    CLIP *clips;
    int	clipcount;
    int xoff, yoff;
{
    if (xymask == NULL) {
	/* spread tile from (dstx,dsty) by (width,height) */
	do {
	    int         cleft, ctop, cwidth, cheight;

	    GetNextClip(clips, cleft, ctop, cwidth, cheight);
	    if (OverLap(cleft, ctop, cwidth, cheight, dstx, dsty, width, height)) {
		int         tleft = (cleft > dstx ? cleft : dstx);
		int         ttop = (ctop > dsty ? ctop : dsty);
		int         twidth = (cleft + cwidth < dstx + width ? cleft + cwidth : dstx + width) - tleft;
		int         theight = (ctop + cheight < dsty + height ? ctop + cheight : dsty + height) - ttop;
		/* XXX - is this the right tile mode? */
		CheckCursor(tleft, ttop, twidth, theight);
		pr_replrop(PixRect, tleft, ttop, twidth, theight, op,
		Tile, tleft - xoff, ttop - yoff);
	    }
	} while (--clipcount > 0);
    }
    else {
	/* spread tile thru xymask */
	struct pixrect *stencil = mem_point(xymask->width, xymask->width, 1, xymask->data);

#ifdef	notdef
	do {
	    int         cleft, ctop, cwidth, cheight;

	    GetNextClip(clips, cleft, ctop, cwidth, cheight);
	    if (OverLap(cleft, ctop, cwidth, cheight, dstx, dsty, width, height)) {
		int         tleft = (cleft > dstx ? cleft : dstx);
		int         ttop = (ctop > dsty ? ctop : dsty);
		int         twidth = (cleft + cwidth < dstx + width ? cleft + cwidth : dstx + width) - tleft;
		int         theight = (ctop + cheight < dsty + height ? ctop + cheight : dsty + height) - ttop;
		CheckCursor(tleft, ttop, twidth, theight);
		/* XXX - need combination of stencil & replrop */
		pr_stencil(PixRect, tleft, ttop, twidth, theight, op, &stencil, tleft, ttop, NULL, 0, 0);
	    }
	} while (--clipcount > 0);
#endif
	pr_destroy(stencil);
    }
}

static
ConstantFill(tile, xymask, dstx, dsty, width, height, op, clips, clipcount)
    PIXMAP *tile;
    BITMAP *xymask;
    int dstx, dsty, width, height;
    unsigned op;
    CLIP *clips;
    int	clipcount;
{
    op |= PIX_COLOR((PINVERT(tile) ^ (int) tile->data));
    if (xymask == NULL) {
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
		pr_rop(PixRect, tleft, ttop, twidth, theight, op, NULL, 0, 0);
	    }
	} while (--clipcount > 0);
    }
    else {
	/* spread constant thru xymask */
	struct pixrect *stencil = mem_point(xymask->width, xymask->width, 1, xymask->data);

	do {
	    int         cleft, ctop, cwidth, cheight;

	    GetNextClip(clips, cleft, ctop, cwidth, cheight);
	    if (OverLap(cleft, ctop, cwidth, cheight, dstx, dsty, width, height)) {
		int         tleft = (cleft > dstx ? cleft : dstx);
		int         ttop = (ctop > dsty ? ctop : dsty);
		int         twidth = (cleft + cwidth < dstx + width ? cleft + cwidth : dstx + width) - tleft;
		int         theight = (ctop + cheight < dsty + height ? ctop + cheight : dsty + height) - ttop;

		CheckCursor(tleft, ttop, twidth, theight);
		pr_stencil(PixRect, tleft, ttop, twidth, theight, op, stencil, tleft, ttop, NULL, 0, 0);
	    }
	} while (--clipcount > 0);
	pr_destroy(stencil);
    }
}


/*ARGSUSED*/
TileFill (tile, xoff, yoff, xymask, dstx, dsty, width, height,
	  clips, clipcount, func, zmask)
	PIXMAP *tile;
	BITMAP *xymask;
	int xoff, yoff, dstx, dsty, width, height, zmask;
	register int func;
	CLIP *clips;
{
    int         op = SUN_FROM_X_OP(func) | PIX_DONTCLIP;
    int         allmask = -1;

    if ((PixRect->pr_depth == 1) && !(zmask & 1))
	return;
    SetZmask(PixRect, &zmask);
    switch (PTYPE(tile)) {
    case BitmapPixmap:
	{
	    struct pixrect *Tile =
	    mem_point(tile->width, tile->height, 1, ((BITMAP *) tile->data)->data);	/* XXX - slow !!! */

	    PixrectFill(Tile, xymask, dstx, dsty, width, height, op, clips, clipcount, xoff, yoff);
	    pr_destroy(Tile);
	}
	break;
    case ConstantPixmap:
	ConstantFill(tile, xymask, dstx, dsty, width, height, op, clips, clipcount);
	break;
    case XYColorPixmap:
	/* XXX - not yet implemented - do plane by plane */
	break;
    case ZColorPixmap:
	{
	    struct pixrect *Tile =
	    mem_point(tile->width, tile->height, PixRect->pr_depth, tile->data);	/* XXX - slow !!! */

	    PixrectFill(Tile, xymask, dstx, dsty, width, height, op, clips, clipcount, xoff, yoff);
	    pr_destroy(Tile);
	}
	break;
    }
    SetZmask(PixRect, &allmask);
    RestoreCursor();
}

/*ARGSUSED*/
DrawFilled (verts, vertcount, xbase, ybase, srcpix, tile, xoff, yoff,
	    clips, clipcount, func, zmask)
	Vertex *verts;
	register PIXMAP *tile;
	int vertcount, xbase, ybase, srcpix, xoff, yoff, clipcount, zmask;
	register int func;
	CLIP *clips;
{
	/* XXX - need pr_polygon_2() */
}
#endif	sun
