#ifndef lint
static char *rcsid_tile_c = "$Header: tile.c,v 10.3 86/11/29 13:49:10 jg Rel $";
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

	    if (PINVERT(tile)) {
		op = SUN_FROM_X_OP_INVERT(func) | PIX_DONTCLIP;
	    }
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

/*
 * MAXPOLYGONS is the greatest number of polygons possible. It should be
 *	large, but not too large and is used only when defining npts
 */
#define	MAXPOLYGONS	256

/*ARGSUSED*/
DrawFilled (verts, vertcount, xbase, ybase, srcpix, tile, xoff, yoff,
	    clips, clipcount, func, zmask)
	Vertex *verts;
	register PIXMAP *tile;
	int vertcount, xbase, ybase, srcpix, xoff, yoff, clipcount, zmask;
	register int func;
	CLIP *clips;
{
	struct	pr_pos	*vertices;	/* vertices for pr_polygon_2 */
	register struct pr_pos *tvert;	/* pointer into vertices */
	int		npts[MAXPOLYGONS]; /* # points per polygon */
	int		npgons;		/* number of polygons */
	struct	pixrect	*tilepr;	/* tiling pixrect */
	struct	pixrect	*pgons;	/* polygon pgons */
	struct	pixrect	*srcpr;		/* src for filling tilepr */
	register int	curx,		/* x coord of current vertex */
			cury;		/* y coord of current vertex */
	int		minx,		/* smallest X */
			miny,		/* smallest Y */
			maxx,		/* greatest X */
			maxy,		/* greatest Y */
			lastx,		/* x coord of previous vertex */
			lasty,		/* y coord of previous vertex */
			startx,		/* x coord of first point in pgon */
			starty;		/* y coord of first point in pgon */
	register Vertex	*vp;		/* current vertex */
	register int	cppts;		/* # points in current polygon */
	int		tpts;		/* total # of points in vertices */
	int		inpgon = 0;	/* true if in a polygon */
	int		op;		/* sun pr_ op */
	int		width,		/* width of tilepr && pgons */
			height;		/* height of same */
	int		allmask = -1;	/* all planes */
	extern	u_char	Xstatus;	/* return value for server */

	if ((PixRect->pr_depth == 1) && ! (zmask & 1))
		return;

	vertices = (struct pr_pos *) calloc (vertcount, sizeof (struct pr_pos));
	if (vertices == (struct pr_pos *) NULL) {
		DeviceError ("Couldn't allocate array of vertices");
		Xstatus = BadAlloc;
		return;
	}

	minx = -1;

	for (npgons = 0, vp = verts, tvert = vertices, tpts = cppts = 0;
	     vertcount != 0;
	     vertcount--, vp++) {
		curx = vp->x; cury = vp->y;

		if (vp->flags & VertexRelative) {
			curx += lastx; cury += lasty;
		}
		if (minx == -1) {	/* all uninitialized */
			minx = maxx = curx;
			miny = maxy = cury;
		} else {
			if (curx < minx)
				minx = curx;
			else if (curx > maxx)
				maxx = curx;
			
			if (cury < miny)
				miny = cury;
			else if (cury > maxy)
				maxy = cury;
		}

		if (vp->flags & VertexStartClosed) {
			startx = curx;
			starty = cury;
			inpgon = 1;
		}
		if (vp->flags & VertexEndClosed) {
			if (! inpgon || (curx != startx || cury != starty)) {
				free (vertices);
				DeviceError ("VertexEndClosed and either not in polygon or end doesn't match start");
				Xstatus = BadValue;
				return;
			}
			npts[npgons] = cppts;
			npgons++;
			cppts = 0;
			inpgon = 0;

			if (npgons == MAXPOLYGONS) {
				free (vertices);
				DeviceError ("Too many vertices");
				Xstatus = BadAlloc;
				return;
			}
			continue;
		}

		/*
		 * if not currently converting a polygon, this vertex has
		 * no business being here.
		 */
		if (!inpgon)
			continue;

		tvert->x = curx; tvert->y = cury;
		tvert++; cppts++;
		tpts++;
		lastx = curx; lasty = cury;
	}

	width = maxx - minx + 1;
	height = maxy - miny + 1;

	tilepr = mem_create (width, height, PixRect->pr_depth);
	pgons = mem_create (width, height, PixRect->pr_depth);

	if (tile != NULL) {
		switch (tile->kind) {
		case BitmapPixmap:
			{
			BITMAP	*bm = (BITMAP *) tile->data;

			srcpr = mem_point (bm->width,
						bm->height,
						1,
						bm->data);
			break;
			}
		case ConstantPixmap:
			srcpr = (struct pixrect *) NULL;
			srcpix = (int) tile->data;
			break;
		case XYColorPixmap:
			/* ??? */
			srcpr = (struct pixrect *) NULL;
			srcpix = WhitePixel;
			break;
		case ZColorPixmap:
			srcpr = mem_point (tile->width,
						tile->height,
						PixRect->pr_depth,
						tile->data);
			break;
		}
	} else {
		srcpr = (struct pixrect *) NULL;
	}

	for (tvert = vertices; tpts != 0; tvert++, tpts--) {
		tvert->x -= minx;
		tvert->y -= miny;
	}

	xbase += minx;
	ybase += miny;

	pr_rop (pgons, 0, 0, width, height, PIX_SRC | PIX_DONTCLIP,
		PixRect, xbase, ybase);

	SetZmask (pgons, &zmask);

	op = SUN_FROM_X_OP(func) | PIX_DONTCLIP;

	if (srcpr != (struct pixrect *) NULL) {
		pr_replrop (tilepr, 0, 0, width, height, PIX_SRC,
				srcpr, xoff, yoff);
		pr_destroy (srcpr);
		pr_polygon_2 (pgons, 0, 0, npgons, npts, vertices,
			op,
			tilepr, 0, 0);
	} else {
		pr_polygon_2 (pgons, 0, 0, npgons, npts, vertices,
			PIX_COLOR(srcpix) | op,
			NULL, 0, 0);
	}

	do {
		int	cleft, ctop, cwidth, cheight;

		GetNextClip (clips, cleft, ctop, cwidth, cheight);
		if (OverLap (cleft, ctop, cwidth, cheight,
				xbase, ybase, width, height)) {
			int	tleft = (cleft > xbase ? cleft : xbase);
			int	ttop = (ctop > ybase ? ctop : ybase);
			int	twidth = (cleft + cwidth < xbase + width ?
						cleft + cwidth :
						xbase + width) - tleft;
			int	theight = (ctop + cheight < ybase + height ?
						ctop + cheight :
						ybase + height) - ttop;
			
			CheckCursor (tleft, ttop, twidth, theight);
			pr_rop (PixRect, tleft, ttop, twidth, theight,
					PIX_SRC | PIX_DONTCLIP,
					pgons, tleft - xbase, ttop - ybase);
		}
	} while (--clipcount);

	pr_destroy (pgons);
	pr_destroy (tilepr);

	free (vertices);

	SetZmask (PixRect, &allmask);
	RestoreCursor();
}
#endif	sun
