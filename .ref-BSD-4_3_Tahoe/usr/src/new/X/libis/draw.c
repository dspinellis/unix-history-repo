/*
 *	$Source: /u1/X/libis/RCS/draw.c,v $
 *	$Header: draw.c,v 1.1 86/11/17 14:33:49 swick Rel $
 */

#ifndef lint
static char *rcsid_draw_c = "$Header: draw.c,v 1.1 86/11/17 14:33:49 swick Rel $";
#endif	lint

#include "is-copyright.h"

/*	draw.c		Draw lines, curves, and polygons on the screen
 *
 *	DrawCurve	Draw a generalized line/polygon/combination
 *
 *	Copyright (c) 1986, Integrated Solutions, Inc.
 */

/*
 *	ToDo:
 *		Dash/Pattern
 *		Curves
 */

#include "Xis.h"

#define	imin(i,j)	((i)<(j)?(i):(j))
#define	imax(i,j)	((i)>(j)?(i):(j))

static
Draw_Solid(srcpix, xbase, ybase, func, vertcount, verts, clipcount, clips, zmask, bwidth, bheight)
int	srcpix;		/* Pixel value to write */
int	xbase, ybase;	/* Origin of curve */
int	func;		/* Opcode */
int	vertcount;	/* Length of Vertex array */
Vertex	*verts;		/* Vertices */
int	clipcount;	/* Length of clip array */
CLIP	*clips;		/* clipping rectangles */
int	zmask;		/* plane mask */
int	bwidth, bheight;/* brush width and height */
{
    PIXMAP *fillpix;
    extern PIXMAP *MakePixmap();

    fillpix = MakePixmap((BITMAP *)NULL, srcpix, 0);
    do {	/* for each clipping region */
	register int xp, yp;
	register Vertex *v = verts;
	register int vc = vertcount;

	do {	/* for each vertex */
	    register    xo = xp, yo = yp;
	    /* first vertex is always VertexDontDraw and !VertexRelative */
	    /* xo,yo do not matter and xp,yp get initialized first time */

	    if (v->flags & VertexRelative) {
		xp += v->x;
		yp += v->y;
	    } else {
		xp = v->x + xbase;
		yp = v->y + ybase;
	    }
	    /* XXX - ignore VertexCurved for now */
	    /* XXX - ignore VertexDrawLastPoint for now */
	    if (!(v->flags & VertexDontDraw)) {
		CLIP bounds;

		/* bounding rectangle of two points */
		bounds.top = imin(yo, yp);
		bounds.left = imin(xo, xp);
		bounds.width = imax(xo, xp) + bwidth - bounds.left;
		bounds.height = imax(yo, yp) + bheight - bounds.top;

		if (Overlap(bounds, clips[clipcount-1])) {
		    /* vector at least partly visible */
		    CLIP i;
		    i = Intersection(clips[clipcount-1], bounds);
		    CheckCursor(i);
		    if ((xo == xp) || (yo == yp)) {
			/* horizontal or vertical vector */
			GIP_RasterOp((unsigned char)func,
			    fillpix, 0, 0,
			    &ScreenPixmap, i.left, i.top,
			    (BITMAP *)0, 0, 0,
			    i.width, i.height, zmask);
		    } else {
			/* non-horizontal and non-vertical vector */
			GIP_Vector((unsigned char)func,
			    fillpix, 0, 0,
			    &ScreenPixmap, xo, yo, xp, yp,
			    i.left, i.top, i.left+i.width-1, i.top+i.height-1,
			    bwidth, bheight, zmask);
		    }
		}
	    }
	    v++;
	} while (--vc > 0);
    } while (--clipcount > 0);
    if (!--fillpix->refcnt)
	FreePixmap (fillpix);
    RestoreCursor();
}

DrawCurve(verts, vertcount, xbase, ybase, srcpix, altpix, mode,
	   bwidth, bheight, pat, patlen, patmul, clips, clipcount, func, zmask)
Vertex	*verts;
int	vertcount, xbase, ybase, srcpix, altpix, mode, bwidth, bheight;
int	pat, patlen, patmul, clipcount, func, zmask;
CLIP	*clips;
{

#ifdef DEBUG
if (debug & D_DrawCurve)
    printf("DrawCurve(verts=0x%x, vertcount=%d, xbase=%d, ybase=%d,\n	srcpix=%d, altpix=%d, mode=%d, bwidth=%d, bheight=%d\n	pat=%d, patlen=%d, patmul=%d\n	clips=0x%x, clipcount=%d, func=%d, zmask=0x%04x)\n",
	verts, vertcount, xbase, ybase, srcpix, altpix, mode, bwidth, bheight,
	pat, patlen, patmul, clips, clipcount, func, zmask);
#endif DEBUG

    switch (mode) { /* XXX - ignores dash/pattern for now */
    case DrawSolidLine:
	Draw_Solid(srcpix, xbase, ybase, func, vertcount, verts,
		    clipcount, clips, zmask, bwidth, bheight);
	break;
    case DrawDashedLine:
	Draw_Solid(srcpix, xbase, ybase, func, vertcount, verts,
		    clipcount, clips, zmask, bwidth, bheight);
	break;
    case DrawPatternedLine:
	Draw_Solid(srcpix, xbase, ybase, func, vertcount, verts,
		    clipcount, clips, zmask, bwidth, bheight);
	break;
    }
}
