#ifndef lint
static char *rcsid_draw_c = "$Header: draw.c,v 10.3 86/11/29 13:47:42 jg Rel $";
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
static char sccsid[] = "@(#)draw.c 2.2 86/01/29 Copyright 1986 Sun Micro";
#endif

/*-
 * Copyright (c) 1986 by Sun Microsystems,  Inc.
 */

/* draw.c	Draw lines, curves, and polygons on the screen
 *
 *	DrawCurve	Draw a generalized line/polygon/combination
 *
 */

/*
 *	ToDo:
 *		Brush shapes
 *		Dash/Pattern
 *		Curves
 */

#include "Xsun.h"

extern struct pixrect *PixRect;

static
Draw_Solid(srcpix, xbase, ybase, op, vertcount, verts, clipcount, clips, zmask)
    int srcpix;	/* Pixel value to write */
    int xbase, ybase;	/* Origin of curve */
    int op;		/* Opcode */
    int vertcount;	/* Length of Vertex array */
    Vertex *verts;	/* Vertices */
    int clipcount;	/* Length of clip array */
    CLIP *clips;	/* clipping rectangles */
    int zmask;
{
    int allmask = -1;

    SetZmask(PixRect, &zmask);
    do {
	struct pixrect *region;
	int         cleft, ctop, cwidth, cheight;
	register int xp, yp;
	register Vertex *v = verts;
	register int vc = vertcount;

	GetNextClip(clips, cleft, ctop, cwidth, cheight);
	CheckCursor(cleft, ctop, cwidth, cheight);
	do {
	    register    xo = xp, yo = yp;

	    if (v->flags & VertexRelative) {
		xp += v->x;
		yp += v->y;
	    }
	    else {
		xp = v->x + xbase;
		yp = v->y + ybase;
	    }
	    /* XXX - ignore VertexCurved for now */
	    /* XXX - ignore VertexDrawLastPoint for now */
	    if (!(v->flags & VertexDontDraw)) {
		register int xmin, ymin, xsize, ysize;

		xmin = imin(xo, xp);
		ymin = imin(yo, yp),
		    xsize = imax(xo, xp) - imin(xo, xp) + 1;
		ysize = imax(yo, yp) - imin(yo, yp) + 1;
		if (OverLap(xmin, ymin, xsize, ysize, cleft, ctop, cwidth, cheight)) {
		    /* Its at least partly visible */
		    if (xmin < cleft || ymin < ctop
			|| (xmin + xsize) > (cleft + cwidth)
			|| (ymin + ysize) > (ctop + cheight)) {
			/* Its clipped - we lose */
			region = pr_region(PixRect, cleft, ctop, cwidth, cheight);	/* XXX - slow!!! */
			pr_vector(region, xo - cleft, yo - ctop, xp - cleft, yp - ctop, op, srcpix);
			pr_destroy(region);
		    }
		    else {
			/* It isnt clipped */
			pr_vector(PixRect, xo, yo, xp, yp, op, srcpix);
		    }
		}
	    }
	    v++;
	} while (--vc > 0);
    } while (--clipcount > 0);
    RestoreCursor();
    SetZmask(PixRect, &allmask);
}

DrawCurve (verts, vertcount, xbase, ybase, srcpix, altpix, mode,
	   bwidth, bheight, pat, patlen, patmul, clips, clipcount, func, zmask)
	Vertex *verts;
	int vertcount, xbase, ybase, srcpix, altpix, mode, bwidth, bheight;
	int pat, patlen, patmul, clipcount, zmask;
	register int func;
	CLIP *clips;
{
    int op = SUN_FROM_X_OP(func);
    if (bwidth == 1 && bheight == 1)
	switch (mode) {
	    /* XXX - ignores dash/pattern for now */
	case DrawSolidLine:
	    Draw_Solid(srcpix, xbase, ybase, op,
		       vertcount, verts, clipcount, clips, zmask);
	    break;
	case DrawDashedLine:
	    Draw_Solid(srcpix, xbase, ybase, op,
		       vertcount, verts, clipcount, clips, zmask);
	    break;
	case DrawPatternedLine:
	    Draw_Solid(srcpix, xbase, ybase, op,
		       vertcount, verts, clipcount, clips, zmask);
	    break;
	}
    else
	switch (mode) {
	    /* XXX - ignores brush specification for now */
	case DrawSolidLine:
	    Draw_Solid(srcpix, xbase, ybase, op,
		       vertcount, verts, clipcount, clips, zmask);
	    break;
	case DrawDashedLine:
	    Draw_Solid(srcpix, xbase, ybase, op,
		       vertcount, verts, clipcount, clips, zmask);
	    break;
	case DrawPatternedLine:
	    Draw_Solid(srcpix, xbase, ybase, op,
		       vertcount, verts, clipcount, clips, zmask);
	    break;
	}
}
#endif	sun
