#ifndef lint
static char *rcsid_draw_c = "$Header: draw.c,v 10.1 86/11/29 13:51:24 jg Rel $";
#endif	lint
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

/* draw.c	Draw lines, curves, and polygons on the screen
 *
 *	DrawCurve	Draw a generalized line/polygon/combination
 *
 */

/*
 *	ToDo:
 *		Brush shapes
 *              Patterned Curves
 *		Curves
 */

#include "Xapollo.h"
status_$t status;

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
    register Vertex *v = verts;
    char *kind;
    int i;
    int allmask = -1;
    gpr_$window_t cwindow;

    gpr_$set_draw_value((gpr_$pixel_value_t)srcpix, status);
    set_zmask( zmask );
    set_op( op );
    do {
    	register int xp, yp;
	register Vertex *v = verts;
    	register int vc = vertcount;

        GetNextClip(clips, cwindow);
        CheckCursor(cwindow.x_coord, cwindow.y_coord,
                    cwindow.x_size, cwindow.y_size);
        gpr_$set_clip_window( cwindow, status);   
    	do {

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

	    if (v->flags & VertexDontDraw)
	        gpr_$move((short)xp, (short)yp, status);
	    else
	        gpr_$line((short)xp, (short)yp, status);

	    v++;
	  } while (--vc > 0);
      } while (--clipcount > 0);
    RestoreCursor();   
}

DrawCurve (verts, vertcount, xbase, ybase, srcpix, altpix, mode,
	   bwidth, bheight, pat, patlen, patmul, clips, clipcount, func, zmask)
	Vertex *verts;
	int vertcount, xbase, ybase, srcpix, altpix, mode, bwidth, bheight;
	int pat, patlen, patmul, clipcount, zmask;
	register int func;
	CLIP *clips;
{
    static gpr_$line_pattern_t pattern = {0, 0, 0, 0};
    int op = func;

    pattern[0] = pat; /* must invert bits ? */ 
    if (mode == DrawSolidLine) patlen = 0;
    gpr_$set_line_pattern((short)patmul, pattern, (short)patlen, status);
    if (bwidth == 1 && bheight == 1)
	switch (mode) {
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
