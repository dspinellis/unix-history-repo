/* Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* gsdps1.c */
/* Display PostScript graphics additions for Ghostscript library */
#include "gx.h"
#include "gserrors.h"
#include "gsmatrix.h"
#include "gspath.h"

/* ------ Graphics state ------ */

/* Set the bounding box for the current path. */
/****** NOT IMPLEMENTED YET. ******/
int
gs_setbbox(gs_state *pgs, floatp llx, floatp lly, floatp urx, floatp ury)
{	if ( llx > urx || lly > ury )
		return_error(gs_error_rangecheck);
	/* Must transform box to device coordinates. */
	/* No-op for now. */
	return 0;
}

/* ------ Rectangles ------ */

/* Append a list of rectangles to a path. */
int
gs_rectappend(gs_state *pgs, gs_rect *pr, uint count)
{	for ( ; count != 0; count--, pr++ )
	   {	floatp px = pr->p.x, py = pr->p.y, qx = pr->q.x, qy = pr->q.y;
		int code;
		/* Ensure counter-clockwise drawing. */
		if ( (qx >= px) != (qy >= py) )
			qx = px, px = pr->p.x;	/* swap x values */
		if ( (code = gs_moveto(pgs, px, py)) < 0 ||
		     (code = gs_lineto(pgs, qx, py)) < 0 ||
		     (code = gs_lineto(pgs, qx, qy)) < 0 ||
		     (code = gs_lineto(pgs, px, qy)) < 0 ||
		     (code = gs_closepath(pgs)) < 0
		   )
			return code;
	   }
	return 0;
}

/* Clip to a list of rectangles. */
int
gs_rectclip(gs_state *pgs, gs_rect *pr, uint count)
{	int code;
	if ( (code = gs_newpath(pgs)) < 0 ||
	     (code = gs_rectappend(pgs, pr, count)) < 0 ||
	     (code = gs_clip(pgs)) < 0 ||
	     (code = gs_newpath(pgs)) < 0
	   )
		return code;
	return 0;
}

/* Fill a list of rectangles. */
/* (We could do this a lot more efficiently.) */
int
gs_rectfill(gs_state *pgs, gs_rect *pr, uint count)
{	int code;
	if ( (code = gs_gsave(pgs)) < 0 ) return code;
	if ( (code = gs_newpath(pgs)) < 0 ||
	     (code = gs_rectappend(pgs, pr, count)) < 0 ||
	     (code = gs_fill(pgs)) < 0
	   )
		;
	gs_grestore(pgs);
	return code;
}

/* Stroke a list of rectangles. */
/* (We could do this a lot more efficiently.) */
int
gs_rectstroke(gs_state *pgs, gs_rect *pr, uint count, gs_matrix *pmat)
{	int code;
	if ( (code = gs_gsave(pgs)) < 0 ) return code;
	if ( (code = gs_newpath(pgs)) < 0 ||
	     (code = gs_rectappend(pgs, pr, count)) < 0 ||
	     (pmat != NULL && (code = gs_concat(pgs, pmat)) < 0) ||
	     (code = gs_stroke(pgs)) < 0
	   )
		;
	gs_grestore(pgs);
	return code;
}
