/*
 *	$Source: /u1/X/libis/RCS/tile.c,v $
 *	$Header: tile.c,v 1.1 86/11/17 14:34:41 swick Rel $
 */

#ifndef lint
static char *rcsid_tile_c = "$Header: tile.c,v 1.1 86/11/17 14:34:41 swick Rel $";
#endif	lint

#include "is-copyright.h"


/*	tile.c		Perform a raster operation involving a pattern
 *
 *	TileFill	Patterns a portion of the screen
 *	DrawFilled	Draw a filled generalized line/polygon/combination
 *
 *	Copyright (c) 1986, Integrated Solutions, Inc.
 */

#include "Xis.h"

extern PIXMAP ScreenPixmap;

TileFill(tile, xoff, yoff, xymask, dstx, dsty, width, height, clips, clipcount, func, zmask)
PIXMAP		*tile;
int		xoff, yoff;
BITMAP		*xymask;
int		dstx, dsty;
int		width, height;
register CLIP	*clips;
register int	clipcount;
int		func;
int		zmask;
{
    CLIP bounds;

#ifdef DEBUG
if (debug & D_TileFill)
    printf("TileFill(tile=0x%x, xoff=%d, yoff=%d, xymask=0x%x,\n	dstx=%d, dsty=%d, width=%d, height=%d,\n	clips=0x%x, clipcount=%d, func=%d, zmask=0x%04x)\n",
	tile, xoff, yoff, xymask, dstx, dsty, width, height,
	clips, clipcount, func, zmask);
if (debug & D_TileFill_data) {
    printf_pixmap("tile", tile);
    printf_bitmap("xymask", xymask);
}
#endif DEBUG

    bounds.top = dsty;
    bounds.left = dstx;
    bounds.width = width;
    bounds.height = height;
    for ( ; clipcount > 0; clipcount--, ++clips) {

	/* If clip rectangle and destination bounds overlap, display
	 * character in the area the two intersect */
	if (Overlap(clips[0], bounds)) {
	    CLIP i;
	    i = Intersection(clips[0], bounds);
	    CheckCursor(i);
	    GIP_RasterOp((unsigned char)func, tile, xoff, yoff,
			  &ScreenPixmap, i.left, i.top,
			  xymask, i.left - dstx, i.top - dsty,
			  i.width, i.height, zmask);
	}
    }
    RestoreCursor();
}

DrawFilled(verts, vertcount, xbase, ybase, srcpix, tile, xoff, yoff, clips, clipcount, func, zmask)
Vertex		*verts;
register PIXMAP	*tile;
int		vertcount, xbase, ybase, srcpix, xoff, yoff, clipcount, zmask;
register int	func;
CLIP		*clips;
{
/* #ifdef DEBUG
if (debug & D_TileFill) */
    printf("DrawFilled(verts=0x%x, vertcount=%d, xbase=%d, ybase=%d,\n	srcpix=%d, tile=0x%x, xoff=%d, yoff=%d,\n	clips=0x%x, clipcount=%d, func=%d, zmask=0x%04x)\n",
	verts, vertcount, xbase, ybase, srcpix, tile, xoff, yoff,
	clips, clipcount, func, zmask);
/* #endif DEBUG */
}
