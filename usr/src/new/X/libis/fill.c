/*
 *	$Source: /u1/X/libis/RCS/fill.c,v $
 *	$Header: fill.c,v 1.1 86/11/17 14:34:04 swick Rel $
 */

#ifndef lint
static char *rcsid_fill_c = "$Header: fill.c,v 1.1 86/11/17 14:34:04 swick Rel $";
#endif	lint

#include "is-copyright.h"

/*	fill.c		Perform a simple raster operation a section of the
 *			screen
 *
 *	PixFill		Do a function on the screen
 *
 *	Copyright (c) 1986, Integrated Solutions, Inc.
 */

#include "Xis.h"

PixFill(srcpix, xymask, dstx, dsty, width, height, clips, clipcount, func, zmask)
int		srcpix;
BITMAP		*xymask;
register int	dstx, dsty;
int		width, height;
register CLIP	*clips;
register int	clipcount;
int		func;
int		zmask;
{
    CLIP bounds, i;
    PIXMAP *fillpix;
    extern PIXMAP *MakePixmap();

#ifdef DEBUG
if (debug & D_PixFill)
    printf("PixFill(srcpix=0x%x, xymask=0x%x, dstx=%d, dsty=%d,\n	width=%d, height=%d, clips=0x%x, clipcount=%d, func=%d, zmask=0x%04x)\n",
	srcpix, xymask, dstx, dsty, width, height, clips, clipcount,
	func, zmask);
if (debug & D_PixFill_data)
    printf_bitmap("xymask", xymask);
#endif DEBUG

    fillpix = MakePixmap((BITMAP *)NULL, srcpix, 0);

    bounds.top = dsty;
    bounds.left = dstx;
    bounds.width = width;
    bounds.height = height;

    for ( ; clipcount; clipcount--, ++clips) {
	if (Overlap(clips[0], bounds)) {
	    i = Intersection(clips[0], bounds);
	    CheckCursor(i);
	    GIP_RasterOp((unsigned char)func,
		fillpix, 0, 0,
		&ScreenPixmap, i.left, i.top,
		xymask, i.left - dstx, i.top - dsty,
		i.width, i.height,
		zmask);
	}

    }
    RestoreCursor();
    if (!--fillpix->refcnt)
	FreePixmap (fillpix);
}
