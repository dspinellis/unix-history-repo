/*
 *	$Source: /u1/X/libis/RCS/put.c,v $
 *	$Header: put.c,v 1.1 86/11/17 14:34:25 swick Rel $
 */

#ifndef lint
static char *rcsid_put_c = "$Header: put.c,v 1.1 86/11/17 14:34:25 swick Rel $";
#endif	lint

#include "is-copyright.h"

/*	put.c		Perform a raster operation with a source bitmap
 *
 *	PixmapPut	Puts a pixmap up on the screen
 *	PixmapBitsPut	Puts a pixmap up on the screen
 *	BitmapBitsPut	Puts a pixmap up on the screen
 *
 *	Copyright (c) 1986, Integrated Solutions, Inc.
 */

#include "Xis.h"


PixmapPut(src, srcx, srcy, width, height, dstx, dsty, clips, clipcount, func, zmask)
register PIXMAP	*src;
int		srcx, srcy, width, height, dstx, dsty, zmask;
register int	clipcount;
register int	func;
register CLIP	*clips;
{
    CLIP bounds, i;
    int tflag = 0;

#ifdef DEBUG
if (debug & D_Pixmaps)
    printf("PixmapPut\n");
#endif DEBUG

    if (src->tile) {
	src->tile = CannotBeTiled;
	tflag++;
    }
    bounds.top = dsty;
    bounds.left = dstx;
    bounds.width = width;
    bounds.height = height;

    for ( ; clipcount; clipcount--, ++clips) {
	if (Overlap(clips[0], bounds)) {
	    i = Intersection(clips[0], bounds);
	    CheckCursor(i);
	    GIP_RasterOp((unsigned char)func,
		src, (i.left - dstx) + srcx, (i.top - dsty) + srcy,
		&ScreenPixmap, i.left, i.top,
		(BITMAP *)NULL, 0, 0,
		i.width, i.height,
		zmask);
	}

    }
    RestoreCursor();
    if (tflag)
	src->tile = CanBeTiled;
}


PixmapBitsPut(width, height, format, data, xymask, dstx, dsty, clips, clipcount, func, zmask)
char		*data;
int		width, height, format, zmask;
register int	dstx, dsty;
register int	clipcount;
BITMAP		*xymask;
register CLIP	*clips;
int		func;
{
    PIXMAP *src;
    CLIP bounds, i;
    extern PIXMAP *StorePixmap();

#ifdef DEBUG
if (debug & D_Pixmaps)
    printf("PixmapBitsPut\n");
#endif DEBUG

    bounds.top = dsty;
    bounds.left = dstx;
    bounds.width = width;
    bounds.height = height;

    src = StorePixmap(width, height, format, data);
    src->tile = CannotBeTiled;

    for ( ; clipcount; clipcount--, ++clips) {
	if (Overlap(clips[0], bounds)) {
	    i = Intersection(clips[0], bounds);
	    CheckCursor(i);
	    GIP_RasterOp((unsigned char)func,
		src, (i.left - dstx), (i.top - dsty),
		&ScreenPixmap, i.left, i.top,
		xymask, i.left - dstx, i.top - dsty,
		i.width, i.height,
		zmask);
	}

    }
    RestoreCursor();
    FreePixmap(src);
}


BitmapBitsPut(width, height, data, fore, back, xymask, dstx, dsty, clips, clipcount, func, zmask)
char		*data;
int		width, height, fore, back, zmask;
register int	dstx, dsty;
register int	clipcount;
BITMAP		*xymask;
register CLIP	*clips;
int		func;
{
    BITMAP *bsrc;
    PIXMAP *src;
    CLIP bounds, i;
    extern BITMAP *StoreBitmap();
    extern PIXMAP *MakePixmap();

#ifdef DEBUG
if (debug & D_Bitmaps)
    printf("BitmapBitsPut\n");
#endif DEBUG

    /* first make a src pixmap */
    bsrc = StoreBitmap(width, height, data);
    src = MakePixmap(bsrc, fore, back);
    src->tile = CannotBeTiled;

    bounds.top = dsty;
    bounds.left = dstx;
    bounds.width = width;
    bounds.height = height;

    for ( ; clipcount; clipcount--, ++clips) {
	if (Overlap(clips[0], bounds)) {
	    i = Intersection(clips[0], bounds);
	    CheckCursor(i);
	    GIP_RasterOp((unsigned char)func,
		src, (i.left - dstx), (i.top - dsty),
		&ScreenPixmap, i.left, i.top,
		xymask, i.left - dstx, i.top - dsty,
		i.width, i.height,
		zmask);
	}

    }
    RestoreCursor();
    FreePixmap(src);
    FreeBitmap(bsrc);
}
