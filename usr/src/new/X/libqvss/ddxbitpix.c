/* Copyright 1985 Massachusetts Institute of Technology */

/* Routines to cache bitmaps and pixmaps in the frame buffer memory:
 *
 *	StoreBitmap	Creates a bitmap
 *	FreeBitmap	Frees the storage taken by a bitmap
 *	CharBitmap	Creates a bitmap from a font character
 *	StorePixmap	Creates a pixmap
 *	FreePixmap	Frees the storage taken by a pixmap
 *	MakePixmap	Create a pixmap from a bitmap
 *	PixmapSave	Save a region of the screen
 *	PixmapGet	Read a region of the screen
 *
 */

#include "ddxqvss.h"
#include "vstagbl.h"
#include <errno.h>

extern int errno;


char *Xalloc(), *AllocateSpace();
PIXMAP *MakePixmap();

BITMAP *StoreBitmap (width, height, data)
	int width, height;
	char *data;
{
	register BITMAP *bm;
	int size;

	bm = (BITMAP *) Xalloc (sizeof (BITMAP));
	bm->width = width;
	bm->height = height;
	bm->refcnt = 1;

	size = BitmapSize(width, height);
	if ((bm->data = (caddr_t) malloc (size)) == NULL) {
	    free ((caddr_t) bm);
	    return (NULL);
	}

	bcopy (data, bm->data, size);

	return (bm);
}

FreeBitmap (bitmap)
	register BITMAP *bitmap;
{
	free ((caddr_t) bitmap->data);
	free ((caddr_t) bitmap);
}

BITMAP *CharBitmap (c, font)
	unsigned c;
	register FONT *font;
{
	int width;
	register BITMAP *bm;

	if (c < font->first || c > font->last) {
	    errno = EINVAL;
	    return (NULL);
	}
	if (font->fixed)
	    width = font->avg_width;
	else
	    width = FDATA(font)->widths[c];
	if (width == 0) {
	    errno = EINVAL;
	    return (NULL);
	}
	bm = (BITMAP *) Xalloc (sizeof (BITMAP));
	bm->width = width;
	bm->height = font->height;
	bm->refcnt = 1;
	if ((bm->data = 
	    (caddr_t) malloc (BitmapSize(width, bm->height))) == NULL) {
	    free ((caddr_t) bm);
	    errno = ENOMEM;
	    return (NULL);
	}

	CopyText ((caddr_t) &c, 1, font, bm);
	return (bm);
}

/*ARGSUSED*/
PIXMAP *StorePixmap (width, height, format, data)
	int width, height, format;
	char *data;
{
	register BITMAP *bm;
	register PIXMAP *pm;

	bm = (BITMAP *) StoreBitmap (width, height, data);
	if (bm == NULL)
	    return (NULL);
	bm->refcnt = 0;
	if (pm = MakePixmap (bm, 1, 0))
	    return (pm);
	FreeBitmap (bm);
	return (NULL);
}

FreePixmap (pixmap)
	register PIXMAP *pixmap;
{
	register BITMAP *bm;

	if (pixmap->kind) {
	    bm = PDATA(pixmap);
	    if (--bm->refcnt == 0)
		FreeBitmap (bm);
	}
	free ((caddr_t) pixmap);
}

PIXMAP constpix0 = {1, 1, 1, 1, ConstantPixmap, (caddr_t) 0};
PIXMAP constpix1 = {1, 1, 1, 1, ConstantPixmap, (caddr_t) 1};

PIXMAP *MakePixmap (xymask, fore, back)
	register BITMAP *xymask;
	int fore, back;
{
	register PIXMAP *pm;

	if (xymask == NULL || !((fore ^ back) & 1)) {
	    if (fore & 1)
		pm = &constpix1;
	    else
		pm = &constpix0;
	    pm->refcnt++;
	    return (pm);
	}

	pm = (PIXMAP *) Xalloc (sizeof (PIXMAP));
	pm->width = xymask->width;
	pm->height = xymask->height;
	pm->refcnt = 1;
	xymask->refcnt++;
	pm->kind = BitmapPixmap;
	pm->data = (caddr_t) xymask;
	if (xymask->width == 16 && xymask->height == 16) {
	    pm->tile = CanBeTiled;
	} else {
	    pm->tile = CannotBeTiled;
	}
	/* save a bit to indicate if we have to invert the source */

	if (back & 1)
	    pm->kind |= InvertFlag;
	return (pm);
}

PIXMAP *PixmapSave (srcx, srcy, width, height)
	int srcx, srcy, width, height;
{
	register BITMAP *bm;
	PIXMAP *pm;
	extern BITMAP pbm;

	bm = (BITMAP *) Xalloc (sizeof (BITMAP));
	bm->width = width;
	bm->height = height;
	bm->refcnt = 0;

	if ((bm->data = 
		(caddr_t) malloc (BitmapSize(width, height))) == NULL) {
	    free ((caddr_t) bm);
	    return (NULL);
	}
	copyrmsk(VSTA$K_SRC_BITMAP, (short *) pbm.data, pbm.width, pbm.height,
		srcx, srcy, width, height,
		(short *) bm->data, width, height,
		0, 0, GXcopy, 0, 0);
	if (pm = MakePixmap (bm, 1, 0))
	    return (pm);
	FreeBitmap (bm);
	return (NULL);
}

/*ARGSUSED*/
PixmapGet (srcx, srcy, width, height, client, format, swapit)
	int srcx, srcy, width, height, client, format;
{
	PIXMAP *pm;
	BITMAP *bm;
	int slop;
	char padding[2];
	int size = BitmapSize(width, height);
	slop = size & 2;
	pm = PixmapSave (srcx, srcy, width, height);
	bm = (BITMAP *) pm->data;
	if (swapit)
		Swap_shorts ( (short *) bm->data, size >> 1 );
	Write (client, bm->data, size);
	FreePixmap(pm);
	if (slop)
		Write (client, padding, slop);
}
