/*
 *	$Source: /u1/X/libis/RCS/bitpix.c,v $
 *	$Header: bitpix.c,v 1.1 86/11/17 14:33:09 swick Rel $
 */

#ifndef lint
static char *rcsid_bitpix_c = "$Header: bitpix.c,v 1.1 86/11/17 14:33:09 swick Rel $";
#endif	lint

#include "is-copyright.h"

/*
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
 *	Copyright (c) 1986, Integrated Solutions, Inc.
 */

#include "Xis.h"
#include <errno.h>

extern int errno;
extern char *malloc();

DEVICE *CurrentDevice;

PIXMAP *MakePixmap();

/*
 *	StoreBitmap
 *
 *	Store "data" of dimensions ("width, height") in BITMAP data
 *	structure.
 *
 *	WARNING: Bits are put into "X" order
 */
BITMAP *StoreBitmap(width, height, data)
int 	width, height;
char 	*data;
{
    register BITMAP *bm;
    register RASTER *r;
    int size;

    if ((bm = (BITMAP *) malloc(sizeof (BITMAP))) == NULL) {
	return (NULL);
    }
    bm->width = width;
    bm->height = height;
    bm->refcnt = 1;

    if ((r = (RASTER *) malloc(sizeof(RASTER))) == NULL) {
	free((caddr_t) bm);
	return (NULL);
    }
    bm->data = (caddr_t) r;

    size = BitmapSize(width, height);
    if ((r->address = (short *) malloc(size)) == NULL) {
	free((RASTER *) bm->data);
	free((caddr_t) bm);
	return (NULL);
    }

    if (data != NULL) {
	bcopy((char *)data, r->address, size);
	SwapShorts((short *) r->address, size);
	SwapBits((short *) r->address, size);
    }
    r->width = ((width+15) >>4) << 1;

#ifdef DEBUG
if (debug & D_Bitmaps)
    printf("0x%x = StoreBitmap(width=%d, height=%d, data=0x%x)\n",
	bm, width, height, data);
#endif DEBUG

    return (bm);
}

/*
 *	FreeBitmap
 *
 *	Free bitmap data and bitmap structure.
 */
FreeBitmap(bitmap)
register BITMAP	*bitmap;
{

#ifdef DEBUG
if (debug & D_Bitmaps)
    printf("FreeBitmap(bitmap=0x%x)\n", bitmap);
#endif DEBUG

    free (((RASTER *) bitmap->data)->address);
    free ((caddr_t) bitmap->data);
    free ((caddr_t) bitmap);
}

/*
 *	CharBitmap
 *
 *	Copies a character bitmap from a font
 */
BITMAP *CharBitmap(c, font)
unsigned	c;
register FONT	*font;
{
    int width;
    register BITMAP *bm;

#ifdef DEBUG
if (debug & D_Bitmaps)
    printf("CharBitmap(c='%c', font=0x%x)\n", c, font);
#endif DEBUG

    if (c < font->first || c > font->last) {
	errno = EINVAL;
	return (NULL);
    }

    if (font->fixed)
	width = font->avg_width;
    else
	width = FDATA(font)->widths[c - font->first];

    if (width == 0) {
	errno = EINVAL;
	return (NULL);
    }

    bm = (BITMAP *) Xalloc(sizeof (BITMAP));
    bm->width = width;
    bm->height = font->height;
    bm->refcnt = 1;
    if ((bm->data = (caddr_t)malloc(BitmapSize(width, bm->height))) == NULL) {
	free((caddr_t) bm);
	errno = ENOMEM;
	return (NULL);
    }

    CopyText ((caddr_t) &c, 1, font, bm);
    return (bm);
}

/*
 *	StorePixmap
 *
 *	Create a pixmap
 */
PIXMAP *StorePixmap(width, height, format, data)
int	width, height, format;
char	*data;
{
    register PIXMAP *pm;
    register RASTER *r;
    int size, i;

#ifdef DEBUG
if (debug & D_Pixmaps)
    printf("StorePixmap(width=%d, height=%d, format=%d, data=0x%x)\n",
	width, height, format, data);
#endif DEBUG

    if (format != XYFormat /* 0 */) {
	return (NULL);
    }
    if ((pm = (PIXMAP *) malloc(sizeof(PIXMAP))) == NULL) {
	return (NULL);
    }
    pm->width = width;
    pm->height = height;
    if (width == TILE_WIDTH && height == TILE_HEIGHT)
	pm->tile = CanBeTiled;
    else
	pm->tile = CannotBeTiled;
    pm->kind = XYFORMAT | CurrentDevice->planes;
    pm->refcnt = 1;

    if ((r = (RASTER *) malloc(sizeof(RASTER)*CurrentDevice->planes)) == NULL) {
	free((caddr_t) pm);
	return (NULL);
    }
    pm->data = (caddr_t) r;
    size = BitmapSize(width, height);

    for (i=0; i < CurrentDevice->planes; i++, r++) {

	if ((r->address = (short *) malloc(size)) == NULL) {
	    while (i--)
		free((caddr_t) ((--r)->address));
	    free((caddr_t) pm->data);
	    free((caddr_t) pm);
	    return (NULL);
	}

	if (data != NULL) {
	    bcopy((char *)data, r->address, size);
	    data += size;
	    SwapShorts((short *) r->address, size);
	    SwapBits((short *) r->address, size);
	}

	r->width = ((width+15) >>4) << 1;
    }

    return (pm);
}

/* FreePixmap
 *
 *	Frees the storage consumed by the pixmap.
 */
FreePixmap(pixmap)
register PIXMAP	*pixmap;
{
    register RASTER*	r = (RASTER *)pixmap->data;
    register int	i;

#ifdef DEBUG
if (debug & D_Pixmaps)
    printf("FreePixmap(pixmap=0x%x)\n", pixmap);
#endif DEBUG

    for (i=0; i < (pixmap->kind & 0xf); i++, r++)
	free((caddr_t) r->address);
    free((caddr_t) pixmap->data);
    free((caddr_t) pixmap);
}

/*
 *	MakePixmap
 *
 *	Make pixmap from bitmap
 */
static PIXMAP constpix[] = {
    /* width, height, refcnt, tile, kind, *data */
    {1, 1, 1, CanBeTiled, CONSTANT | 4,	(caddr_t) 0},
    {1, 1, 1, CanBeTiled, CONSTANT | 4, (caddr_t) 1},
    {1, 1, 1, CanBeTiled, CONSTANT | 4, (caddr_t) 2},
    {1, 1, 1, CanBeTiled, CONSTANT | 4, (caddr_t) 3},
    {1, 1, 1, CanBeTiled, CONSTANT | 4, (caddr_t) 4},
    {1, 1, 1, CanBeTiled, CONSTANT | 4, (caddr_t) 5},
    {1, 1, 1, CanBeTiled, CONSTANT | 4, (caddr_t) 6},
    {1, 1, 1, CanBeTiled, CONSTANT | 4, (caddr_t) 7},
    {1, 1, 1, CanBeTiled, CONSTANT | 4, (caddr_t) 8},
    {1, 1, 1, CanBeTiled, CONSTANT | 4, (caddr_t) 9},
    {1, 1, 1, CanBeTiled, CONSTANT | 4, (caddr_t) 10},
    {1, 1, 1, CanBeTiled, CONSTANT | 4, (caddr_t) 11},
    {1, 1, 1, CanBeTiled, CONSTANT | 4, (caddr_t) 12},
    {1, 1, 1, CanBeTiled, CONSTANT | 4, (caddr_t) 13},
    {1, 1, 1, CanBeTiled, CONSTANT | 4, (caddr_t) 14},
    {1, 1, 1, CanBeTiled, CONSTANT | 4, (caddr_t) 15},
};

PIXMAP *MakePixmap(xymask, fore, back)
register BITMAP	*xymask;
int		fore, back;
{
    register PIXMAP *pm = NULL;
    register RASTER* r;
    register int i, size;
#ifdef DEBUG
int		sfore=fore, sback=back;
#endif DEBUG

    if (xymask == NULL) {
	pm = &constpix[fore & 0xf];
	pm->refcnt++;
#ifdef DEBUG
if (debug & D_Pixmaps)
    printf("0x%x = MakePixmap(xymask=0x%x, fore=%d, back=%d)\n",
	pm, xymask, fore, back);
#endif DEBUG
	return (pm);
    }

    pm = (PIXMAP *) Xalloc(sizeof(PIXMAP));
    pm->width = xymask->width;
    pm->height = xymask->height;
    pm->refcnt = 1;
    if (xymask->width == TILE_WIDTH && xymask->height == TILE_HEIGHT)
	pm->tile = CanBeTiled;
    else
	pm->tile = CannotBeTiled;
    pm->kind = XYFORMAT | CurrentDevice->planes;
    r = (RASTER *) Xalloc(sizeof(RASTER) * CurrentDevice->planes);
    pm->data = (caddr_t) r;

    for (i=0; i < CurrentDevice->planes; i++, r++) {

	size = BitmapSize(pm->width, pm->height);
	r->address = (short *) Xalloc(size);
	r->width = ((pm->width+15) >> 4) << 1;

	if (fore & 1) {
	    if (back & 1) {
		/* foreground and background are 1 */
		/* set to ~0; mask does not matter */
		register short *d, *limitd;
		d = r->address;
		limitd = d + (size>>1);
		for ( ; d < limitd; )
		    *d++ = ~0;
	    } else {
		/* foreground is 1 and background is 0 */
		/* set to mask */
		bcopy(((RASTER *)(xymask->data))->address, r->address, size);
	    }
	} else {
	    if (back & 1) {
		/* foreground is 0 and background is 1 */
		/* set to inverted mask */
		register short *s, *d, *limitd;
		s = ((RASTER *)(xymask->data))->address;
		d = r->address;
		limitd = d + (size>>1);
		for ( ; d < limitd; )
		    *d++ = ~*s++;
	    } else {
		/* foreground and background are 0 */
		/* set to 0; mask does not matter */
		register short *d, *limitd;
		d = r->address;
		limitd = d + (size>>1);
		for ( ; d < limitd; )
		    *d++ = 0;
	    }
	}
	fore >>= 1;
	back >>= 1;
    }

#ifdef DEBUG
if (debug & D_Pixmaps)
    printf("0x%x = MakePixmap(xymask=0x%x, fore=%d, back=%d)\n",
	pm, xymask, sfore, sback);
#endif DEBUG

    return (pm);
}

/*
 *	PixmapSave
 *
 *	Save a region of the screen
 */
PIXMAP *PixmapSave(srcx, srcy, width, height)
register int	srcx, srcy, width, height;
{
    register PIXMAP	*pm = NULL;
    register RASTER     *r;
    register int	i;
    CLIP		bounds;

#ifdef DEBUG
if (debug & D_Pixmaps)
    printf("PixmapSave(srcx=%d, srcy=%d, width=%d, height=%d)\n",
	srcx, srcy, width, height);
#endif DEBUG

    bounds.top = srcy;
    bounds.left = srcx;
    bounds.width = width;
    bounds.height = height;
    pm = (PIXMAP *) Xalloc(sizeof(PIXMAP));
    pm->width = width;
    pm->height = height;
    pm->refcnt = 1;
    if (width == TILE_WIDTH && height == TILE_HEIGHT)
	pm->tile = CanBeTiled;
    else
	pm->tile = CannotBeTiled;
    pm->kind = XYFORMAT | CurrentDevice->planes;
    r = (RASTER *) Xalloc(sizeof(RASTER) * CurrentDevice->planes);
    pm->data = (caddr_t) r;

    for (i=0; i < CurrentDevice->planes; r++, i++) {
	r->address = (short *) Xalloc(BitmapSize(width, height));
	r->width = ((width+15) >> 4) << 1;
    }
    CheckCursor(bounds);
    GIP_RasterOp(GIPcopy,
	&ScreenPixmap, srcx, srcy,
	pm, 0, 0,
	(BITMAP *)NULL, 0, 0,
	width, height,
	~0);
    RestoreCursor();
    return (pm);
}

/*
 *	PixmapGet
 *
 *
 */
PixmapGet(srcx, srcy, width, height, client, format, swapit)
int	srcx, srcy, width, height, client, format, swapit;
{
    PIXMAP		*pm;
    register RASTER	*r;
    register int	size = BitmapSize(width, height);
    register int	i;

#ifdef DEBUG
if (debug & D_Pixmaps)
    printf("PixmapGet(srcx=%d, srcy=%d, width=%d, height=%d, client=%d, format=%d, swapit=%d)\n",
	srcx, srcy, width, height, client, format, swapit);
#endif DEBUG

    pm = PixmapSave(srcx, srcy, width, height);

    r = (RASTER *) pm->data;
    for (i=0; i< CurrentDevice->planes; i++, r++) {
	SwapBits(r->address, size);
	if (!swapit) /* our bitmaps need swapping just VAX (un)normal */
	    Swap_shorts((short *) r->address, size >> 1);
	Write(client, r->address, size);
    }
    /* Pad amount written to 32-bit boundary */
    if ((size*CurrentDevice->planes)%4) {
	Write(client, r->address, 4 - ((size*CurrentDevice->planes)%4));
    }

    FreePixmap(pm);
}
