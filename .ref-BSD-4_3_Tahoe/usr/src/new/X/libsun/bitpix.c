#ifndef lint
static char *rcsid_bitpix_c = "$Header: bitpix.c,v 10.3 86/11/29 13:47:16 jg Rel $";
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
static char sccsid[] = "@(#)bitpix.c 2.1 86/01/28 Copyright 1986 Sun Micro";
#endif

/*-
 * Copyright (c) 1986 by Sun Microsystems,  Inc.
 */
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

#include "Xsun.h"
#include <errno.h>

extern int errno;
extern struct pixrect *PixRect;

char *Xalloc();
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
	InvertPixelOrder((short *) bm->data, size>>1);

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
	    width = ((struct pixfont *)font->data)->pf_char[c].pc_adv.x;
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
    register PIXMAP *pm;

    if (PixRect->pr_depth == 1) {
	register BITMAP *bm;

	bm = (BITMAP *) StoreBitmap(width, height, data);
	if (bm == NULL)
	    return (NULL);
	bm->refcnt = 0;
	if ((pm = MakePixmap(bm, 1, 0)) == NULL) {
	    FreeBitmap(bm);
	    return (NULL);
	}
    }
    else if (PixRect->pr_depth <= 8) {
	char *     newdata;
	int         size;

	pm = (PIXMAP *) Xalloc(sizeof(PIXMAP));
	pm->width = width;
	pm->height = height;
	pm->refcnt = 1;
	switch (format) {
	case XYFormat:
	    size = XYPixmapSize(width, height, PixRect->pr_depth);
	    pm->kind = XYColorPixmap;
	    break;
	case ZFormat:
	    size = BZPixmapSize(width, height);
	    if (width&1)
		size += height;
	    pm->kind = ZColorPixmap;
	    break;
	}
	newdata = (char *) Xalloc(size);
	if (width&1) {
	    register int i;
	    register char * old = data, *new = newdata;

	    for (i = 0; i < height; i++) { 
		bcopy(old, new, width);
		old += width;
		new += width + 1;
	    }
	} else
	    bcopy(data, newdata, size);
	pm->data = newdata;
    }
    return (pm);
}

FreePixmap (pixmap)
	register PIXMAP *pixmap;
{
    switch (pixmap->kind) {
    case BitmapPixmap:
	{
	    register BITMAP *bm;

	    bm = PDATA(pixmap);
	    if (--bm->refcnt == 0)
		FreeBitmap(bm);
	}
	break;
    case ZColorPixmap:
    case XYColorPixmap:
	free((caddr_t)pixmap->data);
	break;
    case ConstantPixmap:
	return;
    }
    free((caddr_t) pixmap);
}

PIXMAP constpix0 = {1, 1, 1, 1, ConstantPixmap, (caddr_t) 0};
PIXMAP constpix1 = {1, 1, 1, 1, ConstantPixmap, (caddr_t) 1};

PIXMAP *MakePixmap (xymask, fore, back)
	register BITMAP *xymask;
	int fore, back;
{
    register PIXMAP *pm = NULL;

    if (xymask == NULL) {
	if (PixRect->pr_depth == 1) {
	    if (fore & 1)
		pm = &constpix1;
	    else
		pm = &constpix0;
	    pm->refcnt++;
	}
	else if (PixRect->pr_depth <= 8) {
	    static PIXMAP *constpm[256];

	    if (constpm[fore & 0xFF] == 0) {
		constpm[fore & 0xFF] = pm = (PIXMAP *) Xalloc(sizeof(PIXMAP));
		pm->width = 1;
		pm->height = 1;
		pm->refcnt = 1;
		pm->tile = CanBeTiled;
		pm->kind = ConstantPixmap;
		pm->data = (caddr_t) fore;
	    }
	    else {
		pm = constpm[fore & 0xFF];
		pm->refcnt++;
	    }
	}
	return (pm);
    }

    pm = (PIXMAP *) Xalloc(sizeof(PIXMAP));
    pm->width = xymask->width;
    pm->height = xymask->height;
    pm->refcnt = 1;
    if (PixRect->pr_depth > 1) {
	struct pixrect *mask, *dest;

	if (PixRect->pr_depth > 8) {
	    pm->data = (caddr_t) Xalloc(WZPixmapSize(pm->width, pm->height));
	} else {
	    pm->data = (caddr_t) Xalloc(BZPixmapSize(pm->width, pm->height));
	}
	dest = mem_point(pm->width, pm->height, PixRect->pr_depth, pm->data);
	mask = mem_point(pm->width, pm->height, 1, xymask->data);

	/*
	 * First paint the background color over the whole region. Then 
	 * paint the foreground color over the region using xymask as a stencil.
	 */
        pr_rop(dest, 0, 0, pm->width, pm->height,
                        PIX_SRC | PIX_DONTCLIP | PIX_COLOR(back), NULL, 0, 0);
        pr_stencil(dest, 0, 0, pm->width, pm->height,
                        PIX_SRC | PIX_DONTCLIP | PIX_COLOR(fore), 
			mask, 0, 0, NULL, 0 ,0);
        pr_destroy(mask);
	/*
	 * We assume pm->data is NOT freed when dest is destroyed.
	 */
        pr_destroy(dest);

	pm->kind = ZColorPixmap;
    } else {
	xymask->refcnt++;
	pm->kind = BitmapPixmap;
	pm->data = (caddr_t) xymask;
    }
    pm->tile = CanBeTiled;
    /* save a bit to indicate if we have to invert the source */

    if ((PixRect->pr_depth == 1) && (back & 1))
	pm->kind |= InvertFlag;
    return (pm);
}

PIXMAP *PixmapSave (srcx, srcy, width, height)
	int srcx, srcy, width, height;
{
    PIXMAP     *pm = NULL;


    if (PixRect->pr_depth == 1) {
	register BITMAP *bm;

	bm = (BITMAP *) Xalloc(sizeof(BITMAP));
	bm->width = width;
	bm->height = height;
	bm->refcnt = 0;

	if ((bm->data =
	     (caddr_t) malloc(BitmapSize(width, height))) == NULL) {
	    free((caddr_t) bm);
	    return (NULL);
	}
	{
	    struct pixrect *dest;

	    dest = mem_point(width, height, 1, bm->data);
	    CheckCursor(srcx, srcy, width, height);
	    pr_rop(dest, 0, 0, width, height, PIX_SRC | PIX_DONTCLIP, PixRect, srcx, srcy);
	    pr_destroy(dest);
	    RestoreCursor();
	}
	if ((pm = MakePixmap(bm, 1, 0)) == 0) {
	    FreeBitmap(bm);
	    return (NULL);
	}
    }
    else if (PixRect->pr_depth <= 8) {
	int sz = BZPixmapSize(width, height);

	if (width&1)
	    sz += height;
	pm = (PIXMAP *) Xalloc(sizeof(PIXMAP));
	pm->width = width;
	pm->height = height;
	pm->refcnt = 1;
	pm->kind = ZColorPixmap;
	if ((pm->data =
	     (caddr_t) malloc(sz)) == NULL) {
	    free((caddr_t) pm);
	    return (NULL);
	}
	{
	    struct pixrect *dest;

	    dest = mem_point(width, height, PixRect->pr_depth, pm->data);
	    CheckCursor(srcx, srcy, width, height);
	    pr_rop(dest, 0, 0, width, height, PIX_SRC | PIX_DONTCLIP, PixRect, srcx, srcy);
	    pr_destroy(dest);
	    RestoreCursor();
	}
    }
    return (pm);
}

/*ARGSUSED*/
PixmapGet (srcx, srcy, width, height, client, format, swapit)
	int srcx, srcy, width, height, client, format;
{
    PIXMAP     *pm;
    BITMAP     *bm;
    pm = PixmapSave(srcx, srcy, width, height);
    switch (pm->kind) {
    case BitmapPixmap:{
	    int         size = BitmapSize(width, height);

	    bm = (BITMAP *) pm->data;
	    if (swapit)
		Swap_shorts((short *) bm->data, size >> 1);
	    InvertPixelOrder((short *) bm->data, size >> 1);
	    Write(client, bm->data, size);
	    /* Pad amount written to 32-bit boundary - Ahem! */
	    if (size%4) {
		Write(client, bm->data, 4 - (size%4));
	    }
	}
	break;
    case ZColorPixmap:{
	    switch (format) {
	    case XYFormat:{
		    caddr_t     newdata;
		    int size;

		    size = XYPixmapSize(width, height, PixRect->pr_depth);
		    newdata = (caddr_t) malloc(size);
		    if (newdata) {
			ZtoXY(width, height, PixRect->pr_depth, pm->data, newdata);
			free(pm->data);
			pm->data = newdata;
			if (swapit)
			    Swap_shorts((short *) pm->data, size >> 1);
			InvertPixelOrder((short *) pm->data, size >> 1);
		    }
		    Write(client, pm->data, size);
	            /* Pad amount written to 32-bit boundary - Ahem! */
	            if (size%4) {
		        Write(client, pm->data, 4 - (size%4));
	            }
		}
		break;
	    case ZFormat: {
		int size = BZPixmapSize(width, height);

		if (width&1) {
		    register int i;
		    register char *old = pm->data;

		    for (i = 0; i < height; i++) {
			Write(client, old, width);
			old += width + 1;
		    }
		} else {
		    Write(client, pm->data, size);
		}
	        /* Pad amount written to 32-bit boundary - Ahem! */
	        if (size%4) {
		    Write(client, pm->data, 4 - (size%4));
	        }
	    }
	    break;
	}
	break;
	}
    case XYColorPixmap:
	/*NOTREACHED*/
	break;
    }
    FreePixmap(pm);
}

static ZtoXY(w, h, d, old, new)
int	w, h, d;
caddr_t	old;
u_char *new;
{
    unsigned mask = 1;
    struct pixrect *New, *Old;

    Old = mem_point(w, h, d, old);
    while (d--) {
	register int y;

	New = mem_point(w, h, 1, new);
	new += BitmapSize(w, h);
	for (y = 0; y < h; y++) {
	    register int x;

	    for (x = 0; x < w; x++) {
		pr_put(New, x, y, (pr_get(Old, x, y) & mask));
	    }
	}
	pr_destroy(New);
	mask <<= 1;
    }
}
#endif
