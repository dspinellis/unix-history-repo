#ifndef lint
static char *rcsid_bitpix_c = "$Header: bitpix.c,v 10.1 86/11/19 10:40:11 jg Exp $";
#endif	lint
/* Copyright 1985 Massachusetts Institute of Technology */

/* bitpix.c - Routines to cache bitmaps and pixmaps
 *
 *      StoreBitmap     Creates a bitmap
 *      FreeBitmap      Frees the storage taken by a bitmap
 *      CharBitmap      Creates a bitmap from a font character
 *      StorePixmap     Creates a pixmap
 *      FreePixmap      Frees the storage taken by a pixmap
 *      MakePixmap      Create a pixmap from a bitmap
 *      PixmapSave      Save a region of the screen
 *      PixmapGet       Read a region of the screen
 *
 *  	Changes and additions by:
 *
 *		Scott Bates
 *		Brown University
 *		IRIS, Box 1946
 *     		Providence, RI 02912
 *
 *
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 */

#include "private.h"
#include "bitblt.h"

/*
 * Create bitmap from client supplied data
 */

BITMAP *StoreBitmap (width, height, data)
        register width, height;
        char *data;
{
        register BITMAP *bm;
        register size = BitmapSize(width, height);

#ifdef TRACE_X
	fprintf(stderr, "In StoreBitmap\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Allocate space for bitmap structure
	 */

        bm = (BITMAP *) Xalloc (sizeof (BITMAP));

	/*
	 * Fill in bitmap structure
	 */

        bm->width = width;
        bm->height = height;
        bm->refcnt = 1;

	/*
	 * Allocated space to hold bitimage data
	 */

        if ((bm->data = (caddr_t) malloc (size)) == NULL) {
            free ((caddr_t) bm);
            return (NULL);
        }

	/*
	 * Copy image data to newly allocated area and reverse bits
	 * in each short of the image. The image data is in vax 
	 * bit order and must be reversed for this hardware.
	 */

        bcopy (data, bm->data, size);
	ReverseShortBits((u_short *) bm->data, size >> 1);

	/*
	 * Return pointer to bitmap
	 */

        return (bm);
}

/*
 * Free bitmap structure and bit image data
 */

FreeBitmap (bitmap)
        register BITMAP *bitmap;
{

#ifdef TRACE_X
	fprintf(stderr, "In FreeBitmap\n");
	fflush(stderr);
#endif TRACE_X

        free ((caddr_t) bitmap->data);
        free ((caddr_t) bitmap);
}

/*
 * Create character bitmap from font character
 */

BITMAP *CharBitmap (c, font)
        u_char c;
        register FONT *font;
{
        register width;
	register size;
        register BITMAP *bm;

#ifdef TRACE_X
	fprintf(stderr, "In CharBitmap\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Test for valid character
	 */

        if (c < font->first || c > font->last) {
            errno = EINVAL;
            return (NULL);
        }

	/*
	 * Determine width of character
	 */

        if (font->fixed)
            width = font->avg_width;
        else
            width = FDATA(font)->widths[c];

	/*
	 * Validate width
	 */

        if (width == 0) {
            errno = EINVAL;
            return (NULL);
        }

	/* 
	 * Allocate space for bitmap structure
	 */

        bm = (BITMAP *) Xalloc (sizeof (BITMAP));

	/*
	 * Fill in bitmap structure
	 */

        bm->width = width;
        bm->height = font->height;
        bm->refcnt = 1;

	/*
	 * Allocate space to hold bit image of character
	 */

	size = BitmapSize(width, bm->height);
        if ((bm->data = (caddr_t) malloc (size)) == NULL) {
            free ((caddr_t) bm);
            errno = ENOMEM;
            return (NULL);
        }

	/*
	 * Copy bit image of character to newly allocated area
	 * and return pointer to character bitmap
	 */

        CopyText ((caddr_t) &c, 1, font, bm);
        return (bm);
}

/*
 * Create pixmap from client supplied data
 */

/*ARGSUSED*/
PIXMAP *StorePixmap (width, height, format, data)
        int width, height, format;
        char *data;
{
        register BITMAP *bm;
        register PIXMAP *pm;

#ifdef TRACE_X
	fprintf(stderr, "In StorePixmap\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Create bitmap from the bit image data supplied.
	 */

        if ((bm = StoreBitmap (width, height, data)) == NULL) {
            return (NULL);
	}

	/*
	 * Reset reference count so bitmap will be deallocated 
	 * correctly by FreePixmap 
	 */
	 
        bm->refcnt = 0;

	/*
	 * Make pixmap from bitmap
	 */

        if (pm = MakePixmap (bm, 1, 0))
            return (pm);

	/*
	 * If unable to make pixmap free the bitmap
	 */

        FreeBitmap (bm);
        return (NULL);
}

/*
 * Free pixmap resources
 */

FreePixmap (pm)
        register PIXMAP *pm;
{
        register BITMAP *bm;

#ifdef TRACE_X
	fprintf(stderr, "In FreePixmap\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * If a bitmap is associated with this pixmap and its
	 * reference count is equal to zero free it. Otherwise 
	 * decrement its reference count and return.
	 */

        if (pm->kind & BitmapPixmap) {
            bm = PDATA(pm);
            if (--bm->refcnt == 0)
                FreeBitmap (bm);
        }

	/*
	 * Free pixmap structure
	 */

        free ((caddr_t) pm);
}

/*
 * Make a pixmap from a bitmap
 */

PIXMAP *MakePixmap (xymask, fore, back)
        register BITMAP *xymask;
        register fore, back;
{
        register PIXMAP *pm;

#ifdef TRACE_X
	fprintf(stderr, "In MakePixmap\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * If no bitmap was supplied or the foreground and
	 * background colors are the same then pixmap is
	 * a constant pixmap (either all white or all black)
	 */

        if (xymask == NULL || !((fore ^ back) & 1)) {
		if (fore & 1)
			pm = &constpix1;
		else
			pm = &constpix0;
		pm->refcnt++;
		return (pm);
        }

	/*
	 * Allocate space for pixmap structure
	 */

        pm = (PIXMAP *) Xalloc (sizeof (PIXMAP));

	/*
	 * Fill in pixmap structure. Indicate that a bitmap is
	 * associated with this pixmap (BitmapPixmap).
	 */

        pm->width = xymask->width;
        pm->height = xymask->height;
        pm->refcnt = 1;
        pm->kind = BitmapPixmap;
        pm->data = (caddr_t) xymask;

	/*
	 * Increment reference count of bitmap
	 */

        xymask->refcnt++;

	/*
	 * Indicate if pixmap can be used as a tile
	 */

        if (xymask->width == TILE_WIDTH && xymask->height == TILE_HEIGHT) {
            pm->tile = CanBeTiled;
        } else {
            pm->tile = CannotBeTiled;
        }

	/*
	 * Indicate if bit image needs to be inverted when displayed
	 */

        if (back & 1)
            pm->kind |= InvertFlag;

	/*
	 * Return pointer to pixmap
	 */

        return (pm);
}

/*
 * Save rectangular screen image
 */

PIXMAP *PixmapSave (srcx, srcy, width, height)
        register srcx, srcy, width, height;
{
	register Blt_Rectangle *source = &SrcRect;
	register Blt_Rectangle *dest = &DstRect;
        register BITMAP *bm;
        PIXMAP *pm;

#ifdef TRACE_X
	fprintf(stderr, "In PixmapSave\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Allocate space for bitmap structure
	 */

        bm = (BITMAP *) Xalloc (sizeof (BITMAP));


	/*
	 * Fill in bitmap structure
	 */

        bm->width = width;
        bm->height = height;
        bm->refcnt = 0;

	/*
	 * Allocate space to hold screen image
	 */

        if ((bm->data = 
                (caddr_t) malloc (BitmapSize(width, height))) == NULL) {
            free ((caddr_t) bm);
            return (NULL);
        }

	/*
	 * Fill in source and destination rectangles
	 */

	FillInRect(srcx, srcy, width, height, source);
	FillInRect(0, 0, width, height, dest);

	/*
	 * Copy screen image to allocated area
	 */

        CopyBits ((u_short *) pbm.data, pbm.width, pbm.height, source,
                  (u_short *) bm->data, width, height, dest,
		  NILMASK, NIL, NIL, GXcopy, NIL, NILCLIP);

	/*
	 * Make pixmap from bitmap and return pointer to pixmap
	 */

        if (pm = MakePixmap (bm, 1, 0))
            return (pm);

	/*
	 * If unable to make pixmap free bitmap
	 */

        FreeBitmap (bm);
        return (NULL);
}

/*
 * Pass rectangular screen image to client
 */

/*ARGSUSED*/
PixmapGet (srcx, srcy, width, height, client, format, swapit)
        int srcx, srcy, width, height, client, format;
{
        register PIXMAP *pm;
        register BITMAP *bm;
        register size = BitmapSize(width, height);

#ifdef TRACE_X
	fprintf(stderr, "In PixmapGet\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Save screen image 
	 */

        pm = PixmapSave (srcx, srcy, width, height);

	/*
	 * Reverse the bits in each short of the image. The image
	 * is in IBM bit order and needs to be in VAX bit order when
	 * given to clients.
	 */

        bm = (BITMAP *) pm->data;
	ReverseShortBits((u_short *) bm->data, size >> 1);

	/*
	 * Swap shorts of image if required by client
	 */

        if (swapit)
                Swap_shorts ((short *) bm->data, size >> 1 );

	/*
	 * Write screen image to client. Image size must be padded 
	 * to 32 bit boundary.
	 */

	Write (client, bm->data, size);
	if (size % 4) {
		Write(client, bm->data, 4 - (size % 4));
	}

	/*
	 * Free pixmap and return
	 */

        FreePixmap(pm);
}
