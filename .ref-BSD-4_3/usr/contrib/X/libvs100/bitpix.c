/* $Header: bitpix.c,v 10.3 86/02/01 15:46:25 tony Rel $ */
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

/****************************************************************************
 *									    *
 *  Copyright (c) 1983, 1984 by						    *
 *  DIGITAL EQUIPMENT CORPORATION, Maynard, Massachusetts.		    *
 *  All rights reserved.						    *
 * 									    *
 *  This software is furnished on an as-is basis and may be used and copied *
 *  only with inclusion of the above copyright notice. This software or any *
 *  other copies thereof may be provided or otherwise made available to     *
 *  others only for non-commercial purposes.  No title to or ownership of   *
 *  the software is hereby transferred.					    *
 * 									    *
 *  The information in this software is  subject to change without notice   *
 *  and  should  not  be  construed as  a commitment by DIGITAL EQUIPMENT   *
 *  CORPORATION.							    *
 * 									    *
 *  DIGITAL assumes no responsibility for the use  or  reliability of its   *
 *  software on equipment which is not supplied by DIGITAL.		    *
 * 									    *
 *									    *
 ****************************************************************************/

#include "vs100.h"
#include <errno.h>

extern int errno;
extern BitMap screen;
extern int VSReloc;

char *Xalloc(), *AllocateSpace();
VSArea *VSAlloc();
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
	if ((bm->data = (caddr_t) VSAlloc (size, BITMAP_TYPE)) == NULL) {
	    free ((caddr_t) bm);
	    return (NULL);
	}
	if (MoveBufferDown (data, BDATA(bm)->vsPtr, size)) {
	    FreeBitmap (bm);
	    return (NULL);
	}
	return (bm);
}

FreeBitmap (bitmap)
	register BITMAP *bitmap;
{
	VSFree (BDATA(bitmap));
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
	    width = FDATA(font)->widths[c - font->first];
	if (width == 0) {
	    errno = EINVAL;
	    return (NULL);
	}
	bm = (BITMAP *) Xalloc (sizeof (BITMAP));
	bm->width = width;
	bm->height = font->height;
	bm->refcnt = 1;
	if ((bm->data = (caddr_t) VSAlloc (BitmapSize(width, bm->height),
					   BITMAP_TYPE)) == NULL) {
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
#ifdef HTCROCK
	register TilePriv *tp;
#endif
	register BITMAP *bm;

	if (pixmap->kind) {
#ifdef HTCROCK
	    if (pixmap->kind & 2) {
		tp = TDATA(pixmap);
		bm = tp->bitmap;
		free ((caddr_t) tp);
	    } else
#endif
	    bm = PDATA(pixmap);
	    if (--bm->refcnt == 0)
		FreeBitmap (bm);
	}
	free ((caddr_t) pixmap);
}

PIXMAP constpix0 = {1, 1, 1, 1, 0, (caddr_t) 0};
PIXMAP constpix1 = {1, 1, 1, 1, 0, (caddr_t) 1};

PIXMAP *MakePixmap (xymask, fore, back)
	register BITMAP *xymask;
	int fore, back;
{
#ifdef HTCROCK
	register TilePriv *tp;
#endif
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
	if (xymask->width == 16 && xymask->height == 16) {
	    pm->tile = 1;
	    pm->kind = 2;
#ifdef HTCROCK
	    tp = (TilePriv *) Xalloc (sizeof (TilePriv));
	    tp->bitmap = xymask;
	    pm->data = (caddr_t) tp;
	    if (MoveBufferUp (BDATA(xymask)->vsPtr, (char *) tp->data, 32)) {
		FreePixmap (pm);
		return (NULL);
	    }
#else
	    pm->data = (caddr_t) xymask;
#endif
	} else {
	    pm->tile = 0;
	    pm->kind = 1;
	    pm->data = (caddr_t) xymask;
	}
	if (back & 1)
	    pm->kind |= 0x10;
	return (pm);
}

PIXMAP *PixmapSave (srcx, srcy, width, height)
	int srcx, srcy, width, height;
{
	register BITMAP *bm;
	PIXMAP *pm;
	register CopyAreaPacket *cap;
#define	h ((PacketHeader *) cap->cap_head)
#define src ((SubBitmap *) cap->cap_source.image)
#define dst ((SubBitmap *) cap->cap_destImage)
#define	size ((Extent *) cap->cap_maskSize)

	bm = (BITMAP *) Xalloc (sizeof (BITMAP));
	bm->width = width;
	bm->height = height;
	bm->refcnt = 0;

	if ((bm->data = (caddr_t) VSAlloc (BitmapSize(width, height),
					   BITMAP_TYPE)) == NULL) {
	    free ((caddr_t) bm);
	    return (NULL);
	}

	cap = (CopyAreaPacket *) AllocateSpace (sizeof (CopyAreaPacket));
	if (cap == NULL) {
	    FreeBitmap (bm);
	    return (NULL);
	}

	h->ph_copyMod.m_mask = 0;
	h->ph_copyMod.m_map = 0;
	h->ph_copyMod.m_clipping = 0;
	h->ph_copyMod.m_source = 1;
	h->ph_opcode = COPY_AREA;
	*(long *) h->ph_next = NULL;

	*(BitMap *) src->sb_base = screen;
	src->sb_x = srcx;
	src->sb_y = srcy;
	size->e_height = height;
	size->e_width = width;

	*(caddr_t *)dst->sb_address = BDATA(bm)->vsPtr;
	dst->sb_height = height;
	dst->sb_width = width;
	dst->sb_bitsPerPixel = 1;
	dst->sb_x = dst->sb_y = 0;

	WritePacket ((caddr_t) cap);

	if (pm = MakePixmap (bm, 1, 0))
	    return (pm);
	FreeBitmap (bm);
	return (NULL);
#undef h
#undef src
#undef dst
#undef size
}

/*ARGSUSED*/
PixmapGet (srcx, srcy, width, height, client, format, swapit)
	int srcx, srcy, width, height, client, format, swapit;
{
	int slop, width1, num, bytes;
	char padding[2];
	register CopyAreaPacket *cap;
#define	h ((PacketHeader *) cap->cap_head)
#define src ((SubBitmap *) cap->cap_source.image)
#define dst ((SubBitmap *) cap->cap_destImage)
#define	size ((Extent *) cap->cap_maskSize)

	width1 = BitmapSize(width, 1);
	slop = (width1 * height) & 2;
	num = VBUFSIZE / width1;
	while (height) {
	    if (height < num)
		num = height;
	    bytes = num * width1;
	    cap = (CopyAreaPacket *) AllocateSpace (sizeof (CopyAreaPacket) +
						    bytes);
	    if (cap == NULL)
		return;
	    h->ph_copyMod.m_mask = 0;
	    h->ph_copyMod.m_map = 0;
	    h->ph_copyMod.m_clipping = 0;
	    h->ph_copyMod.m_source = 1;
	    h->ph_opcode = COPY_AREA;
	    *(long *) h->ph_next = NULL;

	    *(BitMap *) src->sb_base = screen;
	    src->sb_x = srcx;
	    src->sb_y = srcy;
	    size->e_height = num;
	    size->e_width = width;

	    *(caddr_t *)dst->sb_address = (caddr_t) cap +
					  sizeof (CopyAreaPacket) +
					  VSReloc;
	    dst->sb_height = num;
	    dst->sb_width = width;
	    dst->sb_bitsPerPixel = 1;
	    dst->sb_x = dst->sb_y = 0;

	    WritePacket ((caddr_t) cap);
	    SynchWrites ();
	    if (swapit)
		Swap_shorts ((short *) ((caddr_t) cap + sizeof (CopyAreaPacket)),
			     bytes >> 1);
	    Write (client, (caddr_t) cap + sizeof (CopyAreaPacket), bytes);
	    height -= num;
	    srcy += num;
	}
	if (slop)
	    Write (client, padding, slop);
#undef h
#undef src
#undef dst
#undef size
}
