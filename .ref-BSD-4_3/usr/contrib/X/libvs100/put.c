/* $Header: put.c,v 10.3 86/02/01 15:47:24 tony Rel $ */
/* put.c	Perform a raster operation with a source bitmap
 *
 *	PixmapPut	Puts a pixmap up on the screen
 *	PixmapBitsPut	Puts a pixmap up on the screen
 *	BitmapBitsPut	Puts a pixmap up on the screen
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

extern BitMap screen;
extern int VSReloc;
extern char FBMap[];
extern char SSMap[];

char *AllocateSpace(), *AllocateCopy(), *Xalloc();

PixmapPut (src, srcx, srcy, width, height, dstx, dsty, clips, clipcount,
	   func, zmask)
	PIXMAP *src;
	int srcx, srcy, width, height, dstx, dsty, clipcount, zmask;
	register int func;
	CLIP *clips;
{
	register CopyAreaPacket *cap;
#define	h ((PacketHeader *) cap->cap_head)
#define	img ((SubBitmap *) cap->cap_source.image)
#define	pat ((Halftone *) cap->cap_source.pattern)
#define	size ((Extent *) cap->cap_maskSize)
#define	destOff ((Point *) cap->cap_destOffset)
#define	clip ((RectangleList *) cap->cap_clipping.rectList)

	if (!(zmask & 1)) {
	    DeallocateSpace ();
	    return;
	}
	cap = (CopyAreaPacket *) AllocateSpace (sizeof (CopyAreaPacket));
	if (cap == NULL) return;

	func = SSMap[func | (src->kind & 0x10)];
	h->ph_copyMod.m_mask = 0;
	h->ph_copyMod.m_map = MAPTYPE(func);
	h->ph_opcode = COPY_AREA;
	*(long *) h->ph_next = NULL;

	if (src->kind == 0) {
	    h->ph_copyMod.m_source = 0;
	    cap->cap_source.const = (int) src->data;
	} else {
	    h->ph_copyMod.m_source = 1;
#ifdef HTCROCK
	    if (src->kind & 2)
		*(caddr_t *) img->sb_address = BDATA(TDATA(src)->bitmap)->vsPtr;
	    else
#endif
	    *(caddr_t *) img->sb_address = BDATA(PDATA(src))->vsPtr;
	    img->sb_height = src->height;
	    img->sb_width = src->width;
	    img->sb_bitsPerPixel = 1;
	    img->sb_x = srcx;
	    img->sb_y = srcy;
	}

	size->e_height = height;
	size->e_width = width;

	*(BitMap *) cap->cap_destImage = screen;
	destOff->p_x = dstx;
	destOff->p_y = dsty;

	*(long *) cap->cap_map.literal = MAPLIT(func);

	if (clipcount == 1) {
	    h->ph_copyMod.m_clipping = 1;
	    *(CLIP *) cap->cap_clipping.litRect = *clips;
	} else {
	    h->ph_copyMod.m_clipping = 2;
	    *(caddr_t *) clip->r_first = (caddr_t) clips + VSReloc;
	    clip->r_count = clipcount;
	}

	WritePacket ((caddr_t) cap);
#undef h
#undef img
#undef pat
#undef size
#undef destOff
#undef clip
}


/*ARGSUSED*/
PixmapBitsPut (width, height, format, data, xymask, dstx, dsty,
	       clips, clipcount, func, zmask)
	char *data;
	int width, height, format, dstx, dsty, clipcount, zmask;
	BITMAP *xymask;
	CLIP *clips;
	int func;
{
	BitmapBitsPut (width, height, data, 1, 0, xymask, dstx, dsty,
		       clips, clipcount, func, zmask);
}


BitmapBitsPut (width, height, data, fore, back, xymask, dstx, dsty,
	       clips, clipcount, func, zmask)
	char *data;
	int width, height, fore, back, dstx, dsty, clipcount, zmask;
	register BITMAP *xymask;
	CLIP *clips;
	register int func;
{
	register CopyAreaPacket *cap;
#define	h ((PacketHeader *) cap->cap_head)
#define	src ((SubBitmap *) cap->cap_source.image)
#define	pat ((Halftone *) cap->cap_source.pattern)
#define	size ((Extent *) cap->cap_maskSize)
#define mask ((SubBitmap *) cap->cap_sourceMask)
#define	destOff ((Point *) cap->cap_destOffset)
#define	clip ((RectangleList *) cap->cap_clipping.rectList)
	caddr_t bits;
	char *boxes = NULL;
	CLIP clip1;
	int width1, num, boxbytes, bytes;

	if (!(zmask & 1)) {
	    DeallocateSpace ();
	    return;
	}
	if (fore & 1)
	    func += 0x20;
	if (back & 1)
	    func += 0x10;
	func = FBMap[func];

	width1 = BitmapSize(width, 1);
	num = VBUFSIZE / width1;
	if (height > num) {
	    if (clipcount == 1) {
		clip1 = *clips;
		clips = &clip1;
	    } else {
		boxbytes = sizeof (CLIP) * clipcount;
		boxes = Xalloc (boxbytes);
		bcopy ((caddr_t) clips, boxes, boxbytes);
		DeallocateSpace ();
	    }
	}

	while (height) {
	    if (height < num)
		num = height;
	    bytes = num * width1;
	    if ((bits = (caddr_t) AllocateCopy (data, bytes)) == NULL ||
		(boxes && (clips = (CLIP *) AllocateCopy (boxes, boxbytes)) == NULL) ||
		(cap = (CopyAreaPacket *) AllocateSpace (sizeof (CopyAreaPacket))) == NULL)
		break;

	    h->ph_copyMod.m_source = 1;
	    h->ph_copyMod.m_mask = xymask ? 1 : 0;
	    h->ph_copyMod.m_map = MAPTYPE(func);
	    h->ph_opcode = COPY_AREA;
	    *(long *) h->ph_next = NULL;

	    *(caddr_t *) src->sb_address = bits + VSReloc;
	    src->sb_height = num;
	    src->sb_width = width;
	    src->sb_bitsPerPixel = 1;
	    src->sb_x = src->sb_y = 0;

	    if (xymask) {
		    *(caddr_t *) mask->sb_address = BDATA(xymask)->vsPtr;
		    mask->sb_height = xymask->height;
		    mask->sb_width = xymask->width;
		    mask->sb_bitsPerPixel = 1;
		    mask->sb_x = mask->sb_y = 0;
	    }
	    size->e_height = num;
	    size->e_width = width;

	    *(BitMap *) cap->cap_destImage = screen;
	    destOff->p_x = dstx;
	    destOff->p_y = dsty;

	    *(long *) cap->cap_map.literal = MAPLIT(func);

	    if (clipcount == 1) {
		h->ph_copyMod.m_clipping = 1;
		*(CLIP *) cap->cap_clipping.litRect = *clips;
	    } else {
		h->ph_copyMod.m_clipping = 2;
		*(caddr_t *) clip->r_first = (caddr_t) clips + VSReloc;
		clip->r_count = clipcount;
	    }

	    WritePacket ((caddr_t) cap);
	    height -= num;
	    dsty += num;
	    data += bytes;
	}
	if (boxes)
	    free (boxes);
#undef h
#undef src
#undef pat
#undef size
#undef mask
#undef destOff
#undef clip
}
