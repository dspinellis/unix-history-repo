/* $Header: tile.c,v 10.3 86/02/01 15:47:46 tony Rel $ */
/* tile.c	Perform a raster operation involving a pattern
 *
 *	TileFill	Patterns a portion of the screen
 *	DrawFilled	Draw a filled generalized line/polygon/combination
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
extern char SSMap[];

char *AllocateSpace(), *AllocateCopy(), *Xalloc();

TileFill (tile, xoff, yoff, xymask, dstx, dsty, width, height,
	  clips, clipcount, func, zmask)
	register PIXMAP *tile;
	register BITMAP *xymask;
	int xoff, yoff, dstx, dsty, width, height, zmask;
	register int func;
	CLIP *clips;
{
	register CopyAreaPacket *cap;
#define	h ((PacketHeader *) cap->cap_head)
#define	pat ((Halftone *) cap->cap_source.pattern)
#define mask ((SubBitmap *) cap->cap_sourceMask)
#define	size ((Extent *) cap->cap_maskSize)
#define	destOff ((Point *) cap->cap_destOffset)
#define	clip ((RectangleList *) cap->cap_clipping.rectList)

	if (!(zmask & 1)) {
	    DeallocateSpace ();
	    return;
	}
#ifdef HTCROCK
	cap = (CopyAreaPacket *) AllocateSpace (sizeof (CopyAreaPacket) + 32);
#else
	cap = (CopyAreaPacket *) AllocateSpace (sizeof (CopyAreaPacket));
#endif
	if (cap == NULL) return;

	func = SSMap[func | (tile->kind & 0x10)];
	h->ph_copyMod.m_source = tile->kind;
	h->ph_copyMod.m_mask = xymask ? 1 : 0;
	h->ph_copyMod.m_map = MAPTYPE(func);
	h->ph_opcode = COPY_AREA;
	*(long *) h->ph_next = NULL;

	if (tile->kind == 0)
	    cap->cap_source.const = (int) tile->data;
	else {
#ifdef HTCROCK
		if ((xoff|yoff) & 0xf) {
		    *(caddr_t *) pat->ht_address = (caddr_t) cap +
						   sizeof (CopyAreaPacket) +
						   VSReloc;
		    Align_Halftone (TDATA(tile)->data,
				    (short *) ((caddr_t) cap + sizeof (CopyAreaPacket)),
				    xoff, yoff);
		} else
		    *(caddr_t *) pat->ht_address =
					BDATA(TDATA(tile)->bitmap)->vsPtr;
		pat->ht_x = pat->ht_y = 0;
#else
		*(caddr_t *) pat->ht_address = BDATA(PDATA(tile))->vsPtr;
		pat->ht_x = xoff;
		pat->ht_y = yoff;
#endif
		pat->ht_height = pat->ht_width = 16;
		pat->ht_bitsPerPixel = 1;
	}

	if (xymask) {
		*(caddr_t *) mask->sb_address = BDATA(xymask)->vsPtr;
		mask->sb_height = xymask->height;
		mask->sb_width = xymask->width;
		mask->sb_bitsPerPixel = 1;
		mask->sb_x = mask->sb_y = 0;
	}
	size->e_height = height;
	size->e_width = width;

	*(BitMap *) cap->cap_destImage = screen;
	destOff->p_x = dstx;
	destOff->p_y = dsty;

	*(short *) cap->cap_map.literal = MAPLIT(func);

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
#undef pat
#undef mask
#undef size
#undef destOff
#undef clip
}

DrawFilled (verts, vertcount, xbase, ybase, srcpix, tile, xoff, yoff,
	    clips, clipcount, func, zmask)
	Vertex *verts;
	register PIXMAP *tile;
	int vertcount, xbase, ybase, srcpix, xoff, yoff, clipcount, zmask;
	register int func;
	CLIP *clips;
{
	register FillAreaPacket *fap;
	char *boxes = NULL;
	Vertex *segs;
#define	h ((PacketHeader *) fap->fap_head)
#define	pat ((Halftone *) fap->fap_source.pattern)
#define	destOff ((Point *) fap->fap_destOffset)
#define	path ((SegmentList *) fap->fap_path)

	if (!(zmask & 1)) {
	    DeallocateSpace ();
	    return;
	}
	if (clipcount > 1) {
	    boxes = Xalloc (sizeof (CLIP) * clipcount);
	    bcopy ((caddr_t) clips, boxes, sizeof (CLIP) * clipcount);
	    clips = (CLIP *) boxes;
	    DeallocateSpace ();
	}

	if (tile && (tile->kind & 0x10))
	    func |= 0x10;
	func = SSMap[func];
	while (--clipcount >= 0) {

#ifdef HTCROCK
	fap = (FillAreaPacket *) AllocateSpace (sizeof (FillAreaPacket) + 32);
#else
	fap = (FillAreaPacket *) AllocateSpace (sizeof (FillAreaPacket));
#endif
	if (fap == NULL ||
	    (segs = (Vertex *) AllocateCopy ((caddr_t) verts, vertcount * sizeof (Vertex))) == NULL)
	    break;

	if (tile)
	    h->ph_fillMod.m_source = tile->kind;
	else
	    h->ph_fillMod.m_source = 0;
	h->ph_fillMod.m_map = MAPTYPE(func);
	h->ph_opcode = FILL_AREA;
	*(long *) h->ph_next = NULL;

	if (tile == NULL)
	    fap->fap_source.const = srcpix & 1;
	else if (tile->kind == 0)
	    fap->fap_source.const = (int) tile->data;
	else {
#ifdef HTCROCK
		if ((xoff|yoff) & 0xf) {
		    *(caddr_t *) pat->ht_address = (caddr_t) fap +
						   sizeof (FillAreaPacket) +
						   VSReloc;
		    Align_Halftone (TDATA(tile)->data,
				    (short *) ((caddr_t) fap + sizeof (FillAreaPacket)),
				    xoff, yoff);
		} else 
		    *(caddr_t *) pat->ht_address =
					BDATA(TDATA(tile)->bitmap)->vsPtr;
		pat->ht_x = pat->ht_y = 0;
#else
		*(caddr_t *) pat->ht_address = BDATA(PDATA(tile))->vsPtr;
		pat->ht_x = xoff;
		pat->ht_y = yoff;
#endif
		pat->ht_height = pat->ht_width = 16;
		pat->ht_bitsPerPixel = 1;
	}

	*(BitMap *) fap->fap_destImage = screen;
	destOff->p_x = xbase;
	destOff->p_y = ybase;

	*(short *) fap->fap_map.literal = MAPLIT(func);

	h->ph_fillMod.m_clipping = 1;
	*(CLIP *) fap->fap_clippingRec = *clips;
	clips++;

	*(caddr_t *) path->seg_first = (caddr_t) segs + VSReloc;
	path->seg_count = vertcount;

	WritePacket ((caddr_t) fap);
	}

	if (boxes)
	    free (boxes);
#undef h
#undef pat
#undef destOff
#undef path
}

#ifdef HTCROCK
Align_Halftone (src, dst, xoff, yoff)
	register short *src, *dst;
	register int xoff, yoff;
{
	register int i;
	int shift, mask;

	xoff &= 0xf;
	yoff = (16 - (yoff & 0xf)) & 0xf;
	shift = (16 - xoff) & 0xf;
	mask = (1 << xoff) - 1;
	for (i = 0; i < 16; i++) {
	    dst[i] = (src[yoff] << xoff) | ((src[yoff] >> shift) & mask);
	    yoff++;
	    yoff &= 0xf;
	}
}
#endif
