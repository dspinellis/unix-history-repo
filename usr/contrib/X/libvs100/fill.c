/* $Header: fill.c,v 10.3 86/02/01 15:46:50 tony Rel $ */
/* fill.c	Perform a simple raster operation a section of the screen
 *
 *	PixFill	Do a function on the screen
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

char *AllocateSpace();

PixFill (srcpix, xymask, dstx, dsty, width, height, clips, clipcount,
	 func, zmask)
	int srcpix, dstx, dsty, width, height, clipcount, zmask;
	register BITMAP *xymask;
	register int func;
	CLIP *clips;
{
	register CopyAreaPacket *cap;
#define	h ((PacketHeader *) cap->cap_head)
#define mask ((SubBitmap *) cap->cap_sourceMask)
#define	size ((Extent *) cap->cap_maskSize)
#define	destOff ((Point *) cap->cap_destOffset)
#define	clip ((RectangleList *) cap->cap_clipping.rectList)

	if (!(zmask & 1)) {
	    DeallocateSpace ();
	    return;
	}
	cap = (CopyAreaPacket *) AllocateSpace (sizeof (CopyAreaPacket));
	if (cap == NULL) return;

	func = SSMap[func];
	h->ph_copyMod.m_source = 0;
	h->ph_copyMod.m_mask = xymask ? 1 : 0;
	h->ph_copyMod.m_map = MAPTYPE(func);
	h->ph_opcode = COPY_AREA;
	*(long *) h->ph_next = NULL;

	cap->cap_source.const = srcpix & 1;

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
#undef mask
#undef size
#undef destOff
#undef clip
}
