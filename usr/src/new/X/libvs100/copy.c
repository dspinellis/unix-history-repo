/* $Header: copy.c,v 10.3 86/02/01 15:46:31 tony Rel $ */
/* copy.c	Copy one section of the framebuffer to another
 *
 *	CopyArea	Copies a section of the framebuffer
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

CopyArea (srcx, srcy, width, height, dstx, dsty, clips, clipcount, func, zmask)
	int srcx, srcy, width, height, dstx, dsty, clipcount, zmask;
	register int func;
	CLIP *clips;
{
	register CopyAreaPacket *cap;

	if (!(zmask & 1)) {
	    DeallocateSpace ();
	    return;
	}
	cap = (CopyAreaPacket *) AllocateSpace (sizeof (CopyAreaPacket));
	if(cap == NULL) return;
#define h ((PacketHeader *) cap->cap_head)
#define src ((SubBitmap *) cap->cap_source.image)
#define size ((Extent *) cap->cap_maskSize)
#define destOff ((Point *) cap->cap_destOffset)
#define clip ((RectangleList *) cap->cap_clipping.rectList)

	func = SSMap[func];
	h->ph_copyMod.m_source = 1;
 	h->ph_copyMod.m_mask = 0;
	h->ph_copyMod.m_map = MAPTYPE(func);
	h->ph_opcode = COPY_AREA;
	*(long *) h->ph_next = NULL;

	*(BitMap *) src->sb_base = screen;
	src->sb_x = srcx;
	src->sb_y = srcy;
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
#undef src
#undef size
#undef destOff
#undef clip
}
