/* $Header: cursor.c,v 10.3 86/02/01 15:46:34 tony Rel $ */
/* cursor.c	various stuff with the mouse & cursor
 *
 *	StoreCursor		Creates a cursor
 *	FreeCursor		Frees the storage taken by a cursor
 *	LoadCursor		Loads a bitmap to use as cursor
 *	InitMouse		Initialize the mouse
 *	SetCursorPosition	Forces cursor to a particular position
 *	SetMouseCharacteristics	Controls speed of cursor relative to mouse
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
#include "vsioctl.h"

extern vsIoAddr *VSAddr;
extern BitMap screen;
extern char FBMap[];

char *Xalloc(), *AllocateSpace();

CURSOR *StoreCursor (func, image, fore, back, mask, xoff, yoff)
	register BITMAP *image, *mask;
	int func, fore, back, xoff, yoff;
{
	register CURSOR *cursor;
	register CursPriv *data;

	cursor = (CURSOR *) Xalloc (sizeof (CURSOR));
	cursor->width = image->width;
	cursor->height = image->height;
	cursor->xoff = xoff;
	cursor->yoff = yoff;
	cursor->xmin = xoff;
	cursor->ymin = yoff;
	cursor->xmax = screen.bm_width - (image->width - xoff);
	cursor->ymax = screen.bm_height - (image->height - yoff);
	cursor->refcnt = 1;
	data = (CursPriv *) Xalloc (sizeof (CursPriv));
	data->image = image;
	image->refcnt++;
	if (data->mask = mask)
	    mask->refcnt++;
	if (fore & 1)
	    func += 0x20;
	if (back & 1)
	    func += 0x10;
	data->map = FBMap[func];
	cursor->data = (caddr_t) data;
	return (cursor);
}

FreeCursor (cursor)
	register CURSOR *cursor;
{
	register CursPriv *data;
	register BITMAP *bm;

	data = CDATA(cursor);
	if ((bm = data->image) && --bm->refcnt == 0)
	    FreeBitmap (bm);
	if ((bm = data->mask) && --bm->refcnt == 0)
	    FreeBitmap (bm);
	free ((caddr_t) cursor);
}

LoadCursor (cursor)
	register CURSOR *cursor;
{
	register CursPriv *data;
	register BITMAP *bm;
	register LoadCursorPacket *lcp;

	lcp = (LoadCursorPacket *) AllocateSpace (sizeof (LoadCursorPacket));
	if (lcp == NULL) return;
#define h ((PacketHeader *) lcp->lcp_head)
#define src ((SubBitmap *) lcp->lcp_source.image)
#define msk ((SubBitmap *) lcp->lcp_sourceMask)
#define pat ((Halftone *) lcp->lcp_source.pattern)
#define	size ((Extent *) lcp->lcp_maskSize)

	data = CDATA(cursor);
	h->ph_cursorMod.m_source = 1;
	h->ph_cursorMod.m_map = MAPTYPE(data->map);
	h->ph_opcode = LOAD_CURSOR;
	*(long *) h->ph_next = NULL;

	bm = data->image;
	*(caddr_t *) src->sb_address = BDATA(bm)->vsPtr;
	src->sb_height = bm->height;
	src->sb_width = bm->width;
	src->sb_bitsPerPixel = 1;
	src->sb_x = src->sb_y = 0;
	size->e_height = bm->height;
	size->e_width = bm->width;

	if (bm = data->mask) {
	    h->ph_cursorMod.m_mask = 1;
	    *(caddr_t *) msk->sb_address = BDATA(bm)->vsPtr;
	    msk->sb_height = bm->height;
	    msk->sb_width = bm->width;
	    msk->sb_bitsPerPixel = 1;
	    msk->sb_x = msk->sb_y = 0;
	} else
	    h->ph_cursorMod.m_mask = 0;
	*(short *) lcp->lcp_map.literal = MAPLIT(data->map);

	lcp->lcp_blink = 0;
	lcp->lcp_tip_x = cursor->xoff;
	lcp->lcp_tip_y = cursor->yoff;
	lcp->lcp_center_x = cursor->xoff;
	lcp->lcp_center_y = cursor->yoff;

	WritePacket ((caddr_t) lcp);
#undef h
#undef src
#undef msk
#undef pat
#undef size
}

InitMouse ()
{
	register AttachCursorPacket *acp;
	register SetPointingDeviceReportingPacket *spdp;

	acp = (AttachCursorPacket *) AllocateSpace (sizeof (AttachCursorPacket));
	if (acp == NULL) return;
#define h ((PacketHeader *) acp->acp_head)

	h->ph_modifier.emptymod = 0;
	h->ph_opcode = ATTACH_CURSOR;
	*(long *) h->ph_next = NULL;

	acp->acp_device = 1;

	WritePacket ((caddr_t) acp);
#undef h

	spdp = (SetPointingDeviceReportingPacket *) AllocateSpace (sizeof (SetPointingDeviceReportingPacket));
	if (spdp == NULL) return;
#define h ((PacketHeader *) spdp->spdp_head)

	h->ph_modifier.emptymod = 0;
	h->ph_opcode = SET_POINTING_DEVICE_REPORTING;
	*(long *) h->ph_next = NULL;

	spdp->spdp_enable = 1;

	WritePacket ((caddr_t) spdp);
#undef h
}

SetCursorPosition(pos)
	register vsCursor *pos;
{
	register SetCursorPositionPacket *scp;

	scp = (SetCursorPositionPacket *) AllocateSpace (sizeof (SetCursorPositionPacket));
	if (scp == NULL) return;
#define h ((PacketHeader *) scp->scp_head)
#define loc ((Point *) scp->scp_position)

	h->ph_modifier.emptymod = 0;
	h->ph_opcode = SET_CURSOR_POSITION;
	*(long *) h->ph_next = NULL;

	loc->p_x = pos->x;
	loc->p_y = pos->y;

	WritePacket ((caddr_t) scp);
	VSAddr->mouse = *pos;
#undef h
#undef loc
}

SetMouseCharacteristics (threshold, acceleration)
	int threshold, acceleration;
{
	register SetMouseCharacteristicsPacket *smp;

	smp = (SetMouseCharacteristicsPacket *) AllocateSpace (sizeof (SetMouseCharacteristicsPacket));
	if (smp == NULL) return;
#define h ((PacketHeader *) smp->smc_head)

	h->ph_mouseMod.m_tracking = 1;
	h->ph_opcode = SET_MOUSE_CHARACTERISTICS;
	*(long *) h->ph_next = NULL;

	smp->smc_scale = acceleration;
	smp->smc_threshold = threshold;

	WritePacket ((caddr_t) smp);
#undef h
}
