/* $Header: draw.c,v 10.3 86/02/01 15:46:46 tony Rel $ */
/* draw.c	Draw lines, curves, and polygons on the screen
 *
 *	DrawCurve	Draw a generalized line/polygon/combination
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

char *AllocateCopy(), *AllocateSpace();

DrawCurve (verts, vertcount, xbase, ybase, srcpix, altpix, mode,
	   bwidth, bheight, pat, patlen, patmul, clips, clipcount, func, zmask)
	Vertex *verts;
	int vertcount, xbase, ybase, srcpix, altpix, mode, bwidth, bheight;
	int pat, patlen, patmul, clipcount, zmask;
	register int func;
	CLIP *clips;
{
	register DrawCurvePacket *dcp;
#define	h ((PacketHeader *) dcp->dcp_head)
#define	size ((Extent *) dcp->dcp_maskSize)
#define	destOff ((Point *) dcp->dcp_destOffset)
#define	path ((SegmentList *) dcp->dcp_path)
#define	clip ((RectangleList *) dcp->dcp_clipping.rectList)
#define pstr ((PatternString *) dcp->dcp_pattern)
#define pstate ((PatternState *) dcp->dcp_patState.literal)

	if (!(zmask & 1)) {
	    DeallocateSpace ();
	    return;
	}
	if ((verts = (Vertex *) AllocateCopy ((caddr_t) verts, vertcount * sizeof (Vertex))) == NULL ||
	    (dcp = (DrawCurvePacket *) AllocateSpace (sizeof (DrawCurvePacket))) == NULL)
	    return;

	func = SSMap[func];
	h->ph_drawMod.m_source = 0;	/* Always constant here */
	h->ph_drawMod.m_mask = 0;
	h->ph_drawMod.m_map = MAPTYPE(func);
	if (mode == 0) {
	    h->ph_drawMod.m_drawMode = 0;
	    h->ph_drawMod.m_patMode = 0;
	} else {
	    h->ph_drawMod.m_drawMode = 1;
	    h->ph_drawMod.m_patMode = mode - 1;
	}
	h->ph_drawMod.m_patState = 0;
	h->ph_opcode = DRAW_CURVE;
	*(long *) h->ph_next = NULL;

	dcp->dcp_source.const = srcpix & 1;
	dcp->dcp_secondSource.const = altpix & 1;

	size->e_height = bheight;
	size->e_width = bwidth;

	*(BitMap *) dcp->dcp_destImage = screen;
	destOff->p_x = xbase;
	destOff->p_y = ybase;

	*(short *) dcp->dcp_map.literal = MAPLIT(func);

	pstr->p_pattern = pat;
	pstr->p_length = patlen;
	pstr->p_multiplier = patmul;
	pstate->p_position = 0;
	pstate->p_count = 0;

	if (clipcount == 1) {
	    h->ph_drawMod.m_clipping = 1;
	    *(CLIP *) dcp->dcp_clipping.litRect = *clips;
	} else {
	    h->ph_drawMod.m_clipping = 2;
	    *(caddr_t *) clip->r_first = (caddr_t) clips + VSReloc;
	    clip->r_count = clipcount;
	}

	*(caddr_t *) path->seg_first = (caddr_t) verts + VSReloc;
	path->seg_count = vertcount;

	WritePacket ((caddr_t) dcp);
#undef h
#undef size
#undef destOff
#undef path
#undef clip
#undef pstr
#undef pstate
}
