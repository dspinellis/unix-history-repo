/************************************************************
Copyright 1989 by The Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of MIT not be used in
advertising or publicity pertaining to distribution of the
software without specific prior written permission.
M.I.T. makes no representation about the suitability of
this software for any purpose. It is provided "as is"
without any express or implied warranty.

********************************************************/

/* $XConsortium: mfbfillarc.c,v 5.7 90/10/06 13:58:08 rws Exp $ */

#include "X.h"
#include "Xprotostr.h"
#include "miscstruct.h"
#include "gcstruct.h"
#include "pixmapstr.h"
#include "scrnintstr.h"
#include "mfb.h"
#include "maskbits.h"
#include "mifillarc.h"

extern void miPolyFillArc();

static void
mfbFillEllipseSolid(pDraw, arc, rop)
    DrawablePtr pDraw;
    xArc *arc;
    register int rop;
{
    int x, y, e;
    int yk, xk, ym, xm, dx, dy, xorg, yorg;
    register int slw;
    miFillArcRec info;
    int *addrlt, *addrlb;
    register int *addrl;
    register int n;
    int nlwidth;
    register int xpos;
    int startmask, endmask, nlmiddle;

    if (pDraw->type == DRAWABLE_WINDOW)
    {
	addrlt = (int *) getPrivScreenPtr(pDraw->pScreen)->bits;
	nlwidth = (int) getPrivScreenPtr(pDraw->pScreen)->stride >> 2;
    }
    else
    {
	addrlt = (int *) getPrivPixmapPtr(pDraw)->bits;
	nlwidth = (int) getPrivPixmapPtr(pDraw)->stride >> 2;
    }
    miFillArcSetup(arc, &info);
    MIFILLARCSETUP();
    xorg += pDraw->x;
    yorg += pDraw->y;
    addrlb = addrlt;
    addrlt += nlwidth * (yorg - y);
    addrlb += nlwidth * (yorg + y + dy);
    while (y)
    {
	addrlt += nlwidth;
	addrlb -= nlwidth;
	MIFILLARCSTEP(slw);
	if (!slw)
	    continue;
	xpos = xorg - x;
	addrl = addrlt + (xpos >> 5);
	if (((xpos & 0x1f) + slw) < 32)
	{
	    maskpartialbits(xpos, slw, startmask);
	    if (rop == RROP_BLACK)
		*addrl &= ~startmask;
	    else if (rop == RROP_WHITE)
		*addrl |= startmask;
	    else
		*addrl ^= startmask;
	    if (miFillArcLower(slw))
	    {
		addrl = addrlb + (xpos >> 5);
		if (rop == RROP_BLACK)
		    *addrl &= ~startmask;
		else if (rop == RROP_WHITE)
		    *addrl |= startmask;
		else
		    *addrl ^= startmask;
	    }
	    continue;
	}
	maskbits(xpos, slw, startmask, endmask, nlmiddle);
	if (startmask)
	{
	    if (rop == RROP_BLACK)
		*addrl++ &= ~startmask;
	    else if (rop == RROP_WHITE)
		*addrl++ |= startmask;
	    else
		*addrl++ ^= startmask;
	}
	n = nlmiddle;
	if (rop == RROP_BLACK)
	    while (n--)
		*addrl++ = 0;
	else if (rop == RROP_WHITE)
	    while (n--)
		*addrl++ = ~0;
	else
	    while (n--)
		*addrl++ ^= ~0;
	if (endmask)
	{
	    if (rop == RROP_BLACK)
		*addrl &= ~endmask;
	    else if (rop == RROP_WHITE)
		*addrl |= endmask;
	    else
		*addrl ^= endmask;
	}
	if (!miFillArcLower(slw))
	    continue;
	addrl = addrlb + (xpos >> 5);
	if (startmask)
	{
	    if (rop == RROP_BLACK)
		*addrl++ &= ~startmask;
	    else if (rop == RROP_WHITE)
		*addrl++ |= startmask;
	    else
		*addrl++ ^= startmask;
	}
	n = nlmiddle;
	if (rop == RROP_BLACK)
	    while (n--)
		*addrl++ = 0;
	else if (rop == RROP_WHITE)
	    while (n--)
		*addrl++ = ~0;
	else
	    while (n--)
		*addrl++ ^= ~0;
	if (endmask)
	{
	    if (rop == RROP_BLACK)
		*addrl &= ~endmask;
	    else if (rop == RROP_WHITE)
		*addrl |= endmask;
	    else
		*addrl ^= endmask;
	}
    }
}

#define FILLSPAN(xl,xr,addr) \
    if (xr >= xl) \
    { \
	width = xr - xl + 1; \
	addrl = addr + (xl >> 5); \
	if (((xl & 0x1f) + width) < 32) \
	{ \
	    maskpartialbits(xl, width, startmask); \
	    if (rop == RROP_BLACK) \
		*addrl &= ~startmask; \
	    else if (rop == RROP_WHITE) \
		*addrl |= startmask; \
	    else \
		*addrl ^= startmask; \
	} \
	else \
	{ \
	    maskbits(xl, width, startmask, endmask, nlmiddle); \
	    if (startmask) \
	    { \
		if (rop == RROP_BLACK) \
		    *addrl++ &= ~startmask; \
		else if (rop == RROP_WHITE) \
		    *addrl++ |= startmask; \
		else \
		    *addrl++ ^= startmask; \
	    } \
	    n = nlmiddle; \
	    if (rop == RROP_BLACK) \
		while (n--) \
		    *addrl++ = 0; \
	    else if (rop == RROP_WHITE) \
		while (n--) \
		    *addrl++ = ~0; \
	    else \
		while (n--) \
		    *addrl++ ^= ~0; \
	    if (endmask) \
	    { \
		if (rop == RROP_BLACK) \
		    *addrl &= ~endmask; \
		else if (rop == RROP_WHITE) \
		    *addrl |= endmask; \
		else \
		    *addrl ^= endmask; \
	    } \
	} \
    }

#define FILLSLICESPANS(flip,addr) \
    if (!flip) \
    { \
	FILLSPAN(xl, xr, addr); \
    } \
    else \
    { \
	xc = xorg - x; \
	FILLSPAN(xc, xr, addr); \
	xc += slw - 1; \
	FILLSPAN(xl, xc, addr); \
    }

static void
mfbFillArcSliceSolidCopy(pDraw, pGC, arc, rop)
    DrawablePtr pDraw;
    GCPtr pGC;
    xArc *arc;
    register int rop;
{
    register int *addrl;
    register int n;
    int yk, xk, ym, xm, dx, dy, xorg, yorg, slw;
    register int x, y, e;
    miFillArcRec info;
    miArcSliceRec slice;
    int xl, xr, xc;
    int *addrlt, *addrlb;
    int nlwidth;
    int width;
    int startmask, endmask, nlmiddle;

    if (pDraw->type == DRAWABLE_WINDOW)
    {
	addrlt = (int *) getPrivScreenPtr(pDraw->pScreen)->bits;
	nlwidth = (int) getPrivScreenPtr(pDraw->pScreen)->stride >> 2;
    }
    else
    {
	addrlt = (int *) getPrivPixmapPtr(pDraw)->bits;
	nlwidth = (int) getPrivPixmapPtr(pDraw)->stride >> 2;
    }
    miFillArcSetup(arc, &info);
    miFillArcSliceSetup(arc, &slice, pGC);
    MIFILLARCSETUP();
    xorg += pDraw->x;
    yorg += pDraw->y;
    addrlb = addrlt;
    addrlt += nlwidth * (yorg - y);
    addrlb += nlwidth * (yorg + y + dy);
    slice.edge1.x += pDraw->x;
    slice.edge2.x += pDraw->x;
    while (y > 0)
    {
	addrlt += nlwidth;
	addrlb -= nlwidth;
	MIFILLARCSTEP(slw);
	MIARCSLICESTEP(slice.edge1);
	MIARCSLICESTEP(slice.edge2);
	if (miFillSliceUpper(slice))
	{
	    MIARCSLICEUPPER(xl, xr, slice, slw);
	    FILLSLICESPANS(slice.flip_top, addrlt);
	}
	if (miFillSliceLower(slice))
	{
	    MIARCSLICELOWER(xl, xr, slice, slw);
	    FILLSLICESPANS(slice.flip_bot, addrlb);
	}
    }
}

void
mfbPolyFillArcSolid(pDraw, pGC, narcs, parcs)
    register DrawablePtr pDraw;
    GCPtr	pGC;
    int		narcs;
    xArc	*parcs;
{
    mfbPrivGC *priv;
    register xArc *arc;
    register int i;
    BoxRec box;
    RegionPtr cclip;
    int rop;

    priv = (mfbPrivGC *) pGC->devPrivates[mfbGCPrivateIndex].ptr;
    rop = priv->rop;
    if ((rop == RROP_NOP) || !(pGC->planemask & 1))
	return;
    cclip = priv->pCompositeClip;
    for (arc = parcs, i = narcs; --i >= 0; arc++)
    {
	if (miFillArcEmpty(arc))
	    continue;
	if (miCanFillArc(arc))
	{
	    box.x1 = arc->x + pDraw->x;
	    box.y1 = arc->y + pDraw->y;
	    box.x2 = box.x1 + (int)arc->width + 1;
	    box.y2 = box.y1 + (int)arc->height + 1;
	    if ((*pDraw->pScreen->RectIn)(cclip, &box) == rgnIN)
	    {
		if ((arc->angle2 >= FULLCIRCLE) ||
		    (arc->angle2 <= -FULLCIRCLE))
		    mfbFillEllipseSolid(pDraw, arc, rop);
		else
		    mfbFillArcSliceSolidCopy(pDraw, pGC, arc, rop);
		continue;
	    }
	}
	miPolyFillArc(pDraw, pGC, 1, arc);
    }
}
