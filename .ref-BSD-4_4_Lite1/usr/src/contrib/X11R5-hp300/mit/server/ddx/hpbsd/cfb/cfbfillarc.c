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

/* $XConsortium: cfbfillarc.c,v 5.12 91/04/10 11:41:49 keith Exp $ */

#include "X.h"
#include "Xprotostr.h"
#include "miscstruct.h"
#include "gcstruct.h"
#include "pixmapstr.h"
#include "scrnintstr.h"
#include "cfb.h"
#include "cfbmskbits.h"
#include "mifillarc.h"
#include "cfbrrop.h"

extern void miPolyFillArc();

/* gcc 1.35 is stupid */
#if defined(__GNUC__) && defined(mc68020)
#define STUPID volatile
#else
#define STUPID
#endif

static void
RROP_NAME(cfbFillEllipseSolid) (pDraw, pGC, arc)
    DrawablePtr pDraw;
    GCPtr pGC;
    xArc *arc;
{
    STUPID int x, y, e;
    STUPID int yk, xk, ym, xm, dx, dy, xorg, yorg;
    miFillArcRec info;
    unsigned long *addrlt, *addrlb;
    register unsigned long *addrl;
    register int n;
    int nlwidth;
    RROP_DECLARE
    register int xpos;
    register int slw;
    unsigned long startmask, endmask;
    int	nlmiddle;

    cfbGetLongWidthAndPointer (pDraw, nlwidth, addrlt)

    RROP_FETCH_GC(pGC);
    miFillArcSetup(arc, &info);
    MIFILLARCSETUP();
    xorg += pDraw->x;
    yorg += pDraw->y;
    addrlb = addrlt;
    addrlt += nlwidth * (yorg - y);
    addrlb += nlwidth * (yorg + y + dy);

    SET_REGISTERS_FOR_WRITING(pDraw->pScreen, ~0, GXcopy);

    while (y)
    {
	addrlt += nlwidth;
	addrlb -= nlwidth;
	MIFILLARCSTEP(slw);
	if (!slw)
	    continue;
	xpos = xorg - x;
	addrl = addrlt + (xpos >> PWSH);
	if (((xpos & PIM) + slw) <= PPW)
	{
	    maskpartialbits(xpos, slw, startmask);
	    RROP_SOLID_MASK(addrl,startmask);
	    if (miFillArcLower(slw))
	    {
		addrl = addrlb + (xpos >> PWSH);
		RROP_SOLID_MASK(addrl, startmask);
	    }
	    continue;
	}
	maskbits(xpos, slw, startmask, endmask, nlmiddle);
	if (startmask)
	{
	    RROP_SOLID_MASK(addrl, startmask);
	    addrl++;
	}
	n = nlmiddle;
	RROP_SPAN(addrl,n)

	if (endmask)
	    RROP_SOLID_MASK(addrl, endmask);
	if (!miFillArcLower(slw))
	    continue;
	addrl = addrlb + (xpos >> PWSH);
	if (startmask)
	{
	    RROP_SOLID_MASK(addrl, startmask);
	    addrl++;
	}
	n = nlmiddle;
	RROP_SPAN(addrl, n);
	if (endmask)
	    RROP_SOLID_MASK(addrl, endmask);
    }
}

#define FILLSPAN(xl,xr,addr) \
    if (xr >= xl) \
    { \
	n = xr - xl + 1; \
	addrl = addr + (xl >> PWSH); \
	if (((xl & PIM) + n) <= PPW) \
	{ \
	    maskpartialbits(xl, n, startmask); \
	    RROP_SOLID_MASK(addrl, startmask); \
	} \
	else \
	{ \
	    maskbits(xl, n, startmask, endmask, n); \
	    if (startmask) \
	    { \
		RROP_SOLID_MASK(addrl, startmask); \
		addrl++; \
	    } \
	    while (n--) \
	    { \
		RROP_SOLID(addrl); \
		++addrl; \
	    } \
	    if (endmask) \
		RROP_SOLID_MASK(addrl, endmask); \
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
RROP_NAME(cfbFillArcSliceSolid)(pDraw, pGC, arc)
    DrawablePtr pDraw;
    GCPtr pGC;
    xArc *arc;
{
    int yk, xk, ym, xm, dx, dy, xorg, yorg, slw;
    register int x, y, e;
    miFillArcRec info;
    miArcSliceRec slice;
    int xl, xr, xc;
    unsigned long *addrlt, *addrlb;
    register unsigned long *addrl;
    register int n;
    int nlwidth;
    RROP_DECLARE
    unsigned long startmask, endmask;

    cfbGetLongWidthAndPointer (pDraw, nlwidth, addrlt)

    RROP_FETCH_GC(pGC);
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

    SET_REGISTERS_FOR_WRITING(pDraw->pScreen, ~0, GXcopy);

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
RROP_NAME(cfbPolyFillArcSolid) (pDraw, pGC, narcs, parcs)
    DrawablePtr	pDraw;
    GCPtr	pGC;
    int		narcs;
    xArc	*parcs;
{
    register xArc *arc;
    register int i;
    BoxRec box;
    RegionPtr cclip;

    cclip = ((cfbPrivGC *)(pGC->devPrivates[cfbGCPrivateIndex].ptr))->pCompositeClip;
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
		    RROP_NAME(cfbFillEllipseSolid)(pDraw, pGC, arc);
		else
		    RROP_NAME(cfbFillArcSliceSolid)(pDraw, pGC, arc);
		continue;
	    }
	}
	miPolyFillArc(pDraw, pGC, 1, arc);
    }
}
