/************************************************************
Copyright 1989 by The Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
no- tice appear in all copies and that both that copyright
no- tice and this permission notice appear in supporting
docu- mentation, and that the name of MIT not be used in
advertising or publicity pertaining to distribution of the
software without specific prior written permission.
M.I.T. makes no representation about the suitability of
this software for any purpose. It is provided "as is"
without any express or implied warranty.

********************************************************/

/* $XConsortium: cfbpolypnt.c,v 5.13 91/07/14 13:51:14 keith Exp $ */

#include "X.h"
#include "gcstruct.h"
#include "windowstr.h"
#include "pixmapstr.h"
#include "regionstr.h"
#include "scrnintstr.h"
#include "cfb.h"
#include "cfbmskbits.h"

#define isClipped(c,ul,lr)  ((((c) - (ul)) | ((lr) - (c))) & ClipMask)

#define PointLoop(fill) { \
    for (nbox = REGION_NUM_RECTS(cclip), pbox = REGION_RECTS(cclip); \
	 --nbox >= 0; \
	 pbox++) \
    { \
	c1 = *((long *) &pbox->x1) - off; \
	c2 = *((long *) &pbox->x2) - off - 0x00010001; \
	for (ppt = (long *) pptInit, i = npt; --i >= 0;) \
	{ \
	    pt = *ppt++; \
	    if (!isClipped(pt,c1,c2)) { \
		fill \
	    } \
	} \
    } \
}

void
cfbPolyPoint(pDrawable, pGC, mode, npt, pptInit)
    DrawablePtr pDrawable;
    GCPtr pGC;
    int mode;
    int npt;
    xPoint *pptInit;
{
    register long   pt;
    register long   c1, c2;
    register unsigned long   ClipMask = 0x80008000;
    register unsigned long   xor;
#if PPW == 4
    register unsigned char   *addrb;
    register int    nbwidth;
    unsigned char   *addrbt;
#else
    register unsigned long    *addrl;
    register int    nlwidth;
    unsigned long   *addrlt;
#endif
    register long   *ppt;
    RegionPtr	    cclip;
    int		    nbox;
    register int    i;
    register BoxPtr pbox;
    long	    and;
    int		    rop = pGC->alu;
    int		    off;
    cfbPrivGCPtr    devPriv;
    xPoint	    *pptPrev;

    devPriv = (cfbPrivGC *)(pGC->devPrivates[cfbGCPrivateIndex].ptr); 
    rop = devPriv->rop;
    if (rop == GXnoop)
	return;
    cclip = devPriv->pCompositeClip;
    xor = devPriv->xor;
    if ((mode == CoordModePrevious) && (npt > 1))
    {
	for (pptPrev = pptInit + 1, i = npt - 1; --i >= 0; pptPrev++)
	{
	    pptPrev->x += (pptPrev-1)->x;
	    pptPrev->y += (pptPrev-1)->y;
	}
    }
    off = *((int *) &pDrawable->x);
    off -= (off & 0x8000) << 1;
    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);
#if PPW == 4
    cfbGetByteWidthAndPointer(pDrawable, nbwidth, addrb);
    addrb = addrb + pDrawable->y * nbwidth + pDrawable->x;
    if (rop == GXcopy)
    {
	if (!(nbwidth & (nbwidth - 1)))
	{
	    nbwidth = ffs(nbwidth) - 1;
	    PointLoop(*(addrb + (intToY(pt) << nbwidth) + intToX(pt)) = xor;)
	}
#ifdef sun
	else if (nbwidth == 1152)
	{
	    register int    y;
	    PointLoop(y = intToY(pt); *(addrb + (y << 10) + (y << 7) + intToX(pt)) = xor;)
	}
#endif
	else
	{
	    PointLoop(*(addrb + intToY(pt) * nbwidth + intToX(pt)) = xor;)
	}
    }
    else
    {
	and = devPriv->and;
	PointLoop(  addrbt = addrb + intToY(pt) * nbwidth + intToX(pt);
		    *addrbt = DoRRop (*addrbt, and, xor);)
    }
#else
    cfbGetLongWidthAndPointer(pDrawable, nlwidth, addrl);
    and = devPriv->and;
    PointLoop(	addrlt = addrl + intToY(pt) * nlwidth + intToX(pt);
		*addrlt = DoRRop (*addrlt, and, xor); )
#endif
}
