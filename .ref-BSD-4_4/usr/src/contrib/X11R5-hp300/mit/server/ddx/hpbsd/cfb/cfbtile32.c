/*
 * Fill 32 bit tiled rectangles.  Used by both PolyFillRect and PaintWindow.
 * no depth dependencies.
 */

/*
Copyright 1989 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the software
without specific, written prior permission.  M.I.T. makes no
representations about the suitability of this software for any
purpose.  It is provided "as is" without express or implied warranty.
*/

/* $XConsortium: cfbtile32.c,v 1.5 91/07/14 13:49:46 keith Exp $ */

#include "X.h"
#include "Xmd.h"
#include "servermd.h"
#include "gcstruct.h"
#include "window.h"
#include "pixmapstr.h"
#include "scrnintstr.h"
#include "windowstr.h"

#include "cfb.h"
#include "cfbmskbits.h"
#include "cfb8bit.h"

#include "mergerop.h"

#ifdef sparc
#define SHARED_IDCACHE
#endif

#define STORE(p)    (*(p) = MROP_PREBUILT_SOLID(srcpix,*(p)))

#if (MROP == Mcopy) && defined(FAST_CONSTANT_OFFSET_MODE) && defined(SHARED_IDCACHE)
# define Expand(left,right) {\
    int part = nlwMiddle & 7; \
    nlwMiddle >>= 3; \
    while (h--) { \
	srcpix = psrc[srcy * tileStride]; \
	MROP_PREBUILD(srcpix); \
	++srcy; \
	if (srcy == tileHeight) \
	    srcy = 0; \
	left \
	p += part; \
	switch (part) { \
	case 7: \
	    STORE(p - 7); \
	case 6: \
	    STORE(p - 6); \
	case 5: \
	    STORE(p - 5); \
	case 4: \
	    STORE(p - 4); \
	case 3: \
	    STORE(p - 3); \
	case 2: \
	    STORE(p - 2); \
	case 1: \
	    STORE(p - 1); \
	} \
	nlw = nlwMiddle; \
	while (nlw) { \
	    STORE (p + 0); \
	    STORE (p + 1); \
	    STORE (p + 2); \
	    STORE (p + 3); \
	    STORE (p + 4); \
	    STORE (p + 5); \
	    STORE (p + 6); \
	    STORE (p + 7); \
	    p += 8; \
	    nlw--; \
	} \
	right \
	p += nlwExtra; \
    } \
}
#else
#define Expand(left,right) {\
    while (h--)	{ \
	srcpix = psrc[srcy * tileStride]; \
	MROP_PREBUILD(srcpix); \
	++srcy; \
	if (srcy == tileHeight) \
	    srcy = 0; \
	left \
	nlw = nlwMiddle; \
	while (nlw--) \
	{ \
	    STORE(p); \
	    p++; \
	} \
	right \
	p += nlwExtra; \
    } \
}
#endif

void
MROP_NAME(cfbFillRectTile32) (pDrawable, pGC, nBox, pBox)
    DrawablePtr	    pDrawable;
    GCPtr	    pGC;
    int		    nBox;	/* number of boxes to fill */
    BoxPtr 	    pBox;	/* pointer to list of boxes to fill */
{
    register unsigned long srcpix;	
    unsigned long *psrc;		/* pointer to bits in tile, if needed */
    int tileHeight;	/* height of the tile */

    int nlwDst;		/* width in longwords of the dest pixmap */
    int w;		/* width of current box */
    register int h;	/* height of current box */
    register unsigned long startmask;
    register unsigned long endmask; /* masks for reggedy bits at either end of line */
    int nlwMiddle;	/* number of longwords between sides of boxes */
    int nlwExtra;	/* to get from right of box to left of next span */
    register int nlw;	/* loop version of nlwMiddle */
    register unsigned long *p;	/* pointer to bits we're writing */
    int y;		/* current scan line */
    int srcy;		/* current tile position */

    unsigned long *pbits;/* pointer to start of pixmap */
    PixmapPtr	    tile;	/* rotated, expanded tile */
    hpPrivPixmapPtr tilePriv;
    int tileStride;
    MROP_DECLARE_REG()
    MROP_PREBUILT_DECLARE()

    tile = ((cfbPrivGCPtr) (pGC->devPrivates[cfbGCPrivateIndex].ptr))->pRotatedPixmap;
    tilePriv = (hpPrivPixmapPtr)tile->devPrivate.ptr;
    tileHeight = tile->drawable.height;
    tileStride = tilePriv->stride >> 2;
    psrc = (unsigned long *)tilePriv->bits;

    MROP_INITIALIZE(pGC->alu, pGC->planemask);

    cfbGetLongWidthAndPointer (pDrawable, nlwDst, pbits)

    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);
    if (tile->devKind == PIXMAP_FRAME_BUFFER)
        WAIT_READY_TO_RENDER(tile->drawable.pScreen);

    while (nBox--)
    {
	w = pBox->x2 - pBox->x1;
	h = pBox->y2 - pBox->y1;
	y = pBox->y1;
	p = pbits + (y * nlwDst) + (pBox->x1 >> PWSH);
	srcy = y % tileHeight;

	if ( ((pBox->x1 & PIM) + w) <= PPW)
	{
	    maskpartialbits(pBox->x1, w, startmask);
	    nlwExtra = nlwDst;
	    while (h--)
	    {
		srcpix = psrc[srcy * tileStride];
		MROP_PREBUILD(srcpix);
		++srcy;
		if (srcy == tileHeight)
		    srcy = 0;
		*p = MROP_PREBUILT_MASK (srcpix, *p, startmask);
		p += nlwExtra;
	    }
	}
	else
	{
	    maskbits(pBox->x1, w, startmask, endmask, nlwMiddle);
	    nlwExtra = nlwDst - nlwMiddle;

	    if (startmask)
	    {
		nlwExtra -= 1;
		if (endmask)
		{
		    Expand(*p = MROP_PREBUILT_MASK(srcpix, *p, startmask); p++;,
			   *p = MROP_PREBUILT_MASK(srcpix, *p, endmask);)
		}
		else
		{
		    Expand(*p = MROP_PREBUILT_MASK(srcpix, *p, startmask); p++;,
			   ;)
		}
	    }
	    else
	    {
		if (endmask)
		{
		    Expand(;,
			   *p = MROP_PREBUILT_MASK(srcpix, *p, endmask);)
		}
		else
		{
		    Expand(;,
			   ;)
		}
	    }
	}
        pBox++;
    }
}

void
MROP_NAME(cfbTile32FS)(pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		nInit;			/* number of spans to fill */
    DDXPointPtr pptInit;		/* pointer to list of start points */
    int		*pwidthInit;		/* pointer to list of n widths */
    int 	fSorted;
{
				/* next three parameters are post-clip */
    int			n;	/* number of spans to fill */
    DDXPointPtr		ppt;	/* pointer to list of start points */
    int			*pwidth;/* pointer to list of n widths */
    unsigned long	*pbits;	/* pointer to start of bitmap */
    int			nlwDst;	/* width in longwords of bitmap */
    register unsigned long *p;	/* pointer to current longword in bitmap */
    register int	w;	/* current span width */
    register int	nlw;
    register int	x;
    register unsigned long startmask;
    register unsigned long endmask;
    register unsigned long  srcpix;
    int			y;
    int			*pwidthFree;/* copies of the pointers to free */
    DDXPointPtr		pptFree;
    PixmapPtr		tile;
    hpPrivPixmapPtr tilePriv;
    int			tileStride;
    unsigned long	*psrc;	/* pointer to bits in tile */
    int			tileHeight;/* height of the tile */
    MROP_DECLARE_REG ()
    MROP_PREBUILT_DECLARE()

    n = nInit * miFindMaxBand(((cfbPrivGC *)(pGC->devPrivates[cfbGCPrivateIndex].ptr))->pCompositeClip);
    pwidthFree = (int *)ALLOCATE_LOCAL(n * sizeof(int));
    pptFree = (DDXPointRec *)ALLOCATE_LOCAL(n * sizeof(DDXPointRec));
    if(!pptFree || !pwidthFree)
    {
	if (pptFree) DEALLOCATE_LOCAL(pptFree);
	if (pwidthFree) DEALLOCATE_LOCAL(pwidthFree);
	return;
    }
    pwidth = pwidthFree;
    ppt = pptFree;
    n = miClipSpans(((cfbPrivGC *)(pGC->devPrivates[cfbGCPrivateIndex].ptr))->pCompositeClip,
		     pptInit, pwidthInit, nInit,
		     ppt, pwidth, fSorted);

    tile = ((cfbPrivGCPtr) (pGC->devPrivates[cfbGCPrivateIndex].ptr))->pRotatedPixmap;
    tilePriv = (hpPrivPixmapPtr)tile->devPrivate.ptr;
    tileHeight = tile->drawable.height;
    tileStride = tilePriv->stride >> 2;
    psrc = (unsigned long *)tilePriv->bits;

    MROP_INITIALIZE(pGC->alu, pGC->planemask);

    cfbGetLongWidthAndPointer (pDrawable, nlwDst, pbits)

    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);
    if (tile->devKind == PIXMAP_FRAME_BUFFER)
        WAIT_READY_TO_RENDER(tile->drawable.pScreen);

#if MROP == Mcopy
    if (!(tileHeight & (tileHeight-1)))
    {
	tileHeight--;
    	while (n--)
    	{
	    x = ppt->x;
	    y = ppt->y;
	    ++ppt;
	    w = *pwidth++;
	    p = pbits + (y * nlwDst) + (x >> PWSH);
	    srcpix = psrc[(y & tileHeight) * tileStride];
	    MROP_PREBUILD(srcpix);
    
	    if ((x & PIM) + w < PPW)
	    {
	    	maskpartialbits(x, w, startmask);
	    	*p = MROP_PREBUILT_MASK (srcpix, *p, startmask);
	    }
	    else
	    {
	    	maskbits(x, w, startmask, endmask, nlw);
	    	if (startmask)
	    	{
		    *p = MROP_PREBUILT_MASK(srcpix, *p, startmask);
		    p++;
	    	}
	    	while (nlw--)
	    	{
		    STORE(p);
		    ++p;
	    	}
	    	if (endmask)
	    	{
		    *p = MROP_PREBUILT_MASK(srcpix, *p, endmask);
	    	}
	    }
    	}
    }
    else
#endif
    {
    	while (n--)
    	{
	    x = ppt->x;
	    y = ppt->y;
	    ++ppt;
	    w = *pwidth++;
	    p = pbits + (y * nlwDst) + (x >> PWSH);
	    srcpix = psrc[(y % tileHeight) * tileStride];
	    MROP_PREBUILD(srcpix);
    
	    if ((x & PIM) + w < PPW)
	    {
	    	maskpartialbits(x, w, startmask);
	    	*p = MROP_PREBUILT_MASK (srcpix, *p, startmask);
	    }
	    else
	    {
	    	maskbits(x, w, startmask, endmask, nlw);
	    	if (startmask)
	    	{
		    *p = MROP_PREBUILT_MASK(srcpix, *p, startmask);
		    p++;
	    	}
	    	while (nlw--)
	    	{
		    STORE(p);
		    ++p;
	    	}
	    	if (endmask)
	    	{
		    *p = MROP_PREBUILT_MASK(srcpix, *p, endmask);
	    	}
	    }
    	}
    }
    DEALLOCATE_LOCAL(pptFree);
    DEALLOCATE_LOCAL(pwidthFree);
}
