/* Combined Purdue/PurduePlus patches, level 2.0, 1/17/89 */
/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/* $XConsortium: mfbfillsp.c,v 5.7 90/05/15 18:38:15 keith Exp $ */
#include "X.h"
#include "Xmd.h"
#include "gcstruct.h"
#include "window.h"
#include "pixmapstr.h"
#include "scrnintstr.h"
#include "windowstr.h"
#include "mfb.h"
#include "maskbits.h"

#include "mergerop.h"

#include "servermd.h"

/* scanline filling for monochrome frame buffer
   written by drewry, oct 1986

   these routines all clip.  they assume that anything that has called
them has already translated the points (i.e. pGC->miTranslate is
non-zero, which is howit gets set in mfbCreateGC().)

   the number of new scnalines created by clipping ==
MaxRectsPerBand * nSpans.

    FillSolid is overloaded to be used for OpaqueStipple as well,
if fgPixel == bgPixel.  


    FillTiled is overloaded to be used for OpaqueStipple, if
fgPixel != bgPixel.  based on the fill style, it uses
{RotatedPixmap, gc.alu} or {RotatedPixmap, PrivGC.ropOpStip}
*/


void mfbBlackSolidFS(pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		nInit;			/* number of spans to fill */
    DDXPointPtr pptInit;		/* pointer to list of start points */
    int		*pwidthInit;		/* pointer to list of n widths */
    int 	fSorted;
{
				/* next three parameters are post-clip */
    int n;			/* number of spans to fill */
    register DDXPointPtr ppt;	/* pointer to list of start points */
    register int *pwidth;	/* pointer to list of n widths */
    int *addrlBase;		/* pointer to start of bitmap */
    int nlwidth;		/* width in longwords of bitmap */
    register int *addrl;	/* pointer to current longword in bitmap */
    register int nlmiddle;
    register int startmask;
    register int endmask;
    int *pwidthFree;		/* copies of the pointers to free */
    DDXPointPtr pptFree;

    if (!(pGC->planemask & 1))
	return;

    n = nInit * miFindMaxBand(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip);
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
    n = miClipSpans(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip,
		    pptInit, pwidthInit, nInit,
		    ppt, pwidth, fSorted);

    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	addrlBase = (int *) getPrivScreenPtr(pDrawable->pScreen)->bits;
	nlwidth = (int) getPrivScreenPtr(pDrawable->pScreen)->stride >> 2;
    }
    else
    {
	addrlBase = (int *) getPrivPixmapPtr(pDrawable)->bits;
	nlwidth = (int) getPrivPixmapPtr(pDrawable)->stride >> 2;
    }

    while (n--)
    {
        addrl = addrlBase + (ppt->y * nlwidth) + (ppt->x >> 5);

	if (*pwidth)
	{
	    if ( ((ppt->x & 0x1f) + *pwidth) < 32)
	    {
		/* all bits inside same longword */
		maskpartialbits(ppt->x, *pwidth, startmask);
		    *addrl &= ~startmask;
	    }
	    else
	    {
		maskbits(ppt->x, *pwidth, startmask, endmask, nlmiddle);
		if (startmask)
		    *addrl++ &= ~startmask;
		Duff (nlmiddle, *addrl++ = 0x0);
		if (endmask)
		    *addrl &= ~endmask;
	    }
	}
	pwidth++;
	ppt++;
    }
    DEALLOCATE_LOCAL(pptFree);
    DEALLOCATE_LOCAL(pwidthFree);
}



void mfbWhiteSolidFS(pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		nInit;			/* number of spans to fill */
    DDXPointPtr pptInit;		/* pointer to list of start points */
    int		*pwidthInit;		/* pointer to list of n widths */
    int 	fSorted;
{
				/* next three parameters are post-clip */
    int n;			/* number of spans to fill */
    register DDXPointPtr ppt;	/* pointer to list of start points */
    register int *pwidth;	/* pointer to list of n widths */
    int *addrlBase;		/* pointer to start of bitmap */
    int nlwidth;		/* width in longwords of bitmap */
    register int *addrl;	/* pointer to current longword in bitmap */
    register int nlmiddle;
    register int startmask;
    register int endmask;
    int *pwidthFree;		/* copies of the pointers to free */
    DDXPointPtr pptFree;

    if (!(pGC->planemask & 1))
	return;

    n = nInit * miFindMaxBand(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip);
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
    n = miClipSpans(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip,
		    pptInit, pwidthInit, nInit,
		    ppt, pwidth, fSorted);

    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	addrlBase = (int *) getPrivScreenPtr(pDrawable->pScreen)->bits;
	nlwidth = (int) getPrivScreenPtr(pDrawable->pScreen)->stride >> 2;
    }
    else
    {
	addrlBase = (int *) getPrivPixmapPtr(pDrawable)->bits;
	nlwidth = (int) getPrivPixmapPtr(pDrawable)->stride >> 2;
    }

    while (n--)
    {
        addrl = addrlBase + (ppt->y * nlwidth) + (ppt->x >> 5);

	if (*pwidth)
	{
	    if ( ((ppt->x & 0x1f) + *pwidth) < 32)
	    {
		/* all bits inside same longword */
		maskpartialbits(ppt->x, *pwidth, startmask);
		*addrl |= startmask;
	    }
	    else
	    {
		maskbits(ppt->x, *pwidth, startmask, endmask, nlmiddle);
		if (startmask)
		    *addrl++ |= startmask;
		Duff (nlmiddle, *addrl++ = 0xffffffff);
		if (endmask)
		    *addrl |= endmask;
	    }
	}
	pwidth++;
	ppt++;
    }
    DEALLOCATE_LOCAL(pptFree);
    DEALLOCATE_LOCAL(pwidthFree);
}



void mfbInvertSolidFS(pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		nInit;			/* number of spans to fill */
    DDXPointPtr pptInit;		/* pointer to list of start points */
    int		*pwidthInit;		/* pointer to list of n widths */
    int 	fSorted;
{
				/* next three parameters are post-clip */
    int n;			/* number of spans to fill */
    register DDXPointPtr ppt;	/* pointer to list of start points */
    register int *pwidth;	/* pointer to list of n widths */
    int *addrlBase;		/* pointer to start of bitmap */
    int nlwidth;		/* width in longwords of bitmap */
    register int *addrl;	/* pointer to current longword in bitmap */
    register int nlmiddle;
    register int startmask;
    register int endmask;
    int *pwidthFree;		/* copies of the pointers to free */
    DDXPointPtr pptFree;

    if (!(pGC->planemask & 1))
	return;

    n = nInit * miFindMaxBand(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip);
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
    n = miClipSpans(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip,
		    pptInit, pwidthInit, nInit,
		    ppt, pwidth, fSorted);

    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	addrlBase = (int *) getPrivScreenPtr(pDrawable->pScreen)->bits;
	nlwidth = (int) getPrivScreenPtr(pDrawable->pScreen)->stride >> 2;
    }
    else
    {
	addrlBase = (int *) getPrivPixmapPtr(pDrawable)->bits;
	nlwidth = (int) getPrivPixmapPtr(pDrawable)->stride >> 2;
    }

    while (n--)
    {
        addrl = addrlBase + (ppt->y * nlwidth) + (ppt->x >> 5);

	if (*pwidth)
	{
	    if ( ((ppt->x & 0x1f) + *pwidth) < 32)
	    {
		/* all bits inside same longword */
		maskpartialbits(ppt->x, *pwidth, startmask);
		*addrl ^= startmask;
	    }
	    else
	    {
		maskbits(ppt->x, *pwidth, startmask, endmask, nlmiddle);
		if (startmask)
		    *addrl++ ^= startmask;
		Duff (nlmiddle, *addrl++ ^= 0xffffffff);
		if (endmask)
		    *addrl ^= endmask;
	    }
	}
	pwidth++;
	ppt++;
    }
    DEALLOCATE_LOCAL(pptFree);
    DEALLOCATE_LOCAL(pwidthFree);
}


void 
mfbWhiteStippleFS(pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
DrawablePtr pDrawable;
GC *pGC;
int nInit;			/* number of spans to fill */
DDXPointPtr pptInit;		/* pointer to list of start points */
int *pwidthInit;		/* pointer to list of n widths */
int fSorted;
{
				/* next three parameters are post-clip */
    int n;			/* number of spans to fill */
    register DDXPointPtr ppt;	/* pointer to list of start points */
    register int *pwidth;	/* pointer to list of n widths */
    int *addrlBase;		/* pointer to start of bitmap */
    int nlwidth;		/* width in longwords of bitmap */
    register int *addrl;	/* pointer to current longword in bitmap */
    register int src;
    register int nlmiddle;
    register int startmask;
    register int endmask;
    PixmapPtr pStipple;
    int *psrc;
    int tileHeight;
    int *pwidthFree;		/* copies of the pointers to free */
    DDXPointPtr pptFree;

    if (!(pGC->planemask & 1))
	return;

    n = nInit * miFindMaxBand(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip);
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
    n = miClipSpans(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip,
		    pptInit, pwidthInit, nInit, 
		    ppt, pwidth, fSorted);

    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	addrlBase = (int *) getPrivScreenPtr(pDrawable->pScreen)->bits;
	nlwidth = (int) getPrivScreenPtr(pDrawable->pScreen)->stride >> 2;
    }
    else
    {
	addrlBase = (int *) getPrivPixmapPtr(pDrawable)->bits;
	nlwidth = (int) getPrivPixmapPtr(pDrawable)->stride >> 2;
    }

    pStipple = ((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pRotatedPixmap;
    tileHeight = pStipple->drawable.height;
    psrc = (int *)(((hpPrivPixmapPtr)(pStipple->devPrivate.ptr))->bits);

    while (n--)
    {
        addrl = addrlBase + (ppt->y * nlwidth) + (ppt->x >> 5);
	src = psrc[ppt->y % tileHeight];

        /* all bits inside same longword */
        if ( ((ppt->x & 0x1f) + *pwidth) < 32)
        {
	    maskpartialbits(ppt->x, *pwidth, startmask);
	    *addrl |= (src & startmask);
        }
        else
        {
	    maskbits(ppt->x, *pwidth, startmask, endmask, nlmiddle);
	    if (startmask)
		*addrl++ |= (src & startmask);
	    Duff (nlmiddle, *addrl++ |= src);
	    if (endmask)
		*addrl |= (src & endmask);
        }
	pwidth++;
	ppt++;
    }
    DEALLOCATE_LOCAL(pptFree);
    DEALLOCATE_LOCAL(pwidthFree);
}


void 
mfbBlackStippleFS(pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
DrawablePtr pDrawable;
GC *pGC;
int nInit;			/* number of spans to fill */
DDXPointPtr pptInit;		/* pointer to list of start points */
int *pwidthInit;		/* pointer to list of n widths */
int fSorted;
{
				/* next three parameters are post-clip */
    int n;			/* number of spans to fill */
    register DDXPointPtr ppt;	/* pointer to list of start points */
    register int *pwidth;	/* pointer to list of n widths */
    int *addrlBase;		/* pointer to start of bitmap */
    int nlwidth;		/* width in longwords of bitmap */
    register int *addrl;	/* pointer to current longword in bitmap */
    register int src;
    register int nlmiddle;
    register int startmask;
    register int endmask;
    PixmapPtr pStipple;
    int *psrc;
    int tileHeight;
    int *pwidthFree;		/* copies of the pointers to free */
    DDXPointPtr pptFree;

    if (!(pGC->planemask & 1))
	return;

    n = nInit * miFindMaxBand(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip);
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
    n = miClipSpans(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip,
		    pptInit, pwidthInit, nInit, 
		    ppt, pwidth, fSorted);

    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	addrlBase = (int *) getPrivScreenPtr(pDrawable->pScreen)->bits;
	nlwidth = (int) getPrivScreenPtr(pDrawable->pScreen)->stride >> 2;
    }
    else
    {
	addrlBase = (int *) getPrivPixmapPtr(pDrawable)->bits;
	nlwidth = (int) getPrivPixmapPtr(pDrawable)->stride >> 2;
    }

    pStipple = ((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pRotatedPixmap;
    tileHeight = pStipple->drawable.height;
    psrc = (int *)(((hpPrivPixmapPtr)(pStipple->devPrivate.ptr))->bits);

    while (n--)
    {
        addrl = addrlBase + (ppt->y * nlwidth) + (ppt->x >> 5);
	src = psrc[ppt->y % tileHeight];

        /* all bits inside same longword */
        if ( ((ppt->x & 0x1f) + *pwidth) < 32)
        {
	    maskpartialbits(ppt->x, *pwidth, startmask);
	    *addrl &= ~(src & startmask);
        }
        else
        {
	    maskbits(ppt->x, *pwidth, startmask, endmask, nlmiddle);
	    if (startmask)
		*addrl++ &= ~(src & startmask);
	    Duff (nlmiddle, *addrl++ &= ~src);
	    if (endmask)
		*addrl &= ~(src & endmask);
        }
	pwidth++;
	ppt++;
    }
    DEALLOCATE_LOCAL(pptFree);
    DEALLOCATE_LOCAL(pwidthFree);
}


void 
mfbInvertStippleFS(pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
DrawablePtr pDrawable;
GC *pGC;
int nInit;			/* number of spans to fill */
DDXPointPtr pptInit;		/* pointer to list of start points */
int *pwidthInit;		/* pointer to list of n widths */
int fSorted;
{
				/* next three parameters are post-clip */
    int n;			/* number of spans to fill */
    register DDXPointPtr ppt;	/* pointer to list of start points */
    register int *pwidth;	/* pointer to list of n widths */
    int *addrlBase;		/* pointer to start of bitmap */
    int nlwidth;		/* width in longwords of bitmap */
    register int *addrl;	/* pointer to current longword in bitmap */
    register int src;
    register int nlmiddle;
    register int startmask;
    register int endmask;
    PixmapPtr pStipple;
    int *psrc;
    int tileHeight;
    int *pwidthFree;		/* copies of the pointers to free */
    DDXPointPtr pptFree;

    if (!(pGC->planemask & 1))
	return;

    n = nInit * miFindMaxBand(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip);
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
    n = miClipSpans(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip,
		    pptInit, pwidthInit, nInit, 
		    ppt, pwidth, fSorted);

    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	addrlBase = (int *) getPrivScreenPtr(pDrawable->pScreen)->bits;
	nlwidth = (int) getPrivScreenPtr(pDrawable->pScreen)->stride >> 2;
    }
    else
    {
	addrlBase = (int *) getPrivPixmapPtr(pDrawable)->bits;
	nlwidth = (int) getPrivPixmapPtr(pDrawable)->stride >> 2;
    }

    pStipple = ((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pRotatedPixmap;
    tileHeight = pStipple->drawable.height;
    psrc = (int *)(((hpPrivPixmapPtr)(pStipple->devPrivate.ptr))->bits);

    while (n--)
    {
        addrl = addrlBase + (ppt->y * nlwidth) + (ppt->x >> 5);
	src = psrc[ppt->y % tileHeight];

        /* all bits inside same longword */
        if ( ((ppt->x & 0x1f) + *pwidth) < 32)
        {
	    maskpartialbits(ppt->x, *pwidth, startmask);
	    *addrl ^= (src & startmask);
        }
        else
        {
	    maskbits(ppt->x, *pwidth, startmask, endmask, nlmiddle);
	    if (startmask)
		*addrl++ ^= (src & startmask);
	    Duff(nlmiddle, *addrl++ ^= src);
	    if (endmask)
		*addrl ^= (src & endmask);
        }
	pwidth++;
	ppt++;
    }
    DEALLOCATE_LOCAL(pptFree);
    DEALLOCATE_LOCAL(pwidthFree);
}


/* this works with tiles of width == 32 */
#define FILLSPAN32(ROP) \
    while (n--) \
    { \
	if (*pwidth) \
	{ \
            addrl = addrlBase + (ppt->y * nlwidth) + (ppt->x >> 5); \
	    src = psrc[ppt->y % tileHeight]; \
            if ( ((ppt->x & 0x1f) + *pwidth) < 32) \
            { \
	        maskpartialbits(ppt->x, *pwidth, startmask); \
	        *addrl = (*addrl & ~startmask) | \
		         (ROP(src, *addrl) & startmask); \
            } \
            else \
            { \
	        maskbits(ppt->x, *pwidth, startmask, endmask, nlmiddle); \
	        if (startmask) \
	        { \
	            *addrl = (*addrl & ~startmask) | \
			     (ROP(src, *addrl) & startmask); \
		    addrl++; \
	        } \
	        while (nlmiddle--) \
	        { \
		    *addrl = ROP(src, *addrl); \
		    addrl++; \
	        } \
	        if (endmask) \
	            *addrl = (*addrl & ~endmask) | \
			     (ROP(src, *addrl) & endmask); \
            } \
	} \
	pwidth++; \
	ppt++; \
    }



void mfbTileFS(pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
DrawablePtr pDrawable;
GC *pGC;
int nInit;			/* number of spans to fill */
DDXPointPtr pptInit;		/* pointer to list of start points */
int *pwidthInit;		/* pointer to list of n widths */
int fSorted;
{
				/* next three parameters are post-clip */
    int n;			/* number of spans to fill */
    register DDXPointPtr ppt;	/* pointer to list of start points */
    register int *pwidth;	/* pointer to list of n widths */
    int *addrlBase;		/* pointer to start of bitmap */
    int nlwidth;		/* width in longwords of bitmap */
    register int *addrl;	/* pointer to current longword in bitmap */
    register int src;
    register int nlmiddle;
    register int startmask;
    register int endmask;
    PixmapPtr pTile;
    int *psrc;
    int tileHeight;
    int rop;
    int *pwidthFree;		/* copies of the pointers to free */
    DDXPointPtr pptFree;
    unsigned long   flip;


    if (!(pGC->planemask & 1))
	return;

    n = nInit * miFindMaxBand(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip);
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
    n = miClipSpans(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip,
		    pptInit, pwidthInit, nInit, 
		    ppt, pwidth, fSorted);

    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	addrlBase = (int *) getPrivScreenPtr(pDrawable->pScreen)->bits;
	nlwidth = (int) getPrivScreenPtr(pDrawable->pScreen)->stride >> 2;
    }
    else
    {
	addrlBase = (int *) getPrivPixmapPtr(pDrawable)->bits;
	nlwidth = (int) getPrivPixmapPtr(pDrawable)->stride >> 2;
    }

    pTile = ((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pRotatedPixmap;
    tileHeight = pTile->drawable.height;
    psrc = (int *)(((hpPrivPixmapPtr)(pTile->devPrivate.ptr))->bits);
    if (pGC->fillStyle == FillTiled)
	rop = pGC->alu;
    else
	rop = ((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->ropOpStip;

    flip = 0;
    switch(rop)
    {
      case GXcopyInverted:  /* for opaque stipples */
	flip = ~0;
      case GXcopy:
	{

#define DoMaskCopyRop(src,dst,mask)	((dst) & ~(mask) | (src) & (mask))

	    while (n--)
	    {
	    	if (*pwidth)
	    	{
            	    addrl = addrlBase + (ppt->y * nlwidth) + (ppt->x >> 5);
	    	    src = psrc[ppt->y % tileHeight] ^ flip;
            	    if ( ((ppt->x & 0x1f) + *pwidth) < 32)
            	    {
	            	maskpartialbits(ppt->x, *pwidth, startmask);
			*addrl = DoMaskCopyRop (src, *addrl, startmask);
            	    }
            	    else
            	    {
	            	maskbits(ppt->x, *pwidth, startmask, endmask, nlmiddle);
	            	if (startmask)
	            	{
			    *addrl = DoMaskCopyRop (src, *addrl, startmask);
		    	    addrl++;
	            	}
	            	while (nlmiddle--)
	            	{
			    *addrl = src;
		    	    addrl++;
	            	}
	            	if (endmask)
			    *addrl = DoMaskCopyRop (src, *addrl, endmask);
            	    }
	    	}
	    	pwidth++;
	    	ppt++;
	    }
	}
	break;
      default:
	{
	    register DeclareMergeRop ();

	    InitializeMergeRop(rop,~0);
	    while (n--)
	    {
	    	if (*pwidth)
	    	{
            	    addrl = addrlBase + (ppt->y * nlwidth) + (ppt->x >> 5);
	    	    src = psrc[ppt->y % tileHeight];
            	    if ( ((ppt->x & 0x1f) + *pwidth) < 32)
            	    {
	            	maskpartialbits(ppt->x, *pwidth, startmask);
			*addrl = DoMaskMergeRop (src, *addrl, startmask);
            	    }
            	    else
            	    {
	            	maskbits(ppt->x, *pwidth, startmask, endmask, nlmiddle);
	            	if (startmask)
	            	{
			    *addrl = DoMaskMergeRop (src, *addrl, startmask);
		    	    addrl++;
	            	}
	            	while (nlmiddle--)
	            	{
			    *addrl = DoMergeRop (src, *addrl);
		    	    addrl++;
	            	}
	            	if (endmask)
			    *addrl = DoMaskMergeRop (src, *addrl, endmask);
            	    }
	    	}
	    	pwidth++;
	    	ppt++;
	    }
	}
	break;
    }
    DEALLOCATE_LOCAL(pptFree);
    DEALLOCATE_LOCAL(pwidthFree);
}


/* Fill spans with tiles that aren't 32 bits wide */
void
mfbUnnaturalTileFS(pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
DrawablePtr pDrawable;
GC		*pGC;
int		nInit;		/* number of spans to fill */
DDXPointPtr pptInit;		/* pointer to list of start points */
int *pwidthInit;		/* pointer to list of n widths */
int fSorted;
{
    int		iline;		/* first line of tile to use */
				/* next three parameters are post-clip */
    int n;			/* number of spans to fill */
    register DDXPointPtr ppt;	/* pointer to list of start points */
    register int *pwidth;	/* pointer to list of n widths */
    unsigned int *addrlBase;	/* pointer to start of bitmap */
    int		 nlwidth;	/* width in longwords of bitmap */
    register unsigned int *pdst;/* pointer to current word in bitmap */
    register unsigned int *psrc;/* pointer to current word in tile */
    register int nlMiddle;
    register int rop, nstart;
    unsigned int startmask;
    PixmapPtr	pTile;		/* pointer to tile we want to fill with */
    hpPrivPixmapPtr pPrivTile;
    int		w, width, x, xSrc, ySrc, srcStartOver, nend;
    int 	tlwidth, rem, tileWidth, tileHeight, endinc;
    unsigned int      endmask, *psrcT;
    int *pwidthFree;		/* copies of the pointers to free */
    DDXPointPtr pptFree;

    if (!(pGC->planemask & 1))
	return;

    n = nInit * miFindMaxBand(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip);
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
    n = miClipSpans(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip,
		    pptInit, pwidthInit, nInit, 
		    ppt, pwidth, fSorted);

    if (pGC->fillStyle == FillTiled)
    {
	pTile = pGC->tile.pixmap;
	rop = pGC->alu;
    }
    else
    {
	pTile = pGC->stipple;
	rop = ((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->ropOpStip;
    }

    pPrivTile = (hpPrivPixmapPtr) pTile->devPrivate.ptr;
    tlwidth = pPrivTile->stride >> 2;

     xSrc = pDrawable->x;
    ySrc = pDrawable->y;

    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	addrlBase = (unsigned int *) getPrivScreenPtr(pDrawable->pScreen)->bits;
	nlwidth = (int) getPrivScreenPtr(pDrawable->pScreen)->stride >> 2;
    }
    else
    {
	addrlBase = (unsigned int *) getPrivPixmapPtr(pDrawable)->bits;
	nlwidth = (int) getPrivPixmapPtr(pDrawable)->stride >> 2;
    }

    tileWidth = pTile->drawable.width;
    tileHeight = pTile->drawable.height;

    /* this replaces rotating the tile. Instead we just adjust the offset
     * at which we start grabbing bits from the tile.
     * Ensure that ppt->x - xSrc >= 0 and ppt->y - ySrc >= 0,
     * so that iline and rem always stay within the tile bounds.
     */
    xSrc += (pGC->patOrg.x % tileWidth) - tileWidth;
    ySrc += (pGC->patOrg.y % tileHeight) - tileHeight;

    while (n--)
    {
	iline = (ppt->y - ySrc) % tileHeight;
        pdst = addrlBase + (ppt->y * nlwidth) + (ppt->x >> 5);
        psrcT = (unsigned int *) pPrivTile->bits + (iline * tlwidth);
	x = ppt->x;

	if (*pwidth)
	{
	    width = *pwidth;
	    while(width > 0)
	    {
		psrc = psrcT;
	        w = min(tileWidth, width);
		if((rem = (x - xSrc)  % tileWidth) != 0)
		{
		    /* if we're in the middle of the tile, get
		       as many bits as will finish the span, or
		       as many as will get to the left edge of the tile,
		       or a longword worth, starting at the appropriate
		       offset in the tile.
		    */
		    w = min(min(tileWidth - rem, width), BITMAP_SCANLINE_UNIT);
		    endinc = rem / BITMAP_SCANLINE_UNIT;
		    getandputrop((psrc+endinc), (rem&0x1f), (x & 0x1f), w, pdst, rop);
		    if((x & 0x1f) + w >= 0x20)
			pdst++;
		}
		else if(((x & 0x1f) + w) < 32)
		{
		    /* doing < 32 bits is easy, and worth special-casing */
		    putbitsrop(*psrc, x & 0x1f, w, pdst, rop);
		}
		else
		{
		    /* start at the left edge of the tile,
		       and put down as much as we can
		    */
		    maskbits(x, w, startmask, endmask, nlMiddle);

	            if (startmask)
		        nstart = 32 - (x & 0x1f);
	            else
		        nstart = 0;
	            if (endmask)
	                nend = (x + w)  & 0x1f;
	            else
		        nend = 0;

	            srcStartOver = nstart > 31;

		    if(startmask)
		    {
			putbitsrop(*psrc, (x & 0x1f), nstart, pdst, rop);
			pdst++;
			if(srcStartOver)
			    psrc++;
		    }
		     
		    while(nlMiddle--)
		    {
			    getandputrop0(psrc, nstart, 32, pdst, rop);
			    pdst++;
			    psrc++;
		    }
		    if(endmask)
		    {
			getandputrop0(psrc, nstart, nend, pdst, rop);
		    }
		 }
		 x += w;
		 width -= w;
	    }
	}
	ppt++;
	pwidth++;
    }
    DEALLOCATE_LOCAL(pptFree);
    DEALLOCATE_LOCAL(pwidthFree);
}


/* Fill spans with stipples that aren't 32 bits wide */
void
mfbUnnaturalStippleFS(pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
DrawablePtr pDrawable;
GC		*pGC;
int		nInit;		/* number of spans to fill */
DDXPointPtr pptInit;		/* pointer to list of start points */
int *pwidthInit;		/* pointer to list of n widths */
int fSorted;
{
				/* next three parameters are post-clip */
    int n;			/* number of spans to fill */
    register DDXPointPtr ppt;	/* pointer to list of start points */
    register int *pwidth;	/* pointer to list of n widths */
    int		iline;		/* first line of tile to use */
    int		*addrlBase;	/* pointer to start of bitmap */
    int		 nlwidth;	/* width in longwords of bitmap */
    register int *pdst;		/* pointer to current word in bitmap */
    register int *psrc;		/* pointer to current word in tile */
    register int nlMiddle;
    register int rop, nstart;
    int startmask;
    PixmapPtr	pTile;		/* pointer to tile we want to fill with */
    hpPrivPixmapPtr pPrivTile;
    int		w, width,  x, xSrc, ySrc, srcStartOver, nend;
    int 	endmask, tlwidth, rem, tileWidth, *psrcT, endinc;
    int		tileHeight;
    int *pwidthFree;		/* copies of the pointers to free */
    DDXPointPtr pptFree;

    if (!(pGC->planemask & 1))
	return;

    n = nInit * miFindMaxBand(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip);
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
    n = miClipSpans(((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip,
		    pptInit, pwidthInit, nInit, 
		    ppt, pwidth, fSorted);

    pTile = pGC->stipple;
    pPrivTile = (hpPrivPixmapPtr) pTile->devPrivate.ptr;
    rop = ((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->rop;
    tlwidth = pPrivTile->stride >> 2;
    xSrc = pDrawable->x;
    ySrc = pDrawable->y;
    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	addrlBase = (int *) getPrivScreenPtr(pDrawable->pScreen)->bits;
	nlwidth = (int) getPrivScreenPtr(pDrawable->pScreen)->stride >> 2;
    }
    else
    {
	addrlBase = (int *) getPrivPixmapPtr(pDrawable)->bits;
	nlwidth = (int) getPrivPixmapPtr(pDrawable)->stride >> 2;
    }

    tileWidth = pTile->drawable.width;
    tileHeight = pTile->drawable.height;

    /* this replaces rotating the stipple.  Instead, we just adjust the offset
     * at which we start grabbing bits from the stipple.
     * Ensure that ppt->x - xSrc >= 0 and ppt->y - ySrc >= 0,
     * so that iline and rem always stay within the tile bounds.
     */
    xSrc += (pGC->patOrg.x % tileWidth) - tileWidth;
    ySrc += (pGC->patOrg.y % tileHeight) - tileHeight;
    while (n--)
    {
	iline = (ppt->y - ySrc) % tileHeight;
        pdst = addrlBase + (ppt->y * nlwidth) + (ppt->x >> 5);
        psrcT = (int *) pPrivTile->bits + (iline * tlwidth);
	x = ppt->x;

	if (*pwidth)
	{
	    width = *pwidth;
	    while(width > 0)
	    {
		psrc = psrcT;
	        w = min(tileWidth, width);
		if((rem = (x - xSrc) % tileWidth) != 0)
		{
		    /* if we're in the middle of the tile, get
		       as many bits as will finish the span, or
		       as many as will get to the left edge of the tile,
		       or a longword worth, starting at the appropriate
		       offset in the tile.
		    */
		    w = min(min(tileWidth - rem, width), BITMAP_SCANLINE_UNIT);
		    endinc = rem / BITMAP_SCANLINE_UNIT;
		    getandputrrop((psrc + endinc), (rem & 0x1f), (x & 0x1f),
				 w, pdst, rop)
		    if((x & 0x1f) + w >= 0x20)
			pdst++;
		}

		else if(((x & 0x1f) + w) < 32)
		{
		    /* doing < 32 bits is easy, and worth special-casing */
		    putbitsrrop(*psrc, x & 0x1f, w, pdst, rop);
		}
		else
		{
		    /* start at the left edge of the tile,
		       and put down as much as we can
		    */
		    maskbits(x, w, startmask, endmask, nlMiddle);

	            if (startmask)
		        nstart = 32 - (x & 0x1f);
	            else
		        nstart = 0;
	            if (endmask)
	                nend = (x + w)  & 0x1f;
	            else
		        nend = 0;

	            srcStartOver = nstart > 31;

		    if(startmask)
		    {
			putbitsrrop(*psrc, (x & 0x1f), nstart, pdst, rop);
			pdst++;
			if(srcStartOver)
			    psrc++;
		    }
		     
		    while(nlMiddle--)
		    {
			    getandputrrop0(psrc, nstart, 32, pdst, rop);
			    pdst++;
			    psrc++;
		    }
		    if(endmask)
		    {
			getandputrrop0(psrc, nstart, nend, pdst, rop);
		    }
		 }
		 x += w;
		 width -= w;
	    }
	}
	ppt++;
	pwidth++;
    }
    DEALLOCATE_LOCAL(pptFree);
    DEALLOCATE_LOCAL(pwidthFree);
}
