/************************************************************
Copyright 1987 by Sun Microsystems, Inc. Mountain View, CA.

                    All Rights Reserved

Permission  to  use,  copy,  modify,  and  distribute   this
software  and  its documentation for any purpose and without
fee is hereby granted, provided that the above copyright no-
tice  appear  in all copies and that both that copyright no-
tice and this permission notice appear in  supporting  docu-
mentation,  and  that the names of Sun or MIT not be used in
advertising or publicity pertaining to distribution  of  the
software  without specific prior written permission. Sun and
M.I.T. make no representations about the suitability of this
software for any purpose. It is provided "as is" without any
express or implied warranty.

SUN DISCLAIMS ALL WARRANTIES WITH REGARD TO  THIS  SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FIT-
NESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL SUN BE  LI-
ABLE  FOR  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,  DATA  OR
PROFITS,  WHETHER  IN  AN  ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
THE USE OR PERFORMANCE OF THIS SOFTWARE.

********************************************************/

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

/* $XConsortium: cfbfillsp.c,v 5.17 91/07/18 23:31:04 keith Exp $ */

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

#include "mergerop.h"

#if PPW == 4
#include "cfb8bit.h"
#endif

extern void mfbInvertSolidFS(), mfbBlackSolidFS(), mfbWhiteSolidFS();

/* scanline filling for color frame buffer
   written by drewry, oct 1986 modified by smarks
   changes for compatibility with Little-endian systems Jul 1987; MIT:yba.

   these routines all clip.  they assume that anything that has called
them has already translated the points (i.e. pGC->miTranslate is
non-zero, which is howit gets set in cfbCreateGC().)

   the number of new scnalines created by clipping ==
MaxRectsPerBand * nSpans.

    FillSolid is overloaded to be used for OpaqueStipple as well,
if fgPixel == bgPixel.  
Note that for solids, PrivGC.rop == PrivGC.ropOpStip


    FillTiled is overloaded to be used for OpaqueStipple, if
fgPixel != bgPixel.  based on the fill style, it uses
{RotatedTile, gc.alu} or {RotatedStipple, PrivGC.ropOpStip}
*/

#ifdef	notdef
#include	<stdio.h>
static
dumpspans(n, ppt, pwidth)
    int	n;
    DDXPointPtr ppt;
    int *pwidth;
{
    fprintf(stderr,"%d spans\n", n);
    while (n--) {
	fprintf(stderr, "[%d,%d] %d\n", ppt->x, ppt->y, *pwidth);
	ppt++;
	pwidth++;
    }
    fprintf(stderr, "\n");
}
#endif

/* Fill spans with tiles that aren't 32 bits wide */
void
cfbUnnaturalTileFS(pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
DrawablePtr pDrawable;
GC		*pGC;
int		nInit;		/* number of spans to fill */
DDXPointPtr pptInit;		/* pointer to list of start points */
int *pwidthInit;		/* pointer to list of n widths */
int fSorted;
{
    int n;			/* number of spans to fill */
    register DDXPointPtr ppt;	/* pointer to list of start points */
    register int *pwidth;	/* pointer to list of n widths */
    void    (*fill)();
    extern void	cfbFillSpanTileOddCopy ();
    extern void	cfbFillSpanTileOddGeneral ();
    extern void	cfbFillSpanTile32sCopy ();
    extern void cfbFillSpanTile32sGeneral ();
    int	xrot, yrot;

    if (!(pGC->planemask))
	return;

    if (pGC->tile.pixmap->drawable.width & PIM)
    {
    	fill = cfbFillSpanTileOddGeneral;
    	if ((pGC->planemask & PMSK) == PMSK)
    	{
	    if (pGC->alu == GXcopy)
	    	fill = cfbFillSpanTileOddCopy;
    	}
    }
    else
    {
	fill = cfbFillSpanTile32sGeneral;
    	if ((pGC->planemask & PMSK) == PMSK)
    	{
	    if (pGC->alu == GXcopy)
		fill = cfbFillSpanTile32sCopy;
	}
    }
    n = nInit * miFindMaxBand(((cfbPrivGC *)(pGC->devPrivates[cfbGCPrivateIndex].ptr))->pCompositeClip);
    pwidth = (int *)ALLOCATE_LOCAL(n * sizeof(int));
    ppt = (DDXPointRec *)ALLOCATE_LOCAL(n * sizeof(DDXPointRec));
    if(!ppt || !pwidth)
    {
	if (ppt) DEALLOCATE_LOCAL(ppt);
	if (pwidth) DEALLOCATE_LOCAL(pwidth);
	return;
    }
    n = miClipSpans(((cfbPrivGC *)(pGC->devPrivates[cfbGCPrivateIndex].ptr))->pCompositeClip,
		     pptInit, pwidthInit, nInit, 
		     ppt, pwidth, fSorted);

    xrot = pDrawable->x + pGC->patOrg.x;
    yrot = pDrawable->y + pGC->patOrg.y;

    (*fill) (pDrawable, n, ppt, pwidth, pGC->tile.pixmap, xrot, yrot, pGC->alu, pGC->planemask);

    DEALLOCATE_LOCAL(ppt);
    DEALLOCATE_LOCAL(pwidth);
}

#if PPW == 4

void
cfbUnnaturalStippleFS(pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
DrawablePtr pDrawable;
GC		*pGC;
int		nInit;		/* number of spans to fill */
DDXPointPtr pptInit;		/* pointer to list of start points */
int *pwidthInit;		/* pointer to list of n widths */
int fSorted;
{
				/* next three parameters are post-clip */
    int		    n;		/* number of spans to fill */
    DDXPointPtr	    ppt;	/* pointer to list of start points */
    int		    *pwidth;	/* pointer to list of n widths */
    int		    *pwidthFree;/* copies of the pointers to free */
    DDXPointPtr	    pptFree;
    unsigned long   *pdstBase;	/* pointer to start of bitmap */
    int		    nlwDst;	/* width in longwords of bitmap */
    register unsigned long    *pdst;	/* pointer to current word in bitmap */
    PixmapPtr	    pStipple;	/* pointer to stipple we want to fill with */
    hpPrivPixmapPtr pPrivStipple; /* private data of stipple */
    int		    nlw;
    int		    x, y, w, xrem, xSrc, ySrc;
    int		    stwidth, stippleWidth;
    int		    stippleHeight;
    register unsigned long  bits, inputBits;
    register int    partBitsLeft;
    int		    nextPartBits;
    int		    bitsLeft, bitsWhole;
    unsigned long   *srcTemp, *srcStart;
    unsigned long   *psrcBase;
    unsigned long   startmask, endmask;

    if (pGC->fillStyle == FillStippled)
	cfb8CheckStipple (pGC->alu, pGC->fgPixel, pGC->planemask);
    else
	cfb8CheckOpaqueStipple (pGC->alu, pGC->fgPixel, pGC->bgPixel, pGC->planemask);

    if (cfb8StippleRRop == GXnoop)
	return;

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

    /*
     *  OK,  so what's going on here?  We have two Drawables:
     *
     *  The Stipple:
     *		Depth = 1
     *		Width = stippleWidth
     *		Words per scanline = stwidth
     *		Pointer to pixels = pStipple->devPrivate.ptr
     */

    pStipple = pGC->stipple;
    pPrivStipple = (hpPrivPixmapPtr)(pStipple->devPrivate.ptr);

    stwidth = pPrivStipple->stride >> 2;
    stippleWidth = pStipple->drawable.width;
    stippleHeight = pStipple->drawable.height;
    psrcBase = (unsigned long *) pPrivStipple->bits;

    /*
     *	The Target:
     *		Depth = PSZ
     *		Width = determined from *pwidth
     *		Words per scanline = nlwDst
     *		Pointer to pixels = addrlBase
     */

    cfbGetLongWidthAndPointer (pDrawable, nlwDst, pdstBase)

    /* this replaces rotating the stipple. Instead we just adjust the offset
     * at which we start grabbing bits from the stipple.
     * Ensure that ppt->x - xSrc >= 0 and ppt->y - ySrc >= 0,
     * so that iline and xrem always stay within the stipple bounds.
     */

    modulus (pGC->patOrg.x, stippleWidth, xSrc);
    xSrc += pDrawable->x - stippleWidth;
    modulus (pGC->patOrg.y, stippleHeight, ySrc);
    ySrc += pDrawable->y - stippleHeight;

    bitsWhole = stippleWidth;

    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);
#if 0
    if (pStipple->devKind == PIXMAP_FRAME_BUFFER)
        WAIT_READY_TO_RENDER(pStipple->drawable.pScreen);
#endif

    while (n--)
    {
	x = ppt->x;
	y = ppt->y;
	ppt++;
	w = *pwidth++;
	pdst = pdstBase + y * nlwDst + (x >> PWSH);
	y = (y - ySrc) % stippleHeight;
	srcStart = psrcBase + y * stwidth;
	xrem = ((x & ~3) - xSrc) % stippleWidth;
	srcTemp = srcStart + (xrem >> 5);
	bitsLeft = stippleWidth - (xrem & ~0x1f);
	xrem &= 0x1f;
	NextUnnaturalStippleWord
	if (partBitsLeft < xrem)
	    FatalError ("cfbUnnaturalStippleFS bad partBitsLeft %d xrem %d",
			partBitsLeft, xrem);
	NextSomeBits (inputBits, xrem);
	partBitsLeft -= xrem;
	if (((x & PIM) + w) <= PPW)
	{
	    maskpartialbits (x, w, startmask)
	    NextUnnaturalStippleBits
	    *pdst = MaskRRopPixels(*pdst,bits,startmask);
	}
	else
	{
	    maskbits (x, w, startmask, endmask, nlw);
	    nextPartBits = (x & 0x3) + w;
	    if (nextPartBits < partBitsLeft)
	    {
		if (startmask)
		{
		    MaskRRopFourBits(pdst,GetFourBits(inputBits),startmask)
		    pdst++;
		    NextFourBits (inputBits);
		}
		while (nlw--)
		{
		    RRopFourBits (pdst, GetFourBits (inputBits));
		    pdst++;
		    NextFourBits (inputBits);
		}
		if (endmask)
		{
		    MaskRRopFourBits(pdst,GetFourBits(inputBits),endmask)
		}
	    }
	    else if (bitsLeft != bitsWhole && nextPartBits < partBitsLeft + bitsLeft)
	    {
	    	NextUnnaturalStippleBitsFast
	    	if (startmask)
	    	{
		    *pdst = MaskRRopPixels(*pdst,bits,startmask);
		    pdst++;
	    	    NextUnnaturalStippleBitsFast
	    	}
	    	while (nlw--)
	    	{
		    *pdst = RRopPixels(*pdst,bits);
		    pdst++;
	    	    NextUnnaturalStippleBitsFast
	    	}
	    	if (endmask)
		    *pdst = MaskRRopPixels (*pdst,bits,endmask);
	    }
	    else
	    {
	    	NextUnnaturalStippleBits
	    	if (startmask)
	    	{
		    *pdst = MaskRRopPixels(*pdst,bits,startmask);
		    pdst++;
	    	    NextUnnaturalStippleBits
	    	}
	    	while (nlw--)
	    	{
		    *pdst = RRopPixels(*pdst,bits);
		    pdst++;
	    	    NextUnnaturalStippleBits
	    	}
	    	if (endmask)
		    *pdst = MaskRRopPixels(*pdst,bits,endmask);
	    }
	}
    }
    DEALLOCATE_LOCAL(pptFree);
    DEALLOCATE_LOCAL(pwidthFree);
}

#else

/* Fill spans with stipples that aren't 32 bits wide */
void
cfbUnnaturalStippleFS(pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
DrawablePtr pDrawable;
GC		*pGC;
int		nInit;		/* number of spans to fill */
DDXPointPtr pptInit;		/* pointer to list of start points */
int *pwidthInit;		/* pointer to list of n widths */
int fSorted;
{
				/* next three parameters are post-clip */
    int			    n;		/* number of spans to fill */
    register DDXPointPtr    ppt;	/* pointer to list of start points */
    register unsigned long  *pwidth;	/* pointer to list of n widths */
    int			    iline;	/* first line of tile to use */
    unsigned long	    *addrlBase;	/* pointer to start of bitmap */
    int			    nlwidth;	/* width in longwords of bitmap */
    register unsigned long  *pdst;	/* pointer to current word in bitmap */
    PixmapPtr		    pStipple;	/* pointer to stipple we want to fill with */
    hpPrivPixmapPtr	    pPrivStipple; /* private data of stipple */
    register int	    w;
    int			    width,  x, xrem, xSrc, ySrc;
    unsigned long	    tmpSrc, tmpDst1, tmpDst2;
    int			    stwidth, stippleWidth;
    unsigned long	    *psrcS;
    int			    rop, stiprop;
    int			    stippleHeight;
    int			    *pwidthFree;    /* copies of the pointers to free */
    DDXPointPtr		    pptFree;
    unsigned long	    fgfill, bgfill;

    if (!(pGC->planemask))
	return;

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
    rop = pGC->alu;
    if (pGC->fillStyle == FillStippled) {
	switch (rop) {
	    case GXand:
	    case GXcopy:
	    case GXnoop:
	    case GXor:
		stiprop = rop;
		break;
	    default:
		stiprop = rop;
		rop = GXcopy;
	}
    }
    fgfill = PFILL(pGC->fgPixel);
    bgfill = PFILL(pGC->bgPixel);

    /*
     *  OK,  so what's going on here?  We have two Drawables:
     *
     *  The Stipple:
     *		Depth = 1
     *		Width = stippleWidth
     *		Words per scanline = stwidth
     *		Pointer to pixels = pStipple->devPrivate.ptr
     */
    pStipple = pGC->stipple;
    pPrivStipple = (hpPrivPixmapPtr)(pStipple->devPrivate.ptr);

    stwidth = pPrivStipple->stride >> 2;
    stippleWidth = pStipple->drawable.width;
    stippleHeight = pStipple->drawable.height;

    /*
     *	The Target:
     *		Depth = PSZ
     *		Width = determined from *pwidth
     *		Words per scanline = nlwidth
     *		Pointer to pixels = addrlBase
     */

    cfbGetLongWidthAndPointer (pDrawable, nlwidth, addrlBase)

    /* this replaces rotating the stipple. Instead we just adjust the offset
     * at which we start grabbing bits from the stipple.
     * Ensure that ppt->x - xSrc >= 0 and ppt->y - ySrc >= 0,
     * so that iline and xrem always stay within the stipple bounds.
     */
    modulus (pGC->patOrg.x, stippleWidth, xSrc);
    xSrc += pDrawable->x - stippleWidth;
    modulus (pGC->patOrg.y, stippleHeight, ySrc);
    ySrc += pDrawable->y - stippleHeight;

    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);
#if 0
    if (pStipple->devKind == PIXMAP_FRAME_BUFFER)
	WAIT_READY_TO_RENDER(pStipple->drawable.pScreen);
#endif

    while (n--)
    {
	iline = (ppt->y - ySrc) % stippleHeight;
	x = ppt->x;
	pdst = addrlBase + (ppt->y * nlwidth);
        psrcS = (int *) pPrivStipple->bits + (iline * stwidth);

	if (*pwidth)
	{
	    width = *pwidth;
	    while(width > 0)
	    {
	        int xtemp, tmpx;
		register unsigned int *ptemp;
		register int *pdsttmp;
		/*
		 *  Do a stripe through the stipple & destination w pixels
		 *  wide.  w is not more than:
		 *	-	the width of the destination
		 *	-	the width of the stipple
		 *	-	the distance between x and the next word 
		 *		boundary in the destination
		 *	-	the distance between x and the next word
		 *		boundary in the stipple
		 */

		/* width of dest/stipple */
                xrem = (x - xSrc) % stippleWidth;
	        w = min((stippleWidth - xrem), width);
		/* dist to word bound in dest */
		w = min(w, PPW - (x & PIM));
		/* dist to word bound in stip */
		w = min(w, 32 - (x & 0x1f));

	        xtemp = (xrem & 0x1f);
	        ptemp = (unsigned int *)(psrcS + (xrem >> 5));
		tmpx = x & PIM;
		pdsttmp = pdst + (x>>PWSH);
		switch ( pGC->fillStyle ) {
		    case FillOpaqueStippled:
			getstipplepixels(ptemp, xtemp, w, 0, &bgfill, &tmpDst1);
			getstipplepixels(ptemp, xtemp, w, 1, &fgfill, &tmpDst2);
			break;
		    case FillStippled:
			/* Fill tmpSrc with the source pixels */
			getbits(pdsttmp, tmpx, w, tmpSrc);
			getstipplepixels(ptemp, xtemp, w, 0, &tmpSrc, &tmpDst1);
			if (rop != stiprop) {
			    putbitsrop(fgfill, 0, w, &tmpSrc, pGC->planemask, stiprop);
			} else {
			    tmpSrc = fgfill;
			}
			getstipplepixels(ptemp, xtemp, w, 1, &tmpSrc, &tmpDst2);
			break;
		}
		tmpDst2 |= tmpDst1;
		putbitsrop(tmpDst2, tmpx, w, pdsttmp, pGC->planemask, rop);
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

#endif /* PPW == 4 */

#if PPW == 4

void
cfb8Stipple32FS (pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		nInit;			/* number of spans to fill */
    DDXPointPtr pptInit;		/* pointer to list of start points */
    int		*pwidthInit;		/* pointer to list of n widths */
    int 	fSorted;
{
				/* next three parameters are post-clip */
    int		    n;			/* number of spans to fill */
    DDXPointPtr	    ppt;		/* pointer to list of start points */
    int		    *pwidth;		/* pointer to list of n widths */
    unsigned long   *src;		/* pointer to bits in stipple, if needed */
    int		    stippleHeight;	/* height of the stipple */
    PixmapPtr	    stipple;
    hpPrivPixmapPtr privStipple;

    int		    nlwDst;		/* width in longwords of the dest pixmap */
    int		    x,y,w;		/* current span */
    unsigned long   startmask;
    unsigned long   endmask;
    register unsigned long *dst;	/* pointer to bits we're writing */
    register int    nlw;
    unsigned long   *dstTmp;
    int		    nlwTmp;

    unsigned long   *pbits;		/* pointer to start of pixmap */
    register unsigned long  xor;
    register unsigned long  mask;
    register unsigned long  bits;	/* bits from stipple */
    int		    wEnd;

    int		    *pwidthFree;	/* copies of the pointers to free */
    DDXPointPtr	    pptFree;
    cfbPrivGCPtr    devPriv;

    devPriv = (cfbPrivGCPtr) pGC->devPrivates[cfbGCPrivateIndex].ptr;
    cfb8CheckStipple (pGC->alu, pGC->fgPixel, pGC->planemask);
    n = nInit * miFindMaxBand(devPriv->pCompositeClip);
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
    n = miClipSpans(devPriv->pCompositeClip,
		     pptInit, pwidthInit, nInit,
		     ppt, pwidth, fSorted);

    stipple = devPriv->pRotatedPixmap;
    privStipple = (hpPrivPixmapPtr)(stipple->devPrivate.ptr);
    src = (unsigned long *)privStipple->bits;
    stippleHeight = stipple->drawable.height;

    cfbGetLongWidthAndPointer (pDrawable, nlwDst, pbits)

    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);
#if 0
    /* stipples are depth 1 => never in frame buffer otherwise
       would need to adjust bits calc below to allow for width of 
       frame buffer */
    if (stipple->devKind == PIXMAP_FRAME_BUFFER)
	WAIT_READY_TO_RENDER(stipple->drawable.pScreen);
#endif

    while (n--)
    {
    	w = *pwidth++;
	x = ppt->x;
    	y = ppt->y;
	ppt++;
    	dst = pbits + (y * nlwDst) + (x >> PWSH);
	if (((x & PIM) + w) <= PPW)
	{
	    maskpartialbits(x, w, startmask);
	    endmask = 0;
	    nlw = 0;
	}
	else
	{
	    maskbits (x, w, startmask, endmask, nlw);
	}
	bits = src[y % stippleHeight];
	RotBitsLeft (bits, (x & (31 & ~3)));
	if (cfb8StippleRRop == GXcopy)
	{
	    xor = devPriv->xor;
	    if (w < 64)
	    {
		if (startmask)
		{
		    mask = cfb8PixelMasks[GetFourBits(bits)];
		    *dst = (*dst & ~(mask & startmask)) |
			   (xor & (mask & startmask));
		    dst++;
		    RotBitsLeft (bits, 4);
		}
		while (nlw--)
		{
		    WriteFourBits (dst,xor,GetFourBits(bits))
		    dst++;
		    RotBitsLeft (bits, 4);
		}
		if (endmask)
		{
		    mask = cfb8PixelMasks[GetFourBits(bits)];
		    *dst = (*dst & ~(mask & endmask)) |
			   (xor & (mask & endmask));
		}
	    }
	    else
	    {
		wEnd = 7 - (nlw & 7);
		nlw = (nlw >> 3) + 1;
		dstTmp = dst;
		nlwTmp = nlw;
		if (startmask)
		{
		    mask = cfb8PixelMasks[GetFourBits(bits)];
		    *dstTmp = (*dstTmp & ~(mask & startmask)) |
			   (xor & (mask & startmask));
		    dstTmp++;
		    RotBitsLeft (bits, 4);
		}
		w = 7 - wEnd;
		while (w--)
		{
		    dst = dstTmp;
		    dstTmp++;
		    nlw = nlwTmp;
#if defined(__GNUC__) && defined(mc68020)
		    mask = cfb8PixelMasks[GetFourBits(bits)];
		    xor = xor & mask;
		    mask = ~mask;
		    while (nlw--)
		    {
			*dst = (*dst & mask) | xor;
			dst += 8;
		    }
		    xor = devPriv->xor;
#else
#define SwitchBitsLoop(body) \
    while (nlw--)	\
    {		\
	body	\
	dst += 8;	\
    }
		    SwitchFourBits(dst, xor, GetFourBits(bits));
#undef SwitchBitsLoop
#endif
		    NextFourBits (bits);
		}
		nlwTmp--;
		w = wEnd + 1;
		if (endmask)
		{
		    mask = cfb8PixelMasks[GetFourBits(bits)];
		    dst = dstTmp + (nlwTmp << 3);
		    *dst = (*dst & ~(mask & endmask)) |
			   (xor &  (mask & endmask));
		}
		while (w--)
		{
		    nlw = nlwTmp;
		    dst = dstTmp;
		    dstTmp++;
#if defined(__GNUC__) && defined(mc68020)
		    mask = cfb8PixelMasks[GetFourBits(bits)];
		    xor = xor & mask;
		    mask = ~mask;
		    while (nlw--)
		    {
			*dst = (*dst & mask) | xor;
			dst += 8;
		    }
		    xor = devPriv->xor;
#else
#define SwitchBitsLoop(body) \
	while (nlw--)	\
	{		\
	    body	\
	    dst += 8;	\
	}
		    SwitchFourBits(dst, xor, GetFourBits(bits));
#undef SwitchBitsLoop
#endif
		    NextFourBits (bits);
		}
	    }
	}
	else
	{
	    if (startmask)
	    {
		xor = GetFourBits(bits);
		*dst = MaskRRopPixels(*dst, xor, startmask);
		dst++;
		RotBitsLeft (bits, 4);
	    }
	    while (nlw--)
	    {
		RRopFourBits(dst, GetFourBits(bits));
		dst++;
		RotBitsLeft (bits, 4);
	    }
	    if (endmask)
	    {
		xor = GetFourBits(bits);
		*dst = MaskRRopPixels(*dst, xor, endmask);
	    }
	}
    }
    DEALLOCATE_LOCAL(pptFree);
    DEALLOCATE_LOCAL(pwidthFree);
}

void
cfb8OpaqueStipple32FS (pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		nInit;			/* number of spans to fill */
    DDXPointPtr pptInit;		/* pointer to list of start points */
    int		*pwidthInit;		/* pointer to list of n widths */
    int 	fSorted;
{
				/* next three parameters are post-clip */
    int		    n;			/* number of spans to fill */
    DDXPointPtr	    ppt;		/* pointer to list of start points */
    int		    *pwidth;		/* pointer to list of n widths */
    unsigned long   *src;		/* pointer to bits in stipple, if needed */
    int		    stippleHeight;	/* height of the stipple */
    PixmapPtr	    stipple;
    hpPrivPixmapPtr privStipple;

    int		    nlwDst;		/* width in longwords of the dest pixmap */
    int		    x,y,w;		/* current span */
    unsigned long   startmask;
    unsigned long   endmask;
    register unsigned long *dst;	/* pointer to bits we're writing */
    register int    nlw;
    unsigned long   *dstTmp;
    int		    nlwTmp;

    unsigned long   *pbits;		/* pointer to start of pixmap */
    register unsigned long  xor;
    register unsigned long  mask;
    register unsigned long  bits;	/* bits from stipple */
    int		    wEnd;

    int		    *pwidthFree;	/* copies of the pointers to free */
    DDXPointPtr	    pptFree;
    cfbPrivGCPtr    devPriv;

    devPriv = (cfbPrivGCPtr) pGC->devPrivates[cfbGCPrivateIndex].ptr;

    cfb8CheckOpaqueStipple(pGC->alu, pGC->fgPixel, pGC->bgPixel, pGC->planemask);

    n = nInit * miFindMaxBand(devPriv->pCompositeClip);
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
    n = miClipSpans(devPriv->pCompositeClip,
		     pptInit, pwidthInit, nInit,
		     ppt, pwidth, fSorted);

    stipple = devPriv->pRotatedPixmap;
    privStipple = (hpPrivPixmapPtr)(stipple->devPrivate.ptr);
    src = (unsigned long *)privStipple->bits;
    stippleHeight = stipple->drawable.height;

    cfbGetLongWidthAndPointer (pDrawable, nlwDst, pbits)

    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);
#if 0
    /* stipples are depth 1 => never in frame buffer otherwise
       would need to adjust bits calc below to allow for width of 
       frame buffer */
    if (stipple->devKind == PIXMAP_FRAME_BUFFER)
	WAIT_READY_TO_RENDER(stipple->drawable.pScreen);
#endif

    while (n--)
    {
    	w = *pwidth++;
	x = ppt->x;
    	y = ppt->y;
	ppt++;
    	dst = pbits + (y * nlwDst) + (x >> PWSH);
	if (((x & PIM) + w) <= PPW)
	{
	    maskpartialbits(x, w, startmask);
	    endmask = 0;
	    nlw = 0;
	}
	else
	{
	    maskbits (x, w, startmask, endmask, nlw);
	}
	bits = src[y % stippleHeight];
	RotBitsLeft (bits, (x & (31 & ~3)));
	if (cfb8StippleRRop == GXcopy)
	{
	    xor = devPriv->xor;
	    if (w < 64)
	    {
		if (startmask)
		{
		    *dst = *dst & ~startmask |
			    GetFourPixels (bits) & startmask;
		    dst++;
		    RotBitsLeft (bits, 4);
		}
		while (nlw--)
		{
		    *dst++ = GetFourPixels(bits);
		    RotBitsLeft (bits, 4);
		}
		if (endmask)
		{
		    *dst = *dst & ~endmask |
			  GetFourPixels (bits) & endmask;
		}
	    }
	    else
	    {
		wEnd = 7 - (nlw & 7);
		nlw = (nlw >> 3) + 1;
		dstTmp = dst;
		nlwTmp = nlw;
		if (startmask)
		{
		    *dstTmp = *dstTmp & ~startmask |
			   GetFourPixels (bits) & startmask;
		    dstTmp++;
		    RotBitsLeft (bits, 4);
		}
		w = 7 - wEnd;
		while (w--)
		{
		    nlw = nlwTmp;
		    dst = dstTmp;
		    dstTmp++;
		    xor = GetFourPixels (bits);
		    while (nlw--)
		    {
			*dst = xor;
			dst += 8;
		    }
		    NextFourBits (bits);
		}
		nlwTmp--;
		w = wEnd + 1;
		if (endmask)
		{
		    dst = dstTmp + (nlwTmp << 3);
		    *dst = (*dst & ~endmask) |
			   GetFourPixels (bits) & endmask;
		}
		while (w--)
		{
		    nlw = nlwTmp;
		    dst = dstTmp;
		    dstTmp++;
		    xor = GetFourPixels (bits);
		    while (nlw--)
		    {
			*dst = xor;
			dst += 8;
		    }
		    NextFourBits (bits);
		}
	    }
	}
	else
	{
	    if (startmask)
	    {
		xor = GetFourBits(bits);
		*dst = MaskRRopPixels(*dst, xor, startmask);
		dst++;
		RotBitsLeft (bits, 4);
	    }
	    while (nlw--)
	    {
		RRopFourBits(dst, GetFourBits(bits));
		dst++;
		RotBitsLeft (bits, 4);
	    }
	    if (endmask)
	    {
		xor = GetFourBits(bits);
		*dst = MaskRRopPixels(*dst, xor, endmask);
	    }
	}
    }
    DEALLOCATE_LOCAL(pptFree);
    DEALLOCATE_LOCAL(pwidthFree);
}

#endif
