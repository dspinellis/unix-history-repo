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
/* $XConsortium: mfbsetsp.c,v 5.3 89/09/13 18:58:28 rws Exp $ */

#include "X.h"
#include "Xmd.h"

#include "misc.h"
#include "regionstr.h"
#include "gcstruct.h"
#include "windowstr.h"
#include "pixmapstr.h"
#include "scrnintstr.h"

#include "mfb.h"
#include "maskbits.h"

#include "servermd.h"


/* mfbSetScanline -- copies the bits from psrc to the drawable starting at
 * (xStart, y) and continuing to (xEnd, y).  xOrigin tells us where psrc 
 * starts on the scanline. (I.e., if this scanline passes through multiple
 * boxes, we may not want to start grabbing bits at psrc but at some offset
 * further on.) 
 */
mfbSetScanline(y, xOrigin, xStart, xEnd, psrc, alu, pdstBase, widthDst)
    int			y;
    int			xOrigin;	/* where this scanline starts */
    int			xStart;		/* first bit to use from scanline */
    int			xEnd;		/* last bit to use from scanline + 1 */
    register int	*psrc;
    register int	alu;		/* raster op */
    int			*pdstBase;	/* start of the drawable */
    int			widthDst;	/* width of drawable in words */
{
    int			w;		/* width of scanline in bits */
    register int	*pdst;		/* where to put the bits */
    register int	tmpSrc;		/* scratch buffer to collect bits in */
    int			dstBit;		/* offset in bits from beginning of 
					 * word */
    register int	nstart; 	/* number of bits from first partial */
    register int	nend; 		/* " " last partial word */
    int		offSrc;
    int		startmask, endmask, nlMiddle, nl;

    pdst = pdstBase + (y * widthDst) + (xStart >> 5); 
    psrc += (xStart - xOrigin) >> 5;
    offSrc = (xStart - xOrigin) & 0x1f;
    w = xEnd - xStart;
    dstBit = xStart & 0x1f;

    if (dstBit + w <= 32) 
    { 
	getandputrop(psrc, offSrc, dstBit, w, pdst, alu)
    } 
    else 
    { 

	maskbits(xStart, w, startmask, endmask, nlMiddle);
	if (startmask) 
	    nstart = 32 - dstBit; 
	else 
	    nstart = 0; 
	if (endmask) 
	    nend = xEnd & 0x1f; 
	else 
	    nend = 0; 
	if (startmask) 
	{ 
	    getandputrop(psrc, offSrc, dstBit, nstart, pdst, alu)
	    pdst++; 
	    offSrc += nstart;
	    if (offSrc > 31)
	    {
		psrc++;
		offSrc -= 32;
	    }
	} 
	nl = nlMiddle; 
	while (nl--) 
	{ 
	    getbits(psrc, offSrc, 32, tmpSrc);
	    DoRop(*pdst, alu, tmpSrc, *pdst); 
	    pdst++; 
	    psrc++; 
	} 
	if (endmask) 
	{ 
	    getandputrop0(psrc, offSrc, nend, pdst, alu);
	} 
	 
    } 
}



/* SetSpans -- for each span copy pwidth[i] bits from psrc to pDrawable at
 * ppt[i] using the raster op from the GC.  If fSorted is TRUE, the scanlines
 * are in increasing Y order.
 * Source bit lines are server scanline padded so that they always begin
 * on a word boundary.
 */ 
void
mfbSetSpans(pDrawable, pGC, psrc, ppt, pwidth, nspans, fSorted)
    DrawablePtr		pDrawable;
    GCPtr		pGC;
    int			*psrc;
    register DDXPointPtr ppt;
    int			*pwidth;
    int			nspans;
    int			fSorted;
{
    int 		*pdstBase;	/* start of dst bitmap */
    int 		widthDst;	/* width of bitmap in words */
    register BoxPtr 	pbox, pboxLast, pboxTest;
    register DDXPointPtr pptLast;
    int 		alu;
    RegionPtr 		prgnDst;
    int			xStart, xEnd;
    int			yMax;

    alu = pGC->alu;
    prgnDst = ((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip;

    pptLast = ppt + nspans;

    yMax = pDrawable->y + (int) pDrawable->height;
    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	pdstBase = (int *) getPrivScreenPtr(pDrawable->pScreen)->bits;
	widthDst = (int) getPrivScreenPtr(pDrawable->pScreen)->stride >> 2;
    }
    else
    {
	pdstBase = (int *) getPrivPixmapPtr(pDrawable)->bits;
	widthDst = (int) getPrivPixmapPtr(pDrawable)->stride >> 2;
    }

    pbox =  REGION_RECTS(prgnDst);
    pboxLast = pbox + REGION_NUM_RECTS(prgnDst);

    if(fSorted)
    {
    /* scan lines sorted in ascending order. Because they are sorted, we
     * don't have to check each scanline against each clip box.  We can be
     * sure that this scanline only has to be clipped to boxes at or after the
     * beginning of this y-band 
     */
	pboxTest = pbox;
	while(ppt < pptLast)
	{
	    pbox = pboxTest;
	    if(ppt->y >= yMax)
		break;
	    while(pbox < pboxLast)
	    {
		if(pbox->y1 > ppt->y)
		{
		    /* scanline is before clip box */
		    break;
		}
		else if(pbox->y2 <= ppt->y)
		{
		    /* clip box is before scanline */
		    pboxTest = ++pbox;
		    continue;
		}
		else if(pbox->x1 > ppt->x + *pwidth) 
		{
		    /* clip box is to right of scanline */
		    break;
		}
		else if(pbox->x2 <= ppt->x)
		{
		    /* scanline is to right of clip box */
		    pbox++;
		    continue;
		}

		/* at least some of the scanline is in the current clip box */
		xStart = max(pbox->x1, ppt->x);
		xEnd = min(ppt->x + *pwidth, pbox->x2);
		mfbSetScanline(ppt->y, ppt->x, xStart, xEnd, psrc, alu,
			       pdstBase, widthDst);
		if(ppt->x + *pwidth <= pbox->x2)
		{
		    /* End of the line, as it were */
		    break;
		}
		else
		    pbox++;
	    }
	    /* We've tried this line against every box; it must be outside them
	     * all.  move on to the next point */
	    ppt++;
	    psrc += PixmapWidthInPadUnits(*pwidth, 1);
	    pwidth++;
	}
    }
    else
    {
    /* scan lines not sorted. We must clip each line against all the boxes */
	while(ppt < pptLast)
	{
	    if(ppt->y >= 0 && ppt->y < yMax)
	    {
		
		for(pbox = REGION_RECTS(prgnDst); pbox< pboxLast; pbox++)
		{
		    if(pbox->y1 > ppt->y)
		    {
			/* rest of clip region is above this scanline,
			 * skip it */
			break;
		    }
		    if(pbox->y2 <= ppt->y)
		    {
			/* clip box is below scanline */
			pbox++;
			break;
		    }
		    if(pbox->x1 <= ppt->x + *pwidth &&
		       pbox->x2 > ppt->x)
		    {
			xStart = max(pbox->x1, ppt->x);
			xEnd = min(pbox->x2, ppt->x + *pwidth);
			mfbSetScanline(ppt->y, ppt->x, xStart, xEnd, 
				       psrc, alu, pdstBase, widthDst);
		    }

		}
	    }
	psrc += PixmapWidthInPadUnits(*pwidth, 1);
	ppt++;
	pwidth++;
	}
    }
}

