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
/* $XConsortium: mfbpushpxl.c,v 5.2 89/09/14 16:26:57 rws Exp $ */

#include "X.h"
#include "gcstruct.h"
#include "scrnintstr.h"
#include "pixmapstr.h"
#include "miscstruct.h"
#include "maskbits.h"
#include "regionstr.h"
#include "mfb.h"

/*  mfbSolidPP is courtesy of xhacks@csri.toronto.edu

    For fillStyle==FillSolid, a monochrome PushPixels can be reduced to
    a ROP in the following way:  (Note that the ROP is the same as the
    result of ROP(src=0x3,dst=0x5))

			src=0011 0000 0011
			dst=0101 0101 0101
			rop      fg=0 fg=1
	GXclear         0x0 0000 0100 0100 0
	GXand           0x1 0001 0100 0101  s&d
	GXandReverse    0x2 0010 0100 0110 s&~d
	GXcopy          0x3 0011 0100 0111 s
	GXandInverted   0x4 0100 0101 0100 ~s&d
	GXnoop          0x5 0101 0101 0101 d
	GXxor           0x6 0110 0101 0110 s^d
	GXor            0x7 0111 0101 0111 s|d
	GXnor           0x8 1000 0110 0100 ~s&~d
	GXequiv         0x9 1001 0110 0101 ~s^d
	GXinvert        0xa 1010 0110 0110 ~d
	GXorReverse     0xb 1011 0110 0111 s|~d
	GXcopyInverted  0xc 1100 0111 0100 ~s
	GXorInverted    0xd 1101 0111 0101 ~s|d
	GXnand          0xe 1110 0111 0110 ~s|~d
	GXset           0xf 1111 0111 0111 1

For src=0: newRop = 0x4|(rop>>2)
For src=1: newRop = 0x4|(rop&3)
*/

/* mfbSolidPP -- squeegees the forground color of pGC through pBitMap
 * into pDrawable.  pBitMap is a stencil (dx by dy of it is used, it may
 * be bigger) which is placed on the drawable at xOrg, yOrg.  Where a 1 bit
 * is set in the bitmap, the fill style is put onto the drawable using
 * the GC's logical function. The drawable is not changed where the bitmap
 * has a zero bit or outside the area covered by the stencil.
 */
void
mfbSolidPP(pGC, pBitMap, pDrawable, dx, dy, xOrg, yOrg)
    GCPtr	pGC;
    PixmapPtr	pBitMap;
    DrawablePtr pDrawable;
    int		dx, dy, xOrg, yOrg;
{
    unsigned char alu;
    RegionRec rgnDst;
    DDXPointPtr pptSrc;
    BoxRec srcBox;
    register DDXPointPtr ppt;
    register BoxPtr pbox;
    int i;

    if (!pGC->planemask & 1) return;

    /* compute the reduced rop function */
    alu = pGC->alu;
    if (!(pGC->fgPixel&1)) alu >>= 2;
    alu = (alu & 0x3) | 0x4;
    if (alu == GXnoop) return;

    srcBox.x1 = xOrg;
    srcBox.y1 = yOrg;
    srcBox.x2 = xOrg + dx;
    srcBox.y2 = yOrg + dy;
    (*pGC->pScreen->RegionInit)(&rgnDst, &srcBox, 1);

    /* clip the shape of the dst to the destination composite clip */
    (*pGC->pScreen->Intersect)(&rgnDst, &rgnDst,
			       ((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip);

    if (!REGION_NIL(&rgnDst))
    {
	i = REGION_NUM_RECTS(&rgnDst);
	pptSrc = (DDXPointPtr)ALLOCATE_LOCAL(i * sizeof(DDXPointRec));
        if(pptSrc)
        {
	    for (pbox = REGION_RECTS(&rgnDst), ppt = pptSrc;
		 --i >= 0;
		 pbox++, ppt++)
	    {
		ppt->x = pbox->x1 - xOrg;
		ppt->y = pbox->y1 - yOrg;
	    }
	    mfbDoBitblt((DrawablePtr)pBitMap, pDrawable, alu, &rgnDst, pptSrc);
	    DEALLOCATE_LOCAL(pptSrc);
	}
    }
    (*pGC->pScreen->RegionUninit)(&rgnDst);
}

#define NPT 128

/* mfbPushPixels -- squeegees the forground color of pGC through pBitMap
 * into pDrawable.  pBitMap is a stencil (dx by dy of it is used, it may
 * be bigger) which is placed on the drawable at xOrg, yOrg.  Where a 1 bit
 * is set in the bitmap, the fill style is put onto the drawable using
 * the GC's logical function. The drawable is not changed where the bitmap
 * has a zero bit or outside the area covered by the stencil.
 */
void
mfbPushPixels(pGC, pBitMap, pDrawable, dx, dy, xOrg, yOrg)
    GCPtr	pGC;
    PixmapPtr	pBitMap;
    DrawablePtr pDrawable;
    int		dx, dy, xOrg, yOrg;
{
    int		h, dxDiv32, ibEnd;
    unsigned int *pwLineStart;
    register unsigned int	*pw, *pwEnd;
    register unsigned int mask;
    register int ib, w;
    register int ipt;		/* index into above arrays */
    hpPrivPixmapPtr pPrivBitMap;
    Bool 	fInBox;
    DDXPointRec	pt[NPT];
    int		width[NPT];

    if (pBitMap->devKind == PIXMAP_FRAME_BUFFER)
	WAIT_READY_TO_RENDER(pBitMap->drawable.pScreen);
    WAIT_READY_TO_RENDER(pDrawable->pScreen);

    pPrivBitMap = (hpPrivPixmapPtr)(pBitMap->devPrivate.ptr);
    /* Now scan convert the pixmap and use the result to call fillspans in
     * in the drawable with the original GC */
    ipt = 0;
    dxDiv32 = dx/32;
    for(h = 0; h < dy; h++)
    {

	pw = (unsigned int *)
	     (((char *)(pPrivBitMap->bits))+(h * pPrivBitMap->stride));
	pwLineStart = pw;
	/* Process all words which are fully in the pixmap */
	
	fInBox = FALSE;
	pwEnd = pwLineStart + dxDiv32;
	while(pw  < pwEnd)
	{
	    w = *pw;
	    mask = endtab[1];
	    for(ib = 0; ib < 32; ib++)
	    {
		if(w & mask)
		{
		    if(!fInBox)
		    {
			pt[ipt].x = ((pw - pwLineStart) << 5) + ib + xOrg;
			pt[ipt].y = h + yOrg;
			/* start new box */
			fInBox = TRUE;
		    }
		}
		else
		{
		    if(fInBox)
		    {
			width[ipt] = ((pw - pwLineStart) << 5) + 
				     ib + xOrg - pt[ipt].x;
			if (++ipt >= NPT)
			{
			    (*pGC->ops->FillSpans)(pDrawable, pGC, NPT, pt,
			                      width, TRUE);
			    ipt = 0;
			}
			/* end box */
			fInBox = FALSE;
		    }
		}
		mask = SCRRIGHT(mask, 1);
	    }
	    pw++;
	}
	ibEnd = dx & 0x1F;
	if(ibEnd)
	{
	    /* Process final partial word on line */
	    w = *pw;
	    mask = endtab[1];
	    for(ib = 0; ib < ibEnd; ib++)
	    {
		if(w & mask)
		{
		    if(!fInBox)
		    {
			/* start new box */
			pt[ipt].x = ((pw - pwLineStart) << 5) + ib + xOrg;
			pt[ipt].y = h + yOrg;
			fInBox = TRUE;
		    }
		}
		else
		{
		    if(fInBox)
		    {
			/* end box */
			width[ipt] = ((pw - pwLineStart) << 5) + 
				     ib + xOrg - pt[ipt].x;
			if (++ipt >= NPT)
			{
			    (*pGC->ops->FillSpans)(pDrawable, pGC, NPT, pt,
			                      width, TRUE);
			    ipt = 0;
			}
			fInBox = FALSE;
		    }
		}
		mask = SCRRIGHT(mask, 1);
	    }
	}
	/* If scanline ended with last bit set, end the box */
	if(fInBox)
	{
	    width[ipt] = dx + xOrg - pt[ipt].x;
	    if (++ipt >= NPT)
	    {
		(*pGC->ops->FillSpans)(pDrawable, pGC, NPT, pt, width, TRUE);
		ipt = 0;
	    }
	}
    }
    /* Flush any remaining spans */
    if (ipt)
    {
	(*pGC->ops->FillSpans)(pDrawable, pGC, ipt, pt, width, TRUE);
    }
}
