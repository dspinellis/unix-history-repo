/*
 * $XConsortium: cfb8cppl.c,v 1.5 91/08/22 15:41:05 keith Exp $
 *
 * Copyright 1990 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

/* 
 * this is actually an mfb-specific function, except that
 * it knows how to read from 8-bit cfb pixmaps.  Alas, this
 * means that it doesn't know PPW so it is always compiled
 */

#include "X.h"
#include "Xmd.h"
#include "gcstruct.h"
#include "window.h"
#include "pixmapstr.h"
#include "scrnintstr.h"
#include "windowstr.h"
#include "cfb.h"
#include "maskbits.h"

#include "mergerop.h"

#if BITMAP_BIT_ORDER == MSBFirst
#define LeftMost    31
#define StepBit(bit, inc)  ((bit) -= (inc))
#else
#define LeftMost    0
#define StepBit(bit, inc)  ((bit) += (inc))
#endif

#define GetBits(psrc, nBits, curBit, bitPos, bits) {\
    bits = 0; \
    while (nBits--) \
    { \
	bits |= ((*psrc++ >> bitPos) & 1) << curBit; \
	StepBit (curBit, 1); \
    } \
}

cfbCopyImagePlane (pSrcDrawable, pDstDrawable, rop, prgnDst, pptSrc, planemask)
    DrawablePtr pSrcDrawable;
    DrawablePtr pDstDrawable;
    int	rop;
    unsigned long planemask;
    RegionPtr prgnDst;
    DDXPointPtr pptSrc;
{
    cfbCopyPlane8to1 (pSrcDrawable, pDstDrawable, rop, prgnDst, pptSrc,
		      (unsigned long) ~0L, planemask);
}

cfbCopyPlane8to1 (pSrcDrawable, pDstDrawable, rop, prgnDst, pptSrc, planemask, bitPlane)
    DrawablePtr pSrcDrawable;
    DrawablePtr pDstDrawable;
    int	rop;
    RegionPtr prgnDst;
    DDXPointPtr pptSrc;
    unsigned long planemask;
    unsigned long   bitPlane;
{
    int			    srcx, srcy, dstx, dsty, width, height;
    unsigned char	    *psrcBase;
    unsigned long	    *pdstBase;
    int			    widthSrc, widthDst;
    unsigned char	    *psrcLine;
    unsigned long	    *pdstLine;
    register unsigned char  *psrc;
    register int	    i;
    register int	    curBit;
    register int	    bitPos;
    register unsigned long  bits;
    register unsigned long  *pdst;
    unsigned long	    startmask, endmask;
    int			    niStart, niEnd;
    int			    bitStart, bitEnd;
    int			    nl, nlMiddle;
    int			    nbox;
    BoxPtr		    pbox;
    MROP_DECLARE()

    if (!(planemask & 1))
	return;

    if (rop != GXcopy)
	MROP_INITIALIZE (rop, planemask);

    cfbGetByteWidthAndPointer (pSrcDrawable, widthSrc, psrcBase)

    mfbGetLongWidthAndPointer (pDstDrawable, widthDst, pdstBase)

    bitPos = ffs (bitPlane) - 1;

    nbox = REGION_NUM_RECTS(prgnDst);
    pbox = REGION_RECTS(prgnDst);

    /* XXX ??? */
    WAIT_READY_TO_RENDER(pSrcDrawable->pScreen);
    SET_REGISTERS_FOR_WRITING(pDstDrawable->pScreen, ~0, GXcopy);

    while (nbox--)
    {
	dstx = pbox->x1;
	dsty = pbox->y1;
	srcx = pptSrc->x;
	srcy = pptSrc->y;
	width = pbox->x2 - pbox->x1;
	height = pbox->y2 - pbox->y1;
	pbox++;
	pptSrc++;
	psrcLine = psrcBase + srcy * widthSrc + srcx;
	pdstLine = pdstBase + dsty * widthDst + (dstx >> 5);
	dstx &= 0x1f;
	if (dstx + width <= 32)
	{
	    maskpartialbits(dstx, width, startmask);
	    nlMiddle = 0;
	    endmask = 0;
	}
	else
	{
	    maskbits (dstx, width, startmask, endmask, nlMiddle);
	}
	if (startmask)
	{
	    niStart = 32 - dstx;
	    bitStart = LeftMost;
	    StepBit (bitStart, dstx);
	}
	if (endmask)
	{
	    niEnd = (dstx + width) & 0x1f;
	    bitEnd = LeftMost;
	}
	if (rop == GXcopy)
	{
	    while (height--)
	    {
	    	psrc = psrcLine;
	    	pdst = pdstLine;
	    	psrcLine += widthSrc;
	    	pdstLine += widthDst;
	    	if (startmask)
	    	{
		    i = niStart;
		    curBit = bitStart;
		    GetBits (psrc, i, curBit, bitPos, bits);
		    *pdst = *pdst & ~startmask | bits;
		    pdst++;
	    	}
	    	nl = nlMiddle;
	    	while (nl--)
	    	{
		    i = 32;
		    curBit = LeftMost;
		    GetBits (psrc, i, curBit, bitPos, bits);
		    *pdst++ = bits;
	    	}
	    	if (endmask)
	    	{
		    i = niEnd;
		    curBit = bitEnd;
		    GetBits (psrc, i, curBit, bitPos, bits);
		    *pdst = *pdst & ~endmask | bits;
	    	}
	    }
	}
	else
	{
	    while (height--)
	    {
	    	psrc = psrcLine;
	    	pdst = pdstLine;
	    	psrcLine += widthSrc;
	    	pdstLine += widthDst;
	    	if (startmask)
	    	{
		    i = niStart;
		    curBit = bitStart;
		    GetBits (psrc, i, curBit, bitPos, bits);
		    *pdst = MROP_MASK(bits, *pdst, startmask);
		    pdst++;
	    	}
	    	nl = nlMiddle;
	    	while (nl--)
	    	{
		    i = 32;
		    curBit = LeftMost;
		    GetBits (psrc, i, curBit, bitPos, bits);
		    *pdst = MROP_SOLID(bits, *pdst);
		    pdst++;
	    	}
	    	if (endmask)
	    	{
		    i = niEnd;
		    curBit = bitEnd;
		    GetBits (psrc, i, curBit, bitPos, bits);
		    *pdst = MROP_MASK (bits, *pdst, endmask);
	    	}
	    }
	}
    }
}
