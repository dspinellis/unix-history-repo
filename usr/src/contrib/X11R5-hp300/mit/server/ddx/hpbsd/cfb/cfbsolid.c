/*
 * $XConsortium: cfbsolid.c,v 1.5 91/07/11 21:48:24 keith Exp $
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
#include "cfbrrop.h"

#if defined(FAST_CONSTANT_OFFSET_MODE) && (RROP != GXcopy)
# define Expand(left,right,leftAdjust) {\
    int part = nmiddle & 3; \
    int widthStep; \
    widthStep = widthDst - nmiddle - leftAdjust; \
    nmiddle >>= 2; \
    pdst = pdstRect; \
    while (h--) { \
	left \
	pdst += part; \
	switch (part) { \
	    RROP_UNROLL_CASE3(pdst) \
	} \
	m = nmiddle; \
	while (m) { \
	    pdst += 4; \
	    RROP_UNROLL_LOOP4(pdst,-4) \
	    m--; \
	} \
	right \
	pdst += widthStep; \
    } \
}
#else
# ifdef RROP_UNROLL
#  define Expand(left,right,leftAdjust) {\
    int part = nmiddle & RROP_UNROLL_MASK; \
    int widthStep; \
    widthStep = widthDst - nmiddle - leftAdjust; \
    nmiddle >>= RROP_UNROLL_SHIFT; \
    pdst = pdstRect; \
    while (h--) { \
	left \
	pdst += part; \
	switch (part) { \
	    RROP_UNROLL_CASE(pdst) \
	} \
	m = nmiddle; \
	while (m) { \
	    pdst += RROP_UNROLL; \
	    RROP_UNROLL_LOOP(pdst) \
	    m--; \
	} \
	right \
	pdst += widthStep; \
    } \
}

# else
#  define Expand(left, right, leftAdjust) { \
    while (h--) { \
	pdst = pdstRect; \
	left \
	m = nmiddle; \
	while (m--) {\
	    RROP_SOLID(pdst); \
	    pdst++; \
	} \
	right \
	pdstRect += widthDst; \
    } \
}
# endif
#endif
	

void
RROP_NAME(cfbFillRectSolid) (pDrawable, pGC, nBox, pBox)
    DrawablePtr	    pDrawable;
    GCPtr	    pGC;
    int		    nBox;
    BoxPtr	    pBox;
{
    register int    m;
    register unsigned long   *pdst;
    RROP_DECLARE
    register unsigned long   leftMask, rightMask;
    unsigned long   *pdstBase, *pdstRect;
    int		    nmiddle;
    int		    h;
    int		    w;
    int		    widthDst;
    cfbPrivGCPtr    devPriv;

    devPriv = ((cfbPrivGCPtr) (pGC->devPrivates[cfbGCPrivateIndex].ptr));

    cfbGetLongWidthAndPointer (pDrawable, widthDst, pdstBase)
    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);

    RROP_FETCH_GC(pGC)
    
    for (; nBox; nBox--, pBox++)
    {
    	pdstRect = pdstBase + pBox->y1 * widthDst;
    	h = pBox->y2 - pBox->y1;
	w = pBox->x2 - pBox->x1;
#if PPW == 4
	if (w == 1)
	{
	    register char    *pdstb = ((char *) pdstRect) + pBox->x1;
	    int	    incr = widthDst << 2;

	    while (h--)
	    {
		RROP_SOLID (pdstb);
		pdstb += incr;
	    }
	}
	else
	{
#endif
	pdstRect += (pBox->x1 >> PWSH);
	if ((pBox->x1 & PIM) + w <= PPW)
	{
	    maskpartialbits(pBox->x1, w, leftMask);
	    pdst = pdstRect;
	    while (h--) {
		RROP_SOLID_MASK (pdst, leftMask);
		pdst += widthDst;
	    }
	}
	else
	{
	    maskbits (pBox->x1, w, leftMask, rightMask, nmiddle);
	    if (leftMask)
	    {
		if (rightMask)	/* left mask and right mask */
		{
		    Expand(RROP_SOLID_MASK (pdst, leftMask); pdst++;,
			   RROP_SOLID_MASK (pdst, rightMask);, 1)
		}
		else	/* left mask and no right mask */
		{
		    Expand(RROP_SOLID_MASK (pdst, leftMask); pdst++;,
			   ;, 1)
		}
	    }
	    else
	    {
		if (rightMask)	/* no left mask and right mask */
		{
		    Expand(;,
			   RROP_SOLID_MASK (pdst, rightMask);, 0)
		}
		else	/* no left mask and no right mask */
		{
		    Expand(;,
			    ;, 0)
		}
	    }
	}
#if PPW == 4
	}
#endif
    }
}

void
RROP_NAME(cfbSolidSpans) (pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		nInit;			/* number of spans to fill */
    DDXPointPtr pptInit;		/* pointer to list of start points */
    int		*pwidthInit;		/* pointer to list of n widths */
    int 	fSorted;
{
    unsigned long   *pdstBase;
    int		    widthDst;

    RROP_DECLARE
    
    register unsigned long  *pdst;
    register int	    nlmiddle;
    register unsigned long  startmask, endmask;
    register int	    w;
    int			    x;
    
				/* next three parameters are post-clip */
    int		    n;		/* number of spans to fill */
    DDXPointPtr	    ppt;	/* pointer to list of start points */
    int		    *pwidthFree;/* copies of the pointers to free */
    DDXPointPtr	    pptFree;
    int		    *pwidth;
    cfbPrivGCPtr    devPriv;

    devPriv = (cfbPrivGCPtr)pGC->devPrivates[cfbGCPrivateIndex].ptr;
    RROP_FETCH_GCPRIV(devPriv)
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

    cfbGetLongWidthAndPointer (pDrawable, widthDst, pdstBase)
    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);

    while (n--)
    {
	x = ppt->x;
	pdst = pdstBase + (ppt->y * widthDst);
	++ppt;
	w = *pwidth++;
	if (!w)
	    continue;
#if PPW == 4
	if (w <= 4)
	{
	    register char   *addrb;

	    addrb = ((char *) pdst) + x;
	    while (w--)
	    {
		RROP_SOLID (addrb);
		addrb++;
	    }
	}
#else
	if ((x & PIM) + w <= PPW)
	{
	    pdst += x >> PWSH;
	    maskpartialbits (x, w, startmask);
	    RROP_SOLID_MASK (pdst, startmask);
	}
#endif
	else
	{
	    pdst += x >> PWSH;
	    maskbits (x, w, startmask, endmask, nlmiddle);
	    if (startmask)
	    {
		RROP_SOLID_MASK (pdst, startmask);
		++pdst;
	    }
	    
	    RROP_SPAN(pdst,nlmiddle)
	    if (endmask)
	    {
		RROP_SOLID_MASK (pdst, endmask);
	    }
	}
    }
    DEALLOCATE_LOCAL(pptFree);
    DEALLOCATE_LOCAL(pwidthFree);
}
