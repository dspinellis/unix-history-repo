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

#include "X.h"
#include "Xmd.h"
#include "servermd.h"

#include "misc.h"
#include "region.h"
#include "gc.h"
#include "windowstr.h"
#include "pixmapstr.h"
#include "scrnintstr.h"

#include "cfb.h"
#include "cfbmskbits.h"

/* GetSpans -- for each span, gets bits from drawable starting at ppt[i]
 * and continuing for pwidth[i] bits
 * Each scanline returned will be server scanline padded, i.e., it will come
 * out to an integral number of words.
 */
void
cfbGetSpans(pDrawable, wMax, ppt, pwidth, nspans, pdstStart)
    DrawablePtr		pDrawable;	/* drawable from which to get bits */
    int			wMax;		/* largest value of all *pwidths */
    register DDXPointPtr ppt;		/* points to start copying from */
    int			*pwidth;	/* list of number of bits to copy */
    int			nspans;		/* number of scanlines to copy */
    unsigned long	*pdstStart;	/* where to put the bits */
{
    register unsigned long	*pdst;		/* where to put the bits */
    register unsigned long	*psrc;		/* where to get the bits */
    register unsigned long	tmpSrc;		/* scratch buffer for bits */
    unsigned long		*psrcBase;	/* start of src bitmap */
    int			widthSrc;	/* width of pixmap in bytes */
    register DDXPointPtr pptLast;	/* one past last point to get */
    int         	xEnd;		/* last pixel to copy from */
    register int	nstart; 
    int	 		nend; 
    int	 		srcStartOver; 
    unsigned long	startmask, endmask;
    int			nlMiddle, nl, srcBit;
    int			w;
    unsigned long	*pdstNext;

    switch (pDrawable->bitsPerPixel) {
	case 1:
	    mfbGetSpans(pDrawable, wMax, ppt, pwidth, nspans, pdstStart);
	    return;
	case PSZ:
	    break;
	default:
	    FatalError("cfbGetSpans: invalid depth\n");
    }

    
    cfbGetLongWidthAndPointer (pDrawable, widthSrc, psrcBase)

    WAIT_READY_TO_RENDER(pDrawable->pScreen);

#if PPW == 4
    if ((nspans == 1) && (*pwidth == 1))
    {
	tmpSrc = *((unsigned char *)(psrcBase + (ppt->y * (widthSrc >> 2)))
		   + ppt->x);
#if BITMAP_BIT_ORDER == MSBFirst
	tmpSrc <<= 24;
#endif
	*pdstStart = tmpSrc;
	return;
    }
#endif
    pdst = pdstStart;
    pptLast = ppt + nspans;
    while(ppt < pptLast)
    {
	xEnd = min(ppt->x + *pwidth, widthSrc << (PWSH-2) );
	psrc = psrcBase + (ppt->y * (widthSrc >> 2)) + (ppt->x >> PWSH); 
	w = xEnd - ppt->x;
	srcBit = ppt->x & PIM;
	/* This shouldn't be needed */
	pdstNext = pdst + PixmapWidthInPadUnits(w, PSZ);

	if (srcBit + w <= PPW) 
	{ 
	    getbits(psrc, srcBit, w, tmpSrc);
/*XXX*/	    putbits(tmpSrc, 0, w, pdst, ~((unsigned long)0)); 
	    pdst++;
	} 
	else 
	{ 

	    maskbits(ppt->x, w, startmask, endmask, nlMiddle);
	    if (startmask) 
		nstart = PPW - srcBit; 
	    else 
		nstart = 0; 
	    if (endmask) 
		nend = xEnd & PIM; 
	    srcStartOver = srcBit + nstart > PLST;
	    if (startmask) 
	    { 
		getbits(psrc, srcBit, nstart, tmpSrc);
/*XXX*/		putbits(tmpSrc, 0, nstart, pdst, ~((unsigned long)0));
		if(srcStartOver)
		    psrc++;
	    } 
	    nl = nlMiddle; 
	    while (nl--) 
	    { 
		tmpSrc = *psrc;
/*XXX*/		putbits(tmpSrc, nstart, PPW, pdst, ~((unsigned long)0));
		psrc++;
		pdst++;
	    } 
	    if (endmask) 
	    { 
		getbits(psrc, 0, nend, tmpSrc);
/*XXX*/		putbits(tmpSrc, nstart, nend, pdst, ~((unsigned long)0));
		if(nstart + nend >= PPW)
		    pdst++;
	    } 
#ifdef	notdef
	    pdst++; 
	    while(pdst < pdstNext)
	    {
		*pdst++ = 0;
	    }
#else
	    pdst = pdstNext;
#endif
	} 
        ppt++;
	pwidth++;
    }
}
