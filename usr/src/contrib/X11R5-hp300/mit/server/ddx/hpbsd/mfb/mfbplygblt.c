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
/* $XConsortium: mfbplygblt.c,v 5.4 91/01/27 13:02:11 keith Exp $ */

#include "X.h"
#include "Xmd.h"
#include "Xproto.h"
#include "mfb.h"
#include "fontstruct.h"
#include "dixfontstr.h"
#include "gcstruct.h"
#include "windowstr.h"
#include "pixmapstr.h"
#include "scrnintstr.h"
#include "regionstr.h"
#include "maskbits.h"
#include "miscstruct.h"

extern void QueryGlyphExtents();

/*
    we should eventually special-case fixed-width fonts, although
its more important for ImageText, which is meant for terminal
emulators.

    this works for fonts with glyphs <= 32 bits wide.

    the clipping calculations are done for worst-case fonts.
we make no assumptions about the heights, widths, or bearings
of the glyphs.  if we knew that the glyphs are all the same height,
we could clip the tops and bottoms per clipping box, rather
than per character per clipping box.  if we knew that the glyphs'
left and right bearings were well-behaved, we could clip a single
character at the start, output until the last unclipped
character, and then clip the last one.  this is all straightforward
to determine based on max-bounds and min-bounds from the font.
    there is some inefficiency introduced in the per-character
clipping to make what's going on clearer.

    (it is possible, for example, for a font to be defined in which the
next-to-last character in a font would be clipped out, but the last
one wouldn't.  the code below deals with this.)

    PolyText looks at the fg color and the rasterop; mfbValidateGC
swaps in the right routine after looking at the reduced ratserop
in the private field of the GC.  

   the register allocations are provisional; in particualr startmask and
endmask might not be the right things.  pglyph, xoff, pdst, and tmpSrc
are fairly obvious, though.

   to avoid source proliferation, this file is compiled
three times:
	MFBPOLYGLYPHBLT		OPEQ
	mfbPolyGlyphBltWhite	|=
	mfbPolyGlyphBltBlack	&=~
	mfbPolyGlyphBltInvert	^=
*/

void
MFBPOLYGLYPHBLT(pDrawable, pGC, x, y, nglyph, ppci, pglyphBase)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int 	x, y;
    unsigned int nglyph;
    CharInfoPtr *ppci;		/* array of character info */
    unsigned char *pglyphBase;	/* start of array of glyphs (unused in R5) */
{
    ExtentInfoRec info;	/* used by QueryGlyphExtents() */
    BoxRec bbox;		/* string's bounding box */

    CharInfoPtr pci;
    int xorg, yorg;	/* origin of drawable in bitmap */
    int widthDst;	/* width of dst in longwords */

			/* these keep track of the character origin */
    unsigned int *pdstBase;
			/* points to longword with character origin */
    int xchar;		/* xorigin of char (mod 32) */

			/* these are used for placing the glyph */
    register int xoff;	/* x offset of left edge of glyph (mod 32) */
    register unsigned int *pdst;
			/* pointer to current longword in dst */

    int w;		/* width of glyph in bits */
    int h;		/* height of glyph */
    int widthGlyph;	/* width of glyph, in bytes */
    register unsigned char *pglyph;
			/* pointer to current row of glyph */

			/* used for putting down glyph */
    register unsigned int tmpSrc;
			/* for getting bits from glyph */
    register int startmask;
    register int endmask;
    register int nFirst;/* bits of glyph in current longword */

    if (!(pGC->planemask & 1))
	return;

    xorg = pDrawable->x;
    yorg = pDrawable->y;
    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	pdstBase = (unsigned int *) getPrivScreenPtr(pDrawable->pScreen)->bits;
	widthDst = (int) getPrivScreenPtr(pDrawable->pScreen)->stride >> 2;
    }
    else
    {
	pdstBase = (unsigned int *) getPrivPixmapPtr(pDrawable)->bits;
	widthDst = (int) getPrivPixmapPtr(pDrawable)->stride >> 2;
    }

    x += xorg;
    y += yorg;

    QueryGlyphExtents(pGC->font, ppci, (unsigned long)nglyph, &info);
    bbox.x1 = x + info.overallLeft;
    bbox.x2 = x + info.overallRight;
    bbox.y1 = y - info.overallAscent;
    bbox.y2 = y + info.overallDescent;

    switch ((*pGC->pScreen->RectIn)(
                ((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip, &bbox))
    {
      case rgnOUT:
	break;
      case rgnIN:
        pdstBase = pdstBase + (widthDst * y) + (x >> 5);
        xchar = x & 0x1f;

        while(nglyph--)
        {
	    pci = *ppci;
	    pglyph = FONTGLYPHBITS(pglyphBase, pci);
	    w = pci->metrics.rightSideBearing - pci->metrics.leftSideBearing;
	    h = pci->metrics.ascent + pci->metrics.descent;
	    widthGlyph = GLYPHWIDTHBYTESPADDED(pci);

	    /* start at top scanline of glyph */
	    pdst = pdstBase - (pci->metrics.ascent * widthDst);

	    /* find correct word in scanline and x offset within it
	       for left edge of glyph
	    */
	    xoff = xchar + pci->metrics.leftSideBearing;
	    if (xoff > 31)
	    {
	        pdst++;
	        xoff &= 0x1f;
	    }
	    else if (xoff < 0)
	    {
	        xoff += 32;
	        pdst--;
	    }

	    if ((xoff + w) <= 32)
	    {
	        /* glyph all in one longword */
	        maskpartialbits(xoff, w, startmask);
	        while (h--)
	        {
		    getleftbits(pglyph, w, tmpSrc);
		    *pdst OPEQ (SCRRIGHT(tmpSrc, xoff) & startmask);
		    pglyph += widthGlyph;
		    pdst += widthDst;
	        }
	    }
	    else
	    {
	        /* glyph crosses longword boundary */
	        mask32bits(xoff, w, startmask, endmask);
	        nFirst = 32 - xoff;
	        while (h--)
	        {
		    getleftbits(pglyph, w, tmpSrc);
		    *pdst OPEQ (SCRRIGHT(tmpSrc, xoff) & startmask);
		    *(pdst+1) OPEQ (SCRLEFT(tmpSrc, nFirst) & endmask);
		    pglyph += widthGlyph;
		    pdst += widthDst;
	        }
	    } /* glyph crosses longwords boundary */

	    /* update character origin */
	    x += pci->metrics.characterWidth;
	    xchar += pci->metrics.characterWidth;
	    if (xchar > 31)
	    {
	        xchar -= 32;
	        pdstBase++;
	    }
	    else if (xchar < 0)
	    {
	        xchar += 32;
	        pdstBase--;
	    }
	    ppci++;
        } /* while nglyph-- */
	break;
      case rgnPART:
      {
	TEXTPOS *ppos;
	RegionPtr cclip;
	int nbox;
	BoxPtr pbox;
	int xpos;		/* x position of char origin */
	int i;
	BoxRec clip;
	int leftEdge, rightEdge;
	int topEdge, bottomEdge;
	int glyphRow;		/* first row of glyph not wholly
				   clipped out */
	int glyphCol;		/* leftmost visible column of glyph */
	int getWidth;		/* bits to get from glyph */

	if(!(ppos = (TEXTPOS *)ALLOCATE_LOCAL(nglyph * sizeof(TEXTPOS))))
	    return;

        pdstBase = pdstBase + (widthDst * y) + (x >> 5);
        xpos = x;
	xchar = xpos & 0x1f;

	for (i=0; i<nglyph; i++)
	{
	    pci = ppci[i];

	    ppos[i].xpos = xpos;
	    ppos[i].xchar = xchar;
	    ppos[i].leftEdge = xpos + pci->metrics.leftSideBearing;
	    ppos[i].rightEdge = xpos + pci->metrics.rightSideBearing;
	    ppos[i].topEdge = y - pci->metrics.ascent;
	    ppos[i].bottomEdge = y + pci->metrics.descent;
	    ppos[i].pdstBase = pdstBase;
	    ppos[i].widthGlyph = GLYPHWIDTHBYTESPADDED(pci);

	    xpos += pci->metrics.characterWidth;
	    xchar += pci->metrics.characterWidth;
	    if (xchar > 31)
	    {
		xchar &= 0x1f;
		pdstBase++;
	    }
	    else if (xchar < 0)
	    {
		xchar += 32;
		pdstBase--;
	    }
	}

	cclip = ((mfbPrivGC *)(pGC->devPrivates[mfbGCPrivateIndex].ptr))->pCompositeClip;
	pbox = REGION_RECTS(cclip);
	nbox = REGION_NUM_RECTS(cclip);

	for (; --nbox >= 0; pbox++)
	{
	    clip.x1 = max(bbox.x1, pbox->x1);
	    clip.y1 = max(bbox.y1, pbox->y1);
	    clip.x2 = min(bbox.x2, pbox->x2);
	    clip.y2 = min(bbox.y2, pbox->y2);
	    if ((clip.x2<=clip.x1) || (clip.y2<=clip.y1))
		continue;

	    for(i=0; i<nglyph; i++)
	    {
		pci = ppci[i];
		xchar = ppos[i].xchar;

		/* clip the left and right edges */
		if (ppos[i].leftEdge < clip.x1)
		    leftEdge = clip.x1;
		else
		    leftEdge = ppos[i].leftEdge;

		if (ppos[i].rightEdge > clip.x2)
		    rightEdge = clip.x2;
		else
		    rightEdge = ppos[i].rightEdge;

		w = rightEdge - leftEdge;
		if (w <= 0)
		    continue;

		/* clip the top and bottom edges */
		if (ppos[i].topEdge < clip.y1)
		    topEdge = clip.y1;
		else
		    topEdge = ppos[i].topEdge;

		if (ppos[i].bottomEdge > clip.y2)
		    bottomEdge = clip.y2;
		else
		    bottomEdge = ppos[i].bottomEdge;

		h = bottomEdge - topEdge;
		if (h <= 0)
		    continue;

		glyphRow = (topEdge - y) + pci->metrics.ascent;
		widthGlyph = ppos[i].widthGlyph;
		pglyph = FONTGLYPHBITS(pglyphBase, pci);
		pglyph += (glyphRow * widthGlyph);

		pdst = ppos[i].pdstBase - ((y-topEdge) * widthDst);

		glyphCol = (leftEdge - ppos[i].xpos) -
			   (pci->metrics.leftSideBearing);
		getWidth = w + glyphCol;
		xoff = xchar + (leftEdge - ppos[i].xpos);
		if (xoff > 31)
		{
		    xoff &= 0x1f;
		    pdst++;
		}
		else if (xoff < 0)
		{
		    xoff += 32;
		    pdst--;
		}

		if ((xoff + w) <= 32)
		{
		    maskpartialbits(xoff, w, startmask);
		    while (h--)
		    {
			getshiftedleftbits(pglyph, glyphCol, getWidth, tmpSrc);
			*pdst OPEQ (SCRRIGHT(tmpSrc, xoff) & startmask);
			pglyph += widthGlyph;
			pdst += widthDst;
		    }
		}
		else
		{
		    mask32bits(xoff, w, startmask, endmask);
		    nFirst = 32 - xoff;
		    while (h--)
		    {
			getshiftedleftbits(pglyph, glyphCol, getWidth, tmpSrc);
			*pdst OPEQ (SCRRIGHT(tmpSrc, xoff) & startmask);
			*(pdst+1) OPEQ (SCRLEFT(tmpSrc, nFirst) & endmask);
			pglyph += widthGlyph;
			pdst += widthDst;
		    }
		}
	    } /* for each glyph */
	} /* while nbox-- */
	DEALLOCATE_LOCAL(ppos);
	break;
      }
      default:
	break;
    }
}


