/*
 * Poly glyph blt for 8 bit displays.  Accepts
 * an arbitrary font <= 32 bits wide, in Copy mode only.
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
/* $XConsortium: cfbglblt8.c,v 5.23 91/07/18 23:44:50 keith Exp $ */

#include	"X.h"
#include	"Xmd.h"
#include	"Xproto.h"
#include	"cfb.h"
#include	"fontstruct.h"
#include	"dixfontstr.h"
#include	"gcstruct.h"
#include	"windowstr.h"
#include	"scrnintstr.h"
#include	"pixmapstr.h"
#include	"regionstr.h"
#include	"cfbmskbits.h"
#include	"cfb8bit.h"

#define BOX_OVERLAP(box1, box2, xoffset, yoffset) \
 	((box1)->x1 <= ((int) (box2)->x2 + (xoffset)) && \
 	 ((int) (box2)->x1 + (xoffset)) <= (box1)->x2 && \
	 (box1)->y1 <= ((int) (box2)->y2 + (yoffset)) && \
 	 ((int) (box2)->y1 + (yoffset)) <= (box1)->y2)

#define BOX_CONTAINS(box1, box2, xoffset, yoffset) \
 	((box1)->x1 <= ((int) (box2)->x1 + (xoffset)) && \
 	 ((int) (box2)->x2 + (xoffset)) <= (box1)->x2 && \
	 (box1)->y1 <= ((int) (box2)->y1 + (yoffset)) && \
 	 ((int) (box2)->y2 + (yoffset)) <= (box1)->y2)

#if PPW == 4

#if GLYPHPADBYTES != 4
#define USE_LEFTBITS
#endif

#ifdef USE_LEFTBITS
typedef	unsigned char	*glyphPointer;
extern long endtab[];

#define GlyphBits(bits,width,dst)	getleftbits(bits,width,dst); \
					(dst) &= widthMask; \
					(bits) += widthGlyph;
#define GlyphBitsS(bits,width,dst,off)	GlyphBits(bits,width,dst); \
					dst = BitRight (dst, off);
#else
typedef unsigned long	*glyphPointer;

#define GlyphBits(bits,width,dst)	dst = *bits++;
#define GlyphBitsS(bits,width,dst,off)	dst = BitRight(*bits++, off);
#endif

#ifdef GLYPHROP
#define cfbPolyGlyphBlt8	cfbPolyGlyphRop8
#define cfbPolyGlyphBlt8Clipped	cfbPolyGlyphRop8Clipped

#undef WriteFourBits
#define WriteFourBits(dst,pixel,bits)	RRopFourBits(dst,bits)

#endif

static void cfbPolyGlyphBlt8Clipped();

#if defined(HAS_STIPPLE_CODE) && !defined(GLYPHROP) && !defined(USE_LEFTBITS)
#define USE_STIPPLE_CODE
#endif

#if defined(__GNUC__) && !defined(GLYPHROP) && (defined(mc68020) || defined(mc68000) || defined(__mc68000__)) && !defined(USE_LEFTBITS)
#include    <stip68kgnu.h>
#endif

void
cfbPolyGlyphBlt8 (pDrawable, pGC, x, y, nglyph, ppci, pglyphBase)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int 	x, y;
    unsigned int nglyph;
    CharInfoPtr *ppci;		/* array of character info */
    unsigned char *pglyphBase;	/* start of array of glyphs */
{
    register unsigned long  c;
#ifndef GLYPHROP
    register unsigned long  pixel;
#endif
    register unsigned long  *dst;
    register glyphPointer   glyphBits;
    register int	    xoff;
    register int	    ewTmp;

    FontPtr		pfont = pGC->font;
    CharInfoPtr		pci;
    unsigned long	*dstLine;
    unsigned long	*pdstBase;
    int			hTmp;
    int			bwidthDst;
    int			widthDst;
    int			h;
    int			ew;
    BoxRec		bbox;		/* for clipping */
    int			widthDiff;
    int			w;
    RegionPtr		clip;
    BoxPtr		extents;
#ifdef USE_LEFTBITS
    int			widthGlyph;
    unsigned long	widthMask;
#endif
#ifndef STIPPLE
#ifdef USE_STIPPLE_CODE
    void		(*stipple)();
    extern void		stipplestack (), stipplestackte ();

    stipple = stipplestack;
    if (FONTCONSTMETRICS(pfont))
	stipple = stipplestackte;
#endif
#endif
    
    x += pDrawable->x;
    y += pDrawable->y;

    /* compute an approximate (but covering) bounding box */
    bbox.x1 = 0;
    if ((ppci[0]->metrics.leftSideBearing < 0))
	bbox.x1 = ppci[0]->metrics.leftSideBearing;
    h = nglyph - 1;
    w = ppci[h]->metrics.rightSideBearing;
    while (--h >= 0)
	w += ppci[h]->metrics.characterWidth;
    bbox.x2 = w;
    bbox.y1 = -FONTMAXBOUNDS(pfont,ascent);
    bbox.y2 = FONTMAXBOUNDS(pfont,descent);

    clip = ((cfbPrivGC *)(pGC->devPrivates[cfbGCPrivateIndex].ptr))->pCompositeClip;
    extents = &clip->extents;

    if (!clip->data) 
    {
	if (!BOX_CONTAINS(extents, &bbox, x, y))
	{
	    if (BOX_OVERLAP (extents, &bbox, x, y))
		cfbPolyGlyphBlt8Clipped(pDrawable, pGC, x, y, nglyph, ppci, pglyphBase);
	    return;
	}
    }
    else
    {
    	/* check to make sure some of the text appears on the screen */
    	if (!BOX_OVERLAP (extents, &bbox, x, y))
	    return;
    
    	bbox.x1 += x;
    	bbox.x2 += x;
    	bbox.y1 += y;
    	bbox.y2 += y;
    
    	switch ((*pGC->pScreen->RectIn)(clip, &bbox))
    	{
      	  case rgnPART:
	    cfbPolyGlyphBlt8Clipped(pDrawable, pGC, x, y, nglyph, ppci, pglyphBase);
      	  case rgnOUT:
	    return;
    	}
    }

#ifdef GLYPHROP
    cfb8CheckStipple (pGC->alu, pGC->fgPixel, pGC->planemask);
#else
    pixel = ((cfbPrivGCPtr) pGC->devPrivates[cfbGCPrivateIndex].ptr)->xor;
#endif

    cfbGetTypedWidthAndPointer (pDrawable, bwidthDst, pdstBase, char, unsigned long)

    widthDst = bwidthDst >> 2;
    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);
    while (nglyph--)
    {
	pci = *ppci++;
	glyphBits = (glyphPointer) FONTGLYPHBITS(pglyphBase,pci);
	xoff = x + pci->metrics.leftSideBearing;
	dstLine = pdstBase +
	          (y - pci->metrics.ascent) * widthDst + (xoff >> 2);
	x += pci->metrics.characterWidth;
	if (hTmp = pci->metrics.descent + pci->metrics.ascent)
	{
	    xoff &= 0x3;
#ifdef STIPPLE
	    STIPPLE(dstLine,glyphBits,pixel,bwidthDst,hTmp,xoff);
#else
#ifdef USE_STIPPLE_CODE
	    (*stipple)(dstLine,glyphBits,pixel,bwidthDst,hTmp,xoff);
#else
#ifdef USE_LEFTBITS
	    w = pci->metrics.rightSideBearing - pci->metrics.leftSideBearing;
	    widthGlyph = PADGLYPHWIDTHBYTES(w);
	    widthMask = endtab[w];
#endif
	    do {
	    	dst = dstLine;
	    	dstLine = (unsigned long *) (((char *) dstLine) + bwidthDst);
	    	GlyphBits(glyphBits, w, c)
	    	WriteFourBits(dst, pixel, GetFourBits(BitRight(c,xoff)));
	    	dst++;
	    	c = BitLeft(c,4-xoff);
	    	while (c)
	    	{
		    WriteFourBits(dst, pixel, GetFourBits(c));
		    NextFourBits(c);
		    dst++;
	    	}
	    } while (--hTmp);
#endif /* USE_STIPPLE_CODE else */
#endif /* STIPPLE else */
	}
    }
}

static void
cfbPolyGlyphBlt8Clipped (pDrawable, pGC, x, y, nglyph, ppci, pglyphBase)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int 	x, y;
    unsigned int nglyph;
    CharInfoPtr *ppci;		/* array of character info */
    unsigned char *pglyphBase;	/* start of array of glyphs */
{
    register unsigned long  c;
#ifndef GLYPHROP
    register unsigned long  pixel;
#endif
    register unsigned long  *dst;
    register glyphPointer   glyphBits;
    register int	    xoff;
    register int	    ewTmp;
    unsigned long	    c1;

    CharInfoPtr		pci;
    FontPtr		pfont = pGC->font;
    unsigned long	*dstLine;
    unsigned long	*pdstBase;
    unsigned long	*cTmp, *clips;
    int			maxAscent, maxDescent;
    int			minLeftBearing;
    int			hTmp;
    int			widthDst;
    int			bwidthDst;
    int			ew;
    int			xG, yG;
    BoxPtr		pBox;
    int			numRects;
    int			widthDiff;
    int			w;
    RegionPtr		pRegion;
    int			yBand;
#ifdef USE_LEFTBITS
    int			widthGlyph;
    unsigned long	widthMask;
#endif

#ifdef GLYPHROP
    cfb8CheckStipple (pGC->alu, pGC->fgPixel, pGC->planemask);
#else
    pixel = ((cfbPrivGCPtr) pGC->devPrivates[cfbGCPrivateIndex].ptr)->xor;
#endif
    
    cfbGetTypedWidthAndPointer (pDrawable, bwidthDst, pdstBase, char, unsigned long)

    widthDst = bwidthDst >> 2;
    maxAscent = FONTMAXBOUNDS(pfont,ascent);
    maxDescent = FONTMAXBOUNDS(pfont,descent);
    minLeftBearing = FONTMINBOUNDS(pfont,leftSideBearing);

    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);

    pRegion = ((cfbPrivGC *)(pGC->devPrivates[cfbGCPrivateIndex].ptr))->pCompositeClip;

    pBox = REGION_RECTS(pRegion);
    numRects = REGION_NUM_RECTS (pRegion);
    while (numRects && pBox->y2 <= y - maxAscent)
    {
	++pBox;
	--numRects;
    }
    if (!numRects || pBox->y1 >= y + maxDescent)
	return;
    yBand = pBox->y1;
    while (numRects && pBox->y1 == yBand && pBox->x2 <= x + minLeftBearing)
    {
	++pBox;
	--numRects;
    }
    if (!numRects)
	return;
    clips = (unsigned long *)ALLOCATE_LOCAL ((maxAscent + maxDescent) *
					     sizeof (unsigned long));
    while (nglyph--)
    {
	pci = *ppci++;
	glyphBits = (glyphPointer) FONTGLYPHBITS(pglyphBase,pci);
	w = pci->metrics.rightSideBearing - pci->metrics.leftSideBearing;
	xG = x + pci->metrics.leftSideBearing;
	yG = y - pci->metrics.ascent;
	x += pci->metrics.characterWidth;
	if (hTmp = pci->metrics.descent + pci->metrics.ascent)
	{
	    dstLine = pdstBase + yG * widthDst + (xG >> 2);
	    xoff = xG & 0x3;
#ifdef USE_LEFTBITS
	    w = pci->metrics.rightSideBearing - pci->metrics.leftSideBearing;
	    widthGlyph = PADGLYPHWIDTHBYTES(w);
	    widthMask = endtab[w];
#endif
	    switch (cfb8ComputeClipMasks32 (pBox, numRects, xG, yG, w, hTmp, clips))
 	    {
	    case rgnPART:
#ifdef USE_LEFTBITS
	    	cTmp = clips;
	    	do {
	    	    dst = dstLine;
	    	    dstLine = (unsigned long *) (((char *) dstLine) + bwidthDst);
	    	    GlyphBits(glyphBits, w, c)
		    c &= *cTmp++;
		    if (c)
		    {
	    	    	WriteFourBits(dst, pixel, GetFourBits(BitRight(c,xoff)));
	    	    	c = BitLeft(c,4 - xoff);
	    	    	dst++;
	    	    	while (c)
	    	    	{
		    	    WriteFourBits(dst, pixel, GetFourBits(c));
		    	    NextFourBits(c);
		    	    dst++;
	    	    	}
		    }
	    	} while (--hTmp);
	    	break;
#else
	    	{
		    int h;
    
		    h = hTmp;
		    do
		    {
			--h;
		    	clips[h] = clips[h] & glyphBits[h];
		    } while (h);
	    	}
	    	glyphBits = clips;
	    	/* fall through */
#endif
	    case rgnIN:
#ifdef STIPPLE
	    	STIPPLE(dstLine,glyphBits,pixel,bwidthDst,hTmp,xoff);
#else
#ifdef USE_STIPPLE_CODE
	    	stipplestackte(dstLine,glyphBits,pixel,bwidthDst,hTmp,xoff);
#else
	    	do {
	    	    dst = dstLine;
	    	    dstLine = (unsigned long *) (((char *) dstLine) + bwidthDst);
	    	    GlyphBits(glyphBits, w, c)
		    if (c)
		    {
	    	    	WriteFourBits(dst, pixel, GetFourBits(BitRight(c,xoff)));
	    	    	c = BitLeft(c,4-xoff);
	    	    	dst++;
	    	    	while (c)
	    	    	{
		    	    WriteFourBits(dst, pixel, GetFourBits(c));
		    	    NextFourBits(c);
		    	    dst++;
	    	    	}
		    }
	    	} while (--hTmp);
#endif /* USE_STIPPLE_CODE else */
#endif /* STIPPLE else */
	    	break;
	    }
	}
    }
    DEALLOCATE_LOCAL (clips);
}

#endif /* PPW == 4 */
