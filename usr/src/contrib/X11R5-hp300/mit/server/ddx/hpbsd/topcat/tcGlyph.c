/*

Copyright (c) 1986, 1987 by Hewlett-Packard Company
Copyright (c) 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD
TO THIS SOFWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
PURPOSE.  Hewlett-Packard shall not be liable for errors 
contained herein or direct, indirect, special, incidental or 
consequential damages in connection with the furnishing, 
performance, or use of this material.

This software is not subject to any license of the American
Telephone and Telegraph Company or of the Regents of the
University of California.

*/
/***********************************************************************
 *  file: topcatGlyph.c
 *
 *
 *  ******************************************************************
 *  *  (c) Copyright Hewlett-Packard Company, 1987.  All rights are  *
 *  *  reserved.  Copying or other reproduction of this program      *
 *  *  except for archival purposes is prohibited without prior      *
 *  *  written consent of Hewlett-Packard Company.		     *
 *  ******************************************************************
 *
 *  Glyph output routines
 *
 *		Hewlett Packard -- Corvallis Workstation Operation
 *		Project -- port of X11 to HP9000
 *		Harry Phinney -- MTS
 *
 *
 */

#include <sys/types.h>

#include "X.h"
#include "Xmd.h"
#include "Xproto.h"
#include "../cfb/cfb.h"
#include "fontstruct.h"
#include "dixfontstr.h"
#include "gcstruct.h"
#include "regionstr.h"
#include "region.h"
#include "pixmapstr.h"
#include "pixmap.h"
#include "scrnintstr.h"
#include "windowstr.h"

#include "topcat.h"

extern u_char XHP_NewRule[16][6];
extern int XHP_pmap[256][2];
extern int XHP_QUADALIGN;

#if 0
static unsigned char masks[8] = { 0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80 };
#endif
static unsigned char trailMasks[8] = { 0x80, 0x40, 0x20, 0x10, 0x8, 0x4,
                                       0x2, 0x1 };

#define DispAddr(pScreen, x, y) \
      ((getPrivScreenPtr(pScreen)->bits) + \
       ((y) * getPrivScreenPtr(pScreen)->stride) + \
       (x));

/************************************************************************
 *  Routine:	tcWholeGlyph
 *		Output an entire glyph with no clipping
 *		Used for putting chars in offscreen memory
 *
 *  Inputs: pScn points to the ScreenRec for the screen the glyph is going to
 *	    pGlyph points to the bits of the glyph we're outputing
 *	    pCi points to the character info struct for the glyph
 *	    x,y specifies the screen location of the upper-left corner of glyph
 *	    func is the display function (ROP)
 *	    fore, back are the foreground and background colors
 *	    planes is the plane mask (zmask)
 *  
 *  Returns: nothing (it's a void...)
 *
 *  Side Effects:  none
 *
 */
void
tcWholeGlyph(pScn, pGlyph, pCi, x,y, func, fore, back, planes)
     ScreenPtr pScn;
     char *pGlyph;
     CharInfoPtr pCi;
     int x, y;
     int func;
     int fore, back;
     int planes;
{
    int gwidth, gheight, rowBytes;
    int i, j;
    TOPCAT *hardware = getTcHardware(pScn);
    int XHP_bits = hardware->bits;

    gwidth = GLYPHWIDTHPIXELS(pCi);
    gheight = GLYPHHEIGHTPIXELS(pCi);

    rowBytes = GLYPHWIDTHBYTESPADDED(pCi);

    /*
     * call the MaskConfig routine to set the regs
     */
    SET_REGISTERS_FOR_WRITING(pScn, planes, func);

    /*
     * if word/pixel (low res topcat) then write words, else write bytes
     */
    if (XHP_bits)
    {
	register u_short *dest, *base;
	char srcbyte;
	char *pNextByte;

	base = (u_short *) DispAddr(pScn, x << XHP_bits, y);

	/*
	 * make the colors word-wide symetric
	 */
	fore |= fore << 8;
	if (back != -1) back |= back << 8;

	while (gheight--)
	{
	    dest = base;
	    base += ((hpPrivScreen *)(pScn->devPrivate))->stride;

	    srcbyte = *pGlyph;
	    pNextByte = pGlyph + 1;
	    pGlyph += rowBytes;

	    i = 0;
	    j = gwidth;
	    while (j--)
	    {
		if (i++ == 8)
		{
		    srcbyte = *pNextByte++;
		    i = 1;
		}
		if (srcbyte & 128) /* put out the MSB */
		    *dest++ = fore;
		else if(back != -1)
		    *dest++ = back;
		else
		    dest++;
	    
		srcbyte <<= 1; /* get next pix value in MSB */
	    }
	}
	return;
    }
    else
    {				/* high res topcat - copy bytes */
	/*
	 * code for byte/pixel displays
	 * XXX This could/should be sped up by writing more than one pixel at
	 * a time - i.e. write longs & shorts when appropriate.
	 */
	register u_char *dest, *base;
	register char srcbyte;
	char *pNextByte;

	base = (u_char *) DispAddr(pScn, x, y);

	while (gheight--)
	{			/* for each row of pixels in glyph */
	    dest = base;
	    base += ((hpPrivScreen *)(pScn->devPrivate))->stride;
  
	    srcbyte = *pGlyph;
	    pNextByte = pGlyph + 1;
	    pGlyph += rowBytes;

	    i = 0; /* counter of bits put out from current byte */
	    j = gwidth; /* total bits in this row */
	    while (j--)
	    {
		if (i++ == 8)
		{ /* if we've used up the byte, go to next one */
		    srcbyte = *pNextByte++;
		    i = 1;
		}
		if (srcbyte & 128)
		    *dest++ = fore;
		else if(back != -1)
		    *dest++ = back;
		else
		    dest++;

		srcbyte <<= 1; /* get next pix bit in MSB */
	    } /* next bit in this row */
	} /* next pixel row */
    } /* end of byte/pixel code */
}


/************************************************************************
 *  Routine:	tcImageGlyphBlt
 *		Output a set of glyphs
 *		Called by the tcImageText routine(s) iff the glyphs 
 * 		are not in offscreen memory and the destination drawable 
 *		is a window.
 *
 *  Inputs: pDraw points to the *window* we're drawing to
 *	    pGC points to the graphics context
 *	    dstx,dsty is the screen location of the glyph baseline
 *	    nglyph is the number of glyphs
 *	    ppCI is a pointer to the array of CharInfo's for the glyphs
 *	    pGlyphBase points to the array of glyph bits
 *  
 *  Returns: nothing (it's a void...)
 *
 *  Side Effects:  none
 *
 */

void
tcImageVarGlyph(pDraw, pGC, dstx, dsty, nglyph, ppCI, pGlyphBase)
    DrawablePtr pDraw;
    GC *pGC;
    unsigned int dstx, dsty;
    unsigned int nglyph;
    CharInfoPtr *ppCI;
    char *pGlyphBase;
{
#if 0
    ExtentInfoRec info;		/* used by QueryGlyphExtents() */
    xRectangle backrect;
    RegionPtr pRegion = ((cfbPrivGC *)
			 pGC->devPrivates[cfbGCPrivateIndex].ptr)->pCompositeClip;
    register BoxPtr pBox;
    int nbox;
    TOPCAT *hardware = getTcHardware(pDraw->pScreen);
    unsigned long screenPlanes = getPlanesMask(pDraw->pScreen);
    unsigned long zmask = pGC->planemask & screenPlanes;
    int back = pGC->bgPixel;
    u_char *dest, *base;
    int stride = ((hpPrivScreen *)(pDraw->pScreen->devPrivate))->stride;
    register int i, x, y, dx, dy, h, w;
    int XHP_bits = hardware->bits;
    int fontAscent = FONTASCENT(pGC->font);

    if (pDraw->type != DRAWABLE_WINDOW)
    {
	if (pGC->bgPixel== -1) 
	   miPolyGlyphBlt(pDraw, pGC, dstx, dsty, nglyph, ppCI, pGlyphBase);
	else
	   miImageGlyphBlt(pDraw, pGC, dstx, dsty, nglyph, ppCI, pGlyphBase);
    }

    if (!nglyph)
	return;

    if (pGC->miTranslate)
    {				/* absolutize the coordinates */
	dstx += pDraw->x;
	dsty += pDraw->y;
    }

    QueryGlyphExtents(pGC->font, ppCI, nglyph, &info);

    backrect.x = dstx;
    backrect.y = dsty - fontAscent;
    backrect.width = info.overallWidth;
    backrect.height = fontAscent + FONTDESCENT(pGC->font);

    for (nbox = REGION_NUM_RECTS(pRegion), pBox = REGION_RECTS(pRegion);
	 nbox--; pBox++)
    {
	/* 
	 * for each rectangle in the list of clip rects, block fill the
	 * background for the characters and put down the foreground pixels
	 * from the glyphs
	 */
	x = backrect.x;
	y = backrect.y;
	w = backrect.width;
	h = backrect.height;
	dx = 0;
	dy = 0;

	/*
	 * figure out the intersection between the character background box
	 * and the clip box, and fill the resulting rectangle
	 */
	if (y < pBox->y1)
	{
	    dy = pBox->y1 - y;
	    h -= dy;
	    if (h <= 0) continue;
	    y = pBox->y1;
	}
	if (y+h > pBox->y2)
	{
	    h = pBox->y2 - y;
	    if (h <= 0) continue;
	}

	if (x < pBox->x1)
	{
	    dx = pBox->x1 - x;
	    w -= dx;
	    if (w <= 0) continue;
	    x = pBox->x1;
	}
	if (x+w > pBox->x2)
	{
	    w = pBox->x2 - x;
	    if (w <= 0) continue;
	}

	if (h > 0 && w > 0)
	{			/* if the intersection was non-null */
	    int charx = dstx;
	    int slen = nglyph;
	    CharInfoPtr *ppci = ppCI;
	    register CharInfoPtr pci = *ppci++;
	    char *pGlyph;

	    /*
	     * clear the rect to the background color
	     */
	    waitbusy(screenPlanes, hardware); 
	    hardware->write_enable = zmask & back;
	    hardware->window_move_replacement_rule = XHP_NewRule[GXcopy][3];
	    hardware->write_enable = zmask & ~back;
	    hardware->window_move_replacement_rule = XHP_NewRule[GXcopy][0];
	    hardware->write_enable = zmask;
	    hardware->pixel_write_replacement_rule = GXcopy;
                
	    hardware->source_x = x << XHP_bits;
	    hardware->source_y = y;
	    hardware->dest_x = x << XHP_bits;
	    hardware->dest_y = y;
	    hardware->window_width = w << XHP_bits;
	    hardware->window_height = h;
	    hardware->start_move = zmask;

	    /*
	     * while the character is to the left of the clipping rectangle,
	     * go to the next character
	     */
	    while (charx + pci->metrics.characterWidth < pBox->x1)
	    {
		if (--slen == 0) break;
		charx += pci->metrics.characterWidth;
		pci = *ppci++;
	    }

	    /*
	     * clip the first (partially) in char to the left edge of pBox
	     */
	    dx = pBox->x1 - (charx + pci->metrics.leftSideBearing);
	    if (dx < 0) dx = 0;
	    if (charx+dx > pBox->x2) continue;

	    /*
	     * Set up the registers so that writing 0xff will get us the
	     * foreground and writing 0x00 is a noop
	     */
	    waitbusy(screenPlanes, hardware);
	    hardware->write_enable = zmask & pGC->fgPixel;
	    hardware->pixel_write_replacement_rule = XHP_NewRule[GXcopy][4];
	    hardware->write_enable = zmask & ~pGC->fgPixel;
	    hardware->pixel_write_replacement_rule = XHP_NewRule[GXcopy][5];
	    hardware->frame_buf_write_enable = zmask;

	    for (; slen--; pci = *ppci++)
	    {
		int gwidth = GLYPHWIDTHPIXELS(pci);
		int gheight = GLYPHHEIGHTPIXELS(pci);
		int rowBytes = GLYPHWIDTHBYTESPADDED(pci);
		int height = gheight;
		int glyphy = dsty - pci->metrics.ascent;

		if (glyphy < pBox->y1)
		{
		    dy = pBox->y1 - glyphy;
		    height -= dy;
		    glyphy = pBox->y1;
		    if (height <= 0)
		    {
			charx += pci->metrics.characterWidth;
			dx = 0;
			if (charx >= pBox->x2) break;
			continue;
		    }
		}
		else dy = 0;

		if (glyphy+height > pBox->y2)
		{
		    height = pBox->y2 - glyphy;
		    if (height <= 0)
		    {
			charx += pci->metrics.characterWidth;
			dx = 0;
			if (charx >= pBox->x2) break;
			continue;
		    }
		}

		/* get address of first glyph row in the clip rect */
		pGlyph = pGlyphBase + pci->byteOffset + dy * rowBytes;

		base = (u_char *) DispAddr(pDraw->pScreen, 
					   charx + pci->metrics.leftSideBearing + dx, glyphy);

		/*
		 * clip to right edge
		 */
		if ((charx+gwidth+pci->metrics.leftSideBearing) > pBox->x2)
		{
		    gwidth = pBox->x2 - (charx+pci->metrics.leftSideBearing);
		    if (gwidth <= 0) break;
		}

		if (gwidth > dx)
		{
		    int wholeBytes;
		    unsigned char srcbyte;
		    char *pNextByte;

		    if ((!XHP_QUADALIGN || ((unsigned int)base & 0x01)) &&
			!dx && (wholeBytes = gwidth / 8))
		    {
			int trailingBits = gwidth - (wholeBytes << 3);
			unsigned char srcbyte;

			for (;height--; pGlyph += rowBytes, base += stride)
			{
			    register unsigned int *pdstInt = (unsigned int *) base;
			    srcbyte = *(pNextByte = pGlyph);
			    pNextByte++;
			    for (i = wholeBytes; i--; srcbyte = *pNextByte++)
			    {
				*pdstInt++ = XHP_pmap[srcbyte][0];
				*pdstInt++ = XHP_pmap[srcbyte][1];
			    }
			    if (trailingBits)
			    {
				dest = (u_char *) pdstInt;
				for (i = 0; i < trailingBits; i++)
				{
				    if (srcbyte & trailMasks[i])
					*dest++ = 0xff;
				    else
					dest++;
				}
			    }
			}
		    }
		    else
		    {
			while (height--)
			{
			    register j;

			    srcbyte = *(pNextByte = pGlyph + (dx / 8)) << (dx % 8);
			    pNextByte++;
			    pGlyph += rowBytes;

			    dest = base;
			    base += stride;
			    i = dx & 7;
			    j = gwidth - dx;

			    while(j--)
			    {
				if (i++ == 8)
				{
				    srcbyte = *pNextByte++;
				    i = 1;
				}
				if (srcbyte & 128) 
				    *dest++ = 0xff;
				else 
				    dest++;
				srcbyte <<= 1;
			    }
			}
		    }
		}
		charx += pci->metrics.characterWidth;
		dx = 0;
		if (charx >= pBox->x2)
		    slen = 0; /* kind of slimey break from loop */
	    }
	}
    }
    hardware->frame_buf_write_enable = 0xff;
#endif
}

void
tcImageGlyphBlt(pDraw, pGC, dstx, dsty, nglyph, ppCI, pGlyphBase)
    DrawablePtr pDraw;
    register GC *pGC;
    unsigned int dstx, dsty;
    unsigned int nglyph;
    CharInfoPtr *ppCI;
    char *pGlyphBase;
{
#if 0
    xRectangle backrect;
    RegionPtr pRegion = ((cfbPrivGC *)
			 pGC->devPrivates[cfbGCPrivateIndex].ptr)->pCompositeClip;
    register BoxPtr pBox;
    int nbox;
    TOPCAT *hardware = getTcHardware(pDraw->pScreen);
    unsigned long screenPlanes = getPlanesMask(pDraw->pScreen);
    unsigned long zmask = pGC->planemask & screenPlanes;
    u_char *dest, *base;
    int stride = ((hpPrivScreen *)(pDraw->pScreen->devPrivate))->stride;
    register int i, x, y, dx, dy, h, w, dyBytes;
    int fontAscent = FONTASCENT(pGC->font);
    int glyphWidth, gwidth, height;
    int rowBytes = GLYPHWIDTHBYTESPADDED((*ppCI)); 

    if (!nglyph)
	return;

    if (!TERMINALFONT(pGC->font))
    {
	/*
	 * if the font isn't a terminal font, then call the routine that
	 * knows how to deal with variable-width fonts
	 */
	tcImageVarGlyph(pDraw, pGC, dstx, dsty, nglyph, ppCI, pGlyphBase);
	return;
    }

    if (pGC->miTranslate)
    {				/* absolutize the coordinates */
	dstx += pDraw->x;
	dsty += pDraw->y;
    }

    backrect.x = dstx;
    backrect.y = dsty - fontAscent;
    backrect.width = nglyph *  (glyphWidth =
		     (FONTMAXBOUNDS(pGC->font,rightSideBearing) -
		      FONTMINBOUNDS(pGC->font,leftSideBearing)));
    backrect.height = fontAscent + FONTDESCENT(pGC->font);

    waitbusy(screenPlanes, hardware); /* wait for all planes to quiet */
    /*
     * set registers to get proper foreground/background colors
     */
    if (pGC->bgPixel == -1)
    {
	/*
	 * handle special case of PolyText, so PolyText can be fast if
	 * it uses a solid fill
	 */
	hardware->write_enable = zmask & pGC->fgPixel;
	hardware->pixel_write_replacement_rule = XHP_NewRule[pGC->alu][4];
	hardware->write_enable = zmask & ~pGC->fgPixel;
	hardware->pixel_write_replacement_rule = XHP_NewRule[pGC->alu][5];
    }
    else
    {
	/*
	 * Set registers for normal ImageText case.  Note that pGC->alu
	 * is ignored, and GXcopy is forced.
	 */
	hardware->write_enable = zmask & ~pGC->fgPixel & ~pGC->bgPixel;
	hardware->pixel_write_replacement_rule = XHP_NewRule[GXcopy] [0];
	hardware->write_enable = zmask & ~pGC->fgPixel & pGC->bgPixel;
	hardware->pixel_write_replacement_rule = XHP_NewRule[GXcopy] [1];
	hardware->write_enable = zmask & pGC->fgPixel & ~pGC->bgPixel;
	hardware->pixel_write_replacement_rule = XHP_NewRule[GXcopy] [2];
	hardware->write_enable = zmask & pGC->fgPixel & pGC->bgPixel;
	hardware->pixel_write_replacement_rule = XHP_NewRule[GXcopy] [3];
    }
    hardware->write_enable = zmask;
    /* XXX for catseye */
    hardware->frame_buf_write_enable = zmask;

    for (nbox = REGION_NUM_RECTS(pRegion), pBox = REGION_RECTS(pRegion);
	 nbox--; pBox++)
    {
	/* 
	 * for each rectangle in the list of clip rects, block fill the
	 * background for the characters and put down the foreground pixels
	 * from the glyphs
	 */
	x = backrect.x;
	y = backrect.y;
	w = glyphWidth;
	h = backrect.height;
	dx = 0;
	dy = 0;

	/*
	 * clip the height once since all chars are the same height
	 */
	if (y < pBox->y1)
	{
	    dy = pBox->y1 - y;
	    h -= dy;
	    if (h <= 0) continue;
	    y = pBox->y1;
	}
	if (y+h > pBox->y2)
	{
	    h = pBox->y2 - y;
	    if (h <= 0) continue;
	}

	if (h > 0)
	{
	    int charx = dstx;
	    int slen = nglyph;
	    register CharInfoPtr *ppci = ppCI;
	    register CharInfoPtr pci;
	    char *pGlyph;
	    int wholeBytes = w >> 3;
	    int trailingBits = w - (wholeBytes << 3);
	    u_char *stringBase;

	    dyBytes = dy * rowBytes;
	    /*
	     * while the character is to the left of the clipping rectangle,
	     * go to the next character
	     */
	    while (charx + w <= pBox->x1)
	    {
		if (--slen == 0) break;
		charx += w;
		*ppci++;
	    }
	    if (!slen) continue;

	    /*
	     * clip the first (partially) in char to the left edge of pBox
	     */
	    if ((dx = pBox->x1 - charx) < 0) dx = 0;
	    if (charx+dx >= pBox->x2) continue;

	    /*
	     * trim string length to right edge of pBox
	     */
	    if ((charx + w * slen) >= pBox->x2)
	    {
		/* compute ceiling((pBox->x2 - charx)/w) */
		slen = min((pBox->x2 - charx + w - 1)/w, slen);
		if (slen <= 0) continue;
	    }

	    stringBase = (u_char *) DispAddr(pDraw->pScreen, charx, y);
	    for (base = stringBase + dx, gwidth = w, pci = *ppci++; slen--; 
		 dx = 0, charx += w, pci = *ppci++, base = (stringBase += w))
	    {
		height = h;
		/* get address of first glyph row in the clip rect */
		pGlyph = pGlyphBase + pci->byteOffset + dyBytes;

		/*
		 * clip the last character to the right edge of pBox
		 */
		if (!slen)
		{
		    if ((charx+gwidth) > pBox->x2)
		    {
			if ((gwidth = pBox->x2 - charx) <= 0) break;
			wholeBytes = gwidth >> 3;
			trailingBits = gwidth - (wholeBytes << 3);
		    }
		}

		if (gwidth > dx)
		{
		    unsigned char srcbyte;
		    char *pNextByte;

		    if ((!XHP_QUADALIGN || !((unsigned int)base & 0x01)) &&
			!dx && wholeBytes)
		    {
			unsigned char srcbyte;

			for (;height--; pGlyph += rowBytes, base += stride)
			{
			    register unsigned int *pdstInt = (unsigned int *) base;
			    srcbyte = *(pNextByte = pGlyph);
			    pNextByte++;
			    for (i = wholeBytes; i--; srcbyte = *pNextByte++)
			    {
				*pdstInt++ = XHP_pmap[srcbyte][0];
				*pdstInt++ = XHP_pmap[srcbyte][1];
			    }
			    if (trailingBits)
			    {
				dest = (u_char *) pdstInt;
				for (i = 0; i < trailingBits; i++)
				{
				    if (srcbyte & trailMasks[i])
					*dest++ = 0xff;
				    else
					*dest++ = 0x00;
				}
			    }
			}
		    }
		    else
		    {
			while (height--)
			{
			    register j;

			    srcbyte = *(pNextByte = pGlyph + (dx / 8)) << (dx % 8);
			    pNextByte++;
			    pGlyph += rowBytes;

			    dest = base;
			    base += stride;
			    i = dx & 7;
			    j = gwidth - dx;

			    while (j--)
			    {
				if (i++ == 8)
				{
				    srcbyte = *pNextByte++;
				    i = 1;
				}
				if (srcbyte & 128) 
				    *dest++ = 0xff;
				else 
				    *dest++ = 0x00; 
				srcbyte <<= 1;
			    }
			}
		    }
		}
	    }
	}
    }
    hardware->frame_buf_write_enable = 0xff;
#endif
}
