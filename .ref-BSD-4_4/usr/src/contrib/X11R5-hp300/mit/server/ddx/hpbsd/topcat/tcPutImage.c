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
 *  file: tcPutImage.c
 *
 *      PutImage routine for Topcat (should work for Catseye) displays  
 * 
 *
 *
 *		Hewlett Packard -- Corvallis Workstation Operation
 *		Project -- port of X11 to HP9000
 *		Harry Phinney -- MTS
 *
 *
 */

#include "X.h"
#include "Xprotostr.h"

#include "misc.h"
#include "gcstruct.h"
#include "pixmapstr.h"
#include "windowstr.h"
#include "scrnintstr.h"
#include "mi.h"
#include "regionstr.h"
#include "Xmd.h"
#include "servermd.h"
#include "../cfb/cfb.h"
#include "topcat.h"

extern u_char XHP_NewRule[16][6];
extern int XHP_pmap[256][2];
extern int XHP_QUADALIGN;


/* tcPutImage -- entry for the PutImage Request
 *  This is here to try to make XY pixmap puts work reasonably on
 *  a Topcat/Catseye framebuffer.  It uses the "pmap" array gleaned from 
 *  Fort C. folks (thanks!) in conjunction with the video card's replacement
 *  rule and plane enable registers.
 */
void
tcPutImage(pDraw, pGC, depth, dstx, dsty, w, h, srcOffset, format, pImage)
    DrawablePtr         pDraw;
    GCPtr               pGC;
    int                 depth, dstx, dsty, w, h, srcOffset;
    unsigned int        format;
    unsigned char       *pImage;
{
    TOPCAT *hardware = getTcHardware(pDraw->pScreen);
    unsigned long screenPlanes = getPlanesMask(pDraw->pScreen);
    unsigned long planeMask = pGC->planemask & screenPlanes;
    register int i;
    unsigned int planeSize, srcStride, widthDst, garbageBits, bits;
    unsigned int leadingBits, trailingBits, wholeBytes;
    CARD8 *psrc, *psrcLine, *psrcBase, *pdstBase, *pdstLine, *pdst;
    RegionPtr pRegion;
    unsigned int alu;
    register BoxPtr pBox;
    unsigned int numBoxes;
    CARD8 srcByte;
    unsigned int rows;
    unsigned int pixels;
    static unsigned char masks[8] = { 0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 
				     0x40, 0x80 };
    static unsigned char trailMasks[8] = { 0x80, 0x40, 0x20, 0x10, 0x8, 0x4, 
				     0x2, 0x1 };

    if((format == ZPixmap) || ((pDraw->type == DRAWABLE_PIXMAP) &&
	(((PixmapPtr)(pDraw))->devKind == PIXMAP_HOST_MEMORY)))
    {
	/*
	 * mi seems to work for ZPixmaps
	 * and my code doesn't work for main-memory pixmaps since
	 * I rely on the hardware plane-enable
	 */
	miPutImage(pDraw, pGC, depth, dstx, dsty, w, h, srcOffset, 
		   format, pImage);
	return;
    }

    alu = pGC->alu;
    pRegion = ((cfbPrivGC *)(pGC->devPrivates[cfbGCPrivateIndex].ptr))->pCompositeClip;
    pBox = REGION_RECTS(pRegion);
    numBoxes = REGION_NUM_RECTS(pRegion);

    if (pDraw->type == DRAWABLE_WINDOW)
    {
	dstx += pDraw->x;
	dsty += pDraw->y;
	widthDst = (unsigned int) getPrivScreenPtr(pDraw->pScreen)->stride;
	pdstBase = (CARD8 *) getPrivScreenPtr(pDraw->pScreen)->bits; 
    }
    else
    {
        widthDst = (unsigned int)
            (((hpPrivPixmapPtr)(((PixmapPtr)pDraw)->devPrivate.ptr))->stride);
        pdstBase = (CARD8 *)
            (((hpPrivPixmapPtr)(((PixmapPtr)pDraw)->devPrivate.ptr))->bits); 
    }

    srcStride = PixmapBytePad(w, 1);
    planeSize = h * srcStride;

    waitbusy(screenPlanes, hardware);
    while (numBoxes--)
    {
	int clippedWidth = w - srcOffset;
	int clippedHeight = h;
	int startx = dstx;
	int starty = dsty;
	int dx = 0, dy = 0;
	CARD8 *pdstBox = pdstBase;

	psrcBase = (CARD8 *)pImage;

	/*
	 * clip the height
	 */
	if (dsty < pBox->y1)
	{
	    dy = pBox->y1 - dsty;
	    clippedHeight -= dy;
	    if (clippedHeight <= 0)
	    {
		pBox++;
		continue;
	    }
	    starty = pBox->y1;
	}
	if (starty+clippedHeight > pBox->y2)
	{
	    clippedHeight = pBox->y2 - starty;
	    if (clippedHeight <= 0) 
	    {
		pBox++;
		continue;
	    }
	}
	/*
	 * clip the width
	 */
	if (dstx < pBox->x1)
	{
	    dx = pBox->x1 - dstx;
	    clippedWidth -= dx;
	    if (clippedWidth <= 0)
	    {
		pBox++;
		continue;
	    }
	    startx = pBox->x1;
	}
	if (startx+clippedWidth > pBox->x2)
	{
	    clippedWidth = pBox->x2 - startx;
	    if (clippedWidth <= 0)
	    {
		pBox++;
		continue;
	    }
	}

	if ((clippedWidth <= 0) || (clippedHeight <= 0))
	{
	    pBox++;
	    continue;
	}

	/*
	 * get address of the first destination pixel
	 */
	pdstBox += startx + starty * widthDst;
	/*
	 * get address of the first source bytes with useful bits
	 */
	psrcBase += srcStride * dy + (srcOffset + dx) / 8;
	garbageBits = (srcOffset + dx) % 8;

	leadingBits = 8 - garbageBits;
	if (leadingBits == 8)
	    leadingBits = 0;
	else if (leadingBits > clippedWidth)
	    leadingBits = clippedWidth;

	wholeBytes = (clippedWidth - leadingBits) / 8;
	trailingBits = clippedWidth - (wholeBytes * 8 + leadingBits);

	if (depth == 1)
	{
	    /*
	     * XYBitmap code
	     */
	    unsigned int fore = pGC->fgPixel;
	    unsigned int back = pGC->bgPixel;

	    /*
	     * Set up the registers so that writing 0xff will get us the
	     * foreground and writing 0x00 will get the background with
	     * the proper replacement rule 
	     */
	    hardware->write_enable = planeMask & ~fore & ~back;
	    hardware->pixel_write_replacement_rule = XHP_NewRule[alu] [0];
	    hardware->write_enable = planeMask & ~fore & back;
	    hardware->pixel_write_replacement_rule = XHP_NewRule[alu] [1];
	    hardware->write_enable = planeMask & fore & ~back;
	    hardware->pixel_write_replacement_rule = XHP_NewRule[alu] [2];
	    hardware->write_enable = planeMask & fore & back;
	    hardware->pixel_write_replacement_rule = XHP_NewRule[alu] [3];

	    hardware->frame_buf_write_enable = planeMask;

	    psrcLine = psrcBase;
	    pdstLine = pdstBox;
	    pixels = clippedWidth;

	    for (rows = clippedHeight; rows--;
		pixels = clippedWidth, 
		psrcLine += srcStride, 
		pdstLine += widthDst)
	    {
		psrc = psrcLine;
		pdst = pdstLine;

		/*
		 * write any leading bits one at a time until
		 * we're at a whole byte
		 */
		if (leadingBits)
		{
		    srcByte = *psrc++;

		    for (i = leadingBits - 1; i >= 0; i--) 
		    {
			if (srcByte & masks[i])
			    *pdst++ = 0xff;
			else
			    *pdst++ = 0x00;
		    }
		}

		/*
		 * write out all the whole bytes of bitmap
		 */
		if (!XHP_QUADALIGN || !((unsigned int)pdst & 0x01))
		{ 
		    /* 
		     * either we're on a 68020, or we meet the alignment 
		     * requirements of a 68010, so we write 4 pixels at a time 
		     */
		    register unsigned int *pdstInt = (unsigned int *) pdst;

		    for (i = 0; i < wholeBytes; i++)
		    {
			srcByte = *psrc++;
			*pdstInt++ = XHP_pmap[srcByte][0];
			*pdstInt++ = XHP_pmap[srcByte][1];
		    }
		    pdst = (CARD8 *) pdstInt;
		}
		else
		{
		    /*
		     * we're unaligned on a 310 (bummer...)
		     */
		    for (i = 0; i < wholeBytes; i++)
		    {
			srcByte = *psrc++;
			bits = 8;
			while (bits--)
			{
			    if (srcByte & masks[bits])
				*pdst++ = 0xff;
			    else
				*pdst++ = 0x00;
			}
		    }
		}
          
		/*
		 * write any trailing raggedy bits at the end of the row
		 */
		if (trailingBits)
		{
		    srcByte = *psrc;

		    for (i = 0; i < trailingBits; i++)
		    {
			if (srcByte & trailMasks[i])
			    *pdst++= 0xff;
			else
			    *pdst++= 0x00;
		    }
		}
	    }
	}
	else
	{			/* XYPixmap case */
	    int j;
	    for (j = 1 << (depth - 1); j > 0; j >>= 1, psrcBase += planeSize)
	    {
		if (j & planeMask)
		{
		    rows = clippedHeight;
		    pixels = clippedWidth;

		    /*
		     * write enable the one plane we want to alter
		     * and set the rep_rule according to the GC
		     */
		    SET_REGISTERS_FOR_WRITING(pDraw->pScreen, j, alu);

		    psrcLine = psrcBase;
		    pdstLine = pdstBox;
		    for (rows = clippedHeight; rows--;
			 pixels = clippedWidth, 
			 psrcLine += srcStride, 
			 pdstLine += widthDst)
		    {
			psrc = psrcLine;
			pdst = pdstLine;

			/*
			 * write any leading bits one at a time
			 * until at a whole byte
			 */
			if (leadingBits)
			{
			    srcByte = *psrc++;

			    for (i = leadingBits - 1; i >= 0; i--)
			    {
				if (srcByte & masks[i])
				    *pdst++ = 0xff;
				else
				    *pdst++ = 0x00;
			    }
			}

			/*
			 * write out all the whole bytes of bitmap
			 */
			if (!XHP_QUADALIGN || !((unsigned int)pdst & 0x01))
			{ 
			    /* 
			     * either we're on a 68020, or we meet the alignment 
			     * requirements of a 68010, so we write 4 pixels at a time 
			     */
			    register unsigned int *pdstInt = (unsigned int *) pdst;

			    for (i = 0; i < wholeBytes; i++)
			    {
				srcByte = *psrc++;
				*pdstInt++ = XHP_pmap[srcByte][0];
				*pdstInt++ = XHP_pmap[srcByte][1];
			    }
			    pdst = (CARD8 *) pdstInt;
			}
			else
			{
			    /*
			     * we're unaligned on a 310 (bummer...)
			     */
			    for (i = 0; i < wholeBytes; i++)
			    {
				srcByte = *psrc++;
				bits = 8;
				while (bits--)
				{
				    if (srcByte & masks[bits])
					*pdst++ = 0xff;
				    else
					*pdst++ = 0x00;
				}
			    }
			}
          
			/*
			 * write any trailing raggedy bits at the end of the row
			 */
			if (trailingBits)
			{
			    srcByte = *psrc;

			    for (i = 0; i < trailingBits; i++)
			    {
				if (srcByte & trailMasks[i])
				    *pdst++= 0xff;
				else
				    *pdst++= 0x00;
			    }
			}
		    }
		}
	    }
	}
	pBox++;
    }
    SET_REGISTERS_FOR_WRITING(pDraw->pScreen, 0xff, alu);
}

