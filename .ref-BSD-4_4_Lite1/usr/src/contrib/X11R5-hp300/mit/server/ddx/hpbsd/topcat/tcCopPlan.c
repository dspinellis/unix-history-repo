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
 *  file: tcCopyPlane.c
 *
 *      CopyPlane routine for Topcat (should work for Catseye) displays  
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

#define PIXER(Drawable)  ((hpPrivPixmapPtr)((PixmapPtr)Drawable)->devPrivate.ptr)
#define SCRMER(Drawable) getPrivScreenPtr(Drawable->pScreen)
#define DEVKIND(Drawable) ((PixmapPtr)Drawable)->devKind

extern unsigned long *hpGetPlane();

/* tcCopyPlane -- entry for the CopyPlane Request
 *  The idea is to special case CopyPlane from depth 1 pixmaps to the
 *  screen.  This operation looks much like a PutImage, so we've stolen
 *  much of the tcPutImage code.
 */
RegionPtr 
tcCopyPlane(pSrcDrawable, pDstDrawable, pGC, xIn, yIn, widthSrc, heightSrc, 
	    xOut, yOut, bitPlane)
    DrawablePtr         pSrcDrawable, pDstDrawable;
    GCPtr               pGC;
    int                 xIn, yIn, widthSrc, heightSrc, xOut, yOut;
    unsigned long       bitPlane;
{
    TOPCAT *hardware = getTcHardware(pDstDrawable->pScreen);
    unsigned long screenPlanes = getPlanesMask(pDstDrawable->pScreen);
    unsigned long planeMask = pGC->planemask & screenPlanes;
    unsigned int fore = pGC->fgPixel;
    unsigned int back = pGC->bgPixel;
    BoxRec *scl, *dcl, sbox;
    RegionPtr prgnExposed = NULL;
      
    int srcx,srcy, dstx,dsty, width,height, dboxes,sboxes, stx,sty, dtx,dty,
      lowlife = 0, dst_stride, src_stride, the_real_src_stride,
      j, x1,y1, x2,y2, free_plane,
      w1,h1, w2,h2, a,b,c,d, tx,ty;
    RegionPtr sHitList = NULL;	/* source hit list */
    unsigned char *presrc, *predst, *src, *dst;
    register int i;
    unsigned int garbageBits, bits;
    unsigned int leadingBits, trailingBits, wholeBytes;
    CARD8 *psrc, *psrcLine, *pdstLine, *pdst;
    unsigned int alu;
    register BoxPtr btr;
    CARD8 srcByte;
    unsigned int rows;
    unsigned int pixels;
    static unsigned char masks[8] = { 0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 
				     0x40, 0x80 };
    static unsigned char trailMasks[8] = { 0x80, 0x40, 0x20, 0x10, 0x8, 0x4, 
				     0x2, 0x1 };

    if ((pDstDrawable->type == DRAWABLE_WINDOW) &&
        (!((WindowPtr)pDstDrawable)->realized))
	return NULL;
    
    if ((pDstDrawable->type == DRAWABLE_PIXMAP) &&
	(((PixmapPtr)(pDstDrawable))->devKind == PIXMAP_HOST_MEMORY))
    {
	/*
	 * I don't want to handle
	 * all the replacement rules/planemasks for in-memory writes
	 */
	return cfbCopyPlane(pSrcDrawable, pDstDrawable, pGC, xIn, yIn, widthSrc, 
			    heightSrc, xOut, yOut, bitPlane);
    }

    dstx = xOut; dsty = yOut; width = widthSrc; height = heightSrc;
    /* clip the left and top edges of the source */
    if (xIn < 0)
    {
        width += xIn;
        srcx = 0;
    }
    else
	srcx = xIn;
    if (yIn < 0)
    {
        height += yIn;
        srcy = 0;
    }
    else
	srcy = yIn;

    /* 
     * lookup or create the source clip lists 
     */
    stx = sty = 0;
    if (pSrcDrawable->type == DRAWABLE_PIXMAP)
    {
          /* clip right and bottom edges of source */
	if (width > pSrcDrawable->width)
	    width = pSrcDrawable->width;
	if (height > pSrcDrawable->height)
	    height = pSrcDrawable->height;
	if (DEVKIND(pSrcDrawable) == PIXMAP_FRAME_BUFFER)
	{
	    stx = PIXER(pSrcDrawable)->pChunk->x;
	    sty = PIXER(pSrcDrawable)->pChunk->y;
	}
	sbox.x2 = (sbox.x1 = srcx) + width;
	sbox.y2 = (sbox.y1 = srcy) + height;
	scl = &sbox; sboxes = 1;
    }
    else  /* source is a window */
    {
          /* translate window to screen coordinates */
	srcx += pSrcDrawable->x;
	srcy += pSrcDrawable->y;
	if (pGC->subWindowMode == IncludeInferiors)
	{
	    /* included window can write over parent => overlap problem
	     *  (if included window is source and parent is dest)
	     */
	    if (pDstDrawable->type == DRAWABLE_WINDOW) lowlife = 1;
	    if (pSrcDrawable == pDstDrawable && pGC->clientClipType == CT_NONE)
	    {
		scl = REGION_RECTS(((cfbPrivGC *)
				    pGC->devPrivates[cfbGCPrivateIndex].ptr)->pCompositeClip);
		sboxes = REGION_NUM_RECTS(((cfbPrivGC *)
					   pGC->devPrivates[cfbGCPrivateIndex].ptr)->pCompositeClip);
	    }
	    else      /* gotta create a new clip list */
	    {
		sHitList = NotClippedByChildren((WindowPtr)pSrcDrawable);
		scl = REGION_RECTS(sHitList);
		sboxes = REGION_NUM_RECTS(sHitList);
	    }
	}
	else
	{
	    scl    = REGION_RECTS(&((WindowPtr)pSrcDrawable)->clipList);
	    sboxes = REGION_NUM_RECTS(&((WindowPtr)pSrcDrawable)->clipList);
	}
    }

    /* 
     * lookup the dest clip list and any translation 
     */
    dcl = REGION_RECTS(((cfbPrivGC *)
			pGC->devPrivates[cfbGCPrivateIndex].ptr)->pCompositeClip);
    dboxes = REGION_NUM_RECTS(((cfbPrivGC *)
			       pGC->devPrivates[cfbGCPrivateIndex].ptr)->pCompositeClip);
    dtx = dty = 0;
    if (pDstDrawable->type == DRAWABLE_PIXMAP)
    {
	if (DEVKIND(pDstDrawable) == PIXMAP_FRAME_BUFFER)
	{
	    dtx = PIXER(pDstDrawable)->pChunk->x;
	    dty = PIXER(pDstDrawable)->pChunk->y;
	}
	dst_stride = PIXER(pDstDrawable)->stride;
	predst = PIXER(pDstDrawable)->bits;
    }
    else  /* dest is a window */
    {
	if (pGC->miTranslate) /* translate window to screen coordinates */
	{
	    dstx += pDstDrawable->x;
	    dsty += pDstDrawable->y;
	}
	dst_stride = SCRMER(pDstDrawable)->stride;
	predst = SCRMER(pDstDrawable)->bits;
    }

    if (pSrcDrawable->type == DRAWABLE_WINDOW)
    {
	src_stride = SCRMER(pSrcDrawable)->stride;
	presrc = SCRMER(pSrcDrawable)->bits;
    }
    else
    {
	src_stride = PIXER(pSrcDrawable)->stride;
	presrc = PIXER(pSrcDrawable)->bits;
    }
    the_real_src_stride = src_stride;
    tx = srcx - dstx;
    ty = srcy - dsty;

    alu = pGC->alu;

    /*
     * Set up the registers so that writing 0xff will get us the
     * foreground and writing 0x00 will get the background with
     * the proper replacement rule and plane enable
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

    for (; sboxes--; scl++)
    { /* for each source box */
	/* intersect src and src clip rectangle */
	clipper(srcx,srcy,width,height, scl->x1,scl->y1, scl->x2,scl->y2,
		&x1,&y1, &w1,&h1);
	if (w1 <= 0 || h1 <= 0) continue;
	a = x1 -tx; b = y1 -ty; c = a +w1; d = b +h1;
	for (j = dboxes, btr = dcl; j--; btr++)
	{
	    free_plane = FALSE;
	    /* intersect dst and dst clip rectangles */
	    clipper(dstx,dsty,width,height, btr->x1,btr->y1, btr->x2,btr->y2,
		    &x2,&y2, &w2,&h2);
	    if (w2 <= 0 || h2 <= 0) continue;

	    /* intersect clipped src and clipped dst rectangles */
	    clipper(x2,y2,w2,h2, a,b,c,d, &x1,&y1, &w1,&h1);
	    if (w1 <= 0 || h1 <= 0) continue;

	    dst = predst + x1 + y1 * dst_stride;

	    if (pSrcDrawable->depth != 1)
	    {
		/*
		 * need to create a bitmap replica for the rest
		 * of this code to use
		 */
		src = presrc + x1+tx + (y1+ty)*the_real_src_stride;
		src = (unsigned char *)
		    hpGetPlane(pSrcDrawable, ffs(bitPlane) - 1,
			       src, w1, h1, the_real_src_stride, NULL);
		src_stride = PixmapBytePad(w1, 1);
		leadingBits = garbageBits = 0;
		free_plane = TRUE;
	    }
	    else
	    {
		src = presrc + src_stride * (y1 + ty) + (x1 + tx) / 8;
		garbageBits = (x1 + tx) % 8;
		leadingBits = 8 - garbageBits;
		if (leadingBits == 8) leadingBits = 0;
		if (leadingBits > w1) leadingBits = w1;
	    }

	    wholeBytes = (w1 - leadingBits) / 8;
	    trailingBits = w1 - (wholeBytes * 8 + leadingBits);

	    for (psrcLine = src, rows = h1, 
		 pixels = w1, pdstLine = dst; 
		 rows--;
		 psrcLine += src_stride, 
		 pdstLine += dst_stride)
	    {
		psrc = psrcLine;
		pdst = pdstLine;

		/*
		 * write any leading bits one at a time until we're at a whole byte
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
		     * so we have to write one byte at a time
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
	    /* free plane allocated by hpGetPlane */
	    if (free_plane) xfree((int *)src);
	}
    }
    SET_REGISTERS_FOR_WRITING(pDstDrawable->pScreen, 0xff, alu);

    if (pGC->graphicsExposures)
        prgnExposed = miHandleExposures(pSrcDrawable, pDstDrawable, pGC,
					xIn, yIn, widthSrc, heightSrc,
					xOut, yOut, bitPlane);
    if(sHitList) (*pGC->pScreen->RegionDestroy)(sHitList);
    return prgnExposed;
}

