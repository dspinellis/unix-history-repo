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
/* $XConsortium: hpfb.c,v 1.7 88/09/06 15:18:15 jim Exp $ */
/* Author: Todd Newman  (aided and abetted by Mr. Drewry) */

#include "X.h"
#include "Xprotostr.h"

#include "misc.h"
#include "gcstruct.h"
#include "pixmapstr.h"
#include "windowstr.h"
#include "scrnintstr.h"
#include "regionstr.h"
#include "servermd.h"
#include "cfb.h"

/* DoBitblt() does multiple rectangle moves into the rectangles

   we have to cope with the direction on a per band basis,
rather than a per rectangle basis.  moving bottom to top
means we have to invert the order of the bands; moving right
to left requires reversing the order of the rectangles in
each band.

   if src or dst is a window, the points have already been
translated.
*/

void
hpfbDoBitblt(pSrcDrawable, pDstDrawable, alu, prgnDst, pptSrc, planemask)
     DrawablePtr pSrcDrawable;
     DrawablePtr pDstDrawable;
     int alu;
     RegionPtr prgnDst;
     DDXPointPtr pptSrc;
     unsigned long planemask;
{
    register BoxPtr pbox;
    int nbox;

    BoxPtr pboxTmp, pboxNext, pboxBase, pboxNewY, pboxNewX;
    /* temporaries for shuffling rectangles */
    DDXPointPtr pptTmp, pptNewY, pptNewX; /* shuffling boxes entails shuffling the
					   source points too */
    int w, h;

    void (*bitMover)();

    /* check whether we should use this routine... call cfb code if not. */

    if (((pSrcDrawable->type == DRAWABLE_PIXMAP) &&
	 (((PixmapPtr)(pSrcDrawable))->devKind != PIXMAP_FRAME_BUFFER)) &&
	(pSrcDrawable->type != DRAWABLE_WINDOW))
    {
	cfbDoBitblt(pSrcDrawable, pDstDrawable, alu, prgnDst, pptSrc, planemask);
	return;
    }

    if (((pDstDrawable->type == DRAWABLE_PIXMAP) &&
	 (((PixmapPtr)(pDstDrawable))->devKind != PIXMAP_FRAME_BUFFER)) &&
	(pDstDrawable->type != DRAWABLE_WINDOW))
    {
	cfbDoBitblt(pSrcDrawable, pDstDrawable, alu, prgnDst, pptSrc, planemask);
	return;
    }

    if (pSrcDrawable->pScreen != pDstDrawable->pScreen)
    {
	cfbDoBitblt(pSrcDrawable, pDstDrawable, alu, prgnDst, pptSrc, planemask);
	return;
    }

    pbox = REGION_RECTS(prgnDst);
    nbox = REGION_NUM_RECTS(prgnDst);

    pboxNewY = 0;
    pptNewY = 0;
    pboxNewX = 0;
    pptNewX = 0;
    if (pptSrc->y < pbox->y1) 
    {
        /* walk source botttom to top */
	if (nbox > 1)
	{
	    /* keep ordering in each band, reverse order of bands */
	    pboxNewY = (BoxPtr)ALLOCATE_LOCAL(sizeof(BoxRec) * nbox);
	    pptNewY = (DDXPointPtr)ALLOCATE_LOCAL(sizeof(DDXPointRec) * nbox);
	    if (!pboxNewY || !pptNewY)
	    {
	        DEALLOCATE_LOCAL(pptNewY);
	        DEALLOCATE_LOCAL(pboxNewY);
	        return;
	    }
	    pboxBase = pboxNext = pbox+nbox-1;
	    while (pboxBase >= pbox)
	    {
	        while ((pboxNext >= pbox) && 
		       (pboxBase->y1 == pboxNext->y1))
		    pboxNext--;
	        pboxTmp = pboxNext+1;
	        pptTmp = pptSrc + (pboxTmp - pbox);
	        while (pboxTmp <= pboxBase)
	        {
		    *pboxNewY++ = *pboxTmp++;
		    *pptNewY++ = *pptTmp++;
	        }
	        pboxBase = pboxNext;
	    }
	    pboxNewY -= nbox;
	    pbox = pboxNewY;
	    pptNewY -= nbox;
	    pptSrc = pptNewY;
        }
    }

    if (pptSrc->x < pbox->x1)
    {
	/* walk source right to left */

	if (nbox > 1)
	{
	    /* reverse order of rects in each band */
	    pboxNewX = (BoxPtr)ALLOCATE_LOCAL(sizeof(BoxRec) * nbox);
	    pptNewX = (DDXPointPtr)ALLOCATE_LOCAL(sizeof(DDXPointRec) * nbox);
	    if (!pboxNewX || !pptNewX)
	    {
	        DEALLOCATE_LOCAL(pptNewX);
	        DEALLOCATE_LOCAL(pboxNewX);
	        return;
	    }
	    pboxBase = pboxNext = pbox;
	    while (pboxBase < pbox+nbox)
	    {
	        while ((pboxNext < pbox+nbox) &&
		       (pboxNext->y1 == pboxBase->y1))
		    pboxNext++;
	        pboxTmp = pboxNext;
	        pptTmp = pptSrc + (pboxTmp - pbox);
	        while (pboxTmp != pboxBase)
	        {
		    *pboxNewX++ = *--pboxTmp;
		    *pptNewX++ = *--pptTmp;
	        }
	        pboxBase = pboxNext;
	    }
	    pboxNewX -= nbox;
	    pbox = pboxNewX;
	    pptNewX -= nbox;
	    pptSrc = pptNewX;
	}
    }

    bitMover = ((hpPrivScreenPtr)
		(pSrcDrawable->pScreen->devPrivate))->MoveBits;

    while (nbox--)
    {
	w = pbox->x2 - pbox->x1;
	h = pbox->y2 - pbox->y1;
	    
	(*bitMover)(pSrcDrawable->pScreen, ~0, alu,
		    pptSrc->x, pptSrc->y,
		    pbox->x1, pbox->y1,
		    w, h);

	pbox++;
	pptSrc++;
    }
    if (pptNewY) DEALLOCATE_LOCAL(pptNewY);
    if (pboxNewY) DEALLOCATE_LOCAL(pboxNewY);
    if (pptNewX) DEALLOCATE_LOCAL(pptNewX);
    if (pboxNewX) DEALLOCATE_LOCAL(pboxNewX);
}
