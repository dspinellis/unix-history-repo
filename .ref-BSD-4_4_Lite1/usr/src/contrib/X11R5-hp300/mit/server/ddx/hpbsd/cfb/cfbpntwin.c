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

#include "windowstr.h"
#include "regionstr.h"
#include "pixmapstr.h"
#include "scrnintstr.h"

#include "cfb.h"
#include "cfbmskbits.h"

static void cfbPaintArea32(), cfbPaintAreaSolid();

extern void miPaintWindow();

void
cfbPaintWindow(pWin, pRegion, what)
    WindowPtr	pWin;
    RegionPtr	pRegion;
    int		what;
{
    register cfbPrivWin	*pPrivWin;

    pPrivWin = (cfbPrivWin *)(pWin->devPrivates[cfbWindowPrivateIndex].ptr);

    switch (what) {
    case PW_BACKGROUND:
	switch (pWin->backgroundState) {
	case None:
	    return;
	case ParentRelative:
	    do {
		pWin = pWin->parent;
	    } while (pWin->backgroundState == ParentRelative);
	    (*pWin->drawable.pScreen->PaintWindowBackground)(pWin, pRegion,
							     what);
	    return;
	case BackgroundPixmap:
	    if (pPrivWin->fastBackground)
	    {
		cfbFillBoxTile32 ((DrawablePtr)pWin,
				  (int)REGION_NUM_RECTS(pRegion),
				  REGION_RECTS(pRegion),
				  pPrivWin->pRotatedBackground);
		return;
	    }
	    else
	    {
		cfbFillBoxTileOdd ((DrawablePtr)pWin,
				   (int)REGION_NUM_RECTS(pRegion),
				   REGION_RECTS(pRegion),
				   pWin->background.pixmap,
				   (int) pWin->drawable.x, (int) pWin->drawable.y);
		return;
	    }
	    break;
	case BackgroundPixel:
	    cfbFillBoxSolid ((DrawablePtr)pWin,
			     (int)REGION_NUM_RECTS(pRegion),
			     REGION_RECTS(pRegion),
			     pWin->background.pixel);
	    return;
    	}
    	break;
    case PW_BORDER:
	if (pWin->borderIsPixel)
	{
	    cfbFillBoxSolid ((DrawablePtr)pWin,
			     (int)REGION_NUM_RECTS(pRegion),
			     REGION_RECTS(pRegion),
			     pWin->border.pixel);
	    return;
	}
	else if (pPrivWin->fastBorder)
	{
	    cfbFillBoxTile32 ((DrawablePtr)pWin,
			      (int)REGION_NUM_RECTS(pRegion),
			      REGION_RECTS(pRegion),
			      pPrivWin->pRotatedBorder);
	    return;
	}
	else if (pWin->border.pixmap->drawable.width >= PPW/2)
	{
	    cfbFillBoxTileOdd ((DrawablePtr)pWin,
			       (int)REGION_NUM_RECTS(pRegion),
			       REGION_RECTS(pRegion),
			       pWin->border.pixmap,
			       (int) pWin->drawable.x, (int) pWin->drawable.y);
	    return;
	}
	break;
    }
    miPaintWindow (pWin, pRegion, what);
}

/*
 * Use the RROP macros in copy mode
 */

#define RROP GXcopy
#include "cfbrrop.h"

#ifdef RROP_UNROLL
# define Expand(left,right,leftAdjust) {\
    int part = nmiddle & RROP_UNROLL_MASK; \
    int widthStep; \
    widthStep = widthDst - nmiddle - leftAdjust; \
    nmiddle >>= RROP_UNROLL_SHIFT; \
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

#else
# define Expand(left, right, leftAdjust) { \
    int widthStep; \
    widthStep = widthDst - nmiddle - leftAdjust; \
    while (h--) { \
	left \
	m = nmiddle; \
	while (m--) {\
	    RROP_SOLID(pdst); \
	    pdst++; \
	} \
	right \
	pdst += widthStep; \
    } \
}
#endif

void
cfbFillBoxSolid (pDrawable, nBox, pBox, pixel)
    DrawablePtr	    pDrawable;
    int		    nBox;
    BoxPtr	    pBox;
    unsigned long   pixel;
{
    unsigned long   *pdstBase;
    int		    widthDst;
    register int    h;
    register unsigned long   rrop_xor;
    register unsigned long   *pdst;
    register unsigned long   leftMask, rightMask;
    int		    nmiddle;
    register int    m;
    int		    w;

    cfbGetLongWidthAndPointer(pDrawable, widthDst, pdstBase);

    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);

    rrop_xor = PFILL(pixel);
    for (; nBox; nBox--, pBox++)
    {
    	pdst = pdstBase + pBox->y1 * widthDst;
    	h = pBox->y2 - pBox->y1;
	w = pBox->x2 - pBox->x1;
#if PPW == 4
	if (w == 1)
	{
	    register char    *pdstb = ((char *) pdst) + pBox->x1;
	    int	    incr = widthDst << 2;

	    while (h--)
	    {
		*pdstb = rrop_xor;
		pdstb += incr;
	    }
	}
	else
	{
#endif
	pdst += (pBox->x1 >> PWSH);
	if ((pBox->x1 & PIM) + w <= PPW)
	{
	    maskpartialbits(pBox->x1, w, leftMask);
	    while (h--) {
		*pdst = (*pdst & ~leftMask) | (rrop_xor & leftMask);
		pdst += widthDst;
	    }
	}
	else
	{
	    maskbits (pBox->x1, w, leftMask, rightMask, nmiddle);
	    if (leftMask)
	    {
		if (rightMask)
		{
		    Expand (RROP_SOLID_MASK (pdst, leftMask); pdst++; ,
			    RROP_SOLID_MASK (pdst, rightMask); ,
			    1)
		}
		else
		{
		    Expand (RROP_SOLID_MASK (pdst, leftMask); pdst++;,
			    ;,
			    1)
		}
	    }
	    else
	    {
		if (rightMask)
		{
		    Expand (;,
			    RROP_SOLID_MASK (pdst, rightMask);,
			    0)
		}
		else
		{
		    Expand (;,
			    ;,
			    0)
		}
	    }
	}
#if PPW == 4
	}
#endif
    }
}

void
cfbFillBoxTile32 (pDrawable, nBox, pBox, tile)
    DrawablePtr	    pDrawable;
    int		    nBox;	/* number of boxes to fill */
    BoxPtr 	    pBox;	/* pointer to list of boxes to fill */
    PixmapPtr	    tile;	/* rotated, expanded tile */
{
    register unsigned long  rrop_xor;	
    register unsigned long  *pdst;
    register int	    m;
    int			    *psrc;
    int			    tileHeight;

    int			    widthDst;
    int			    widthSrc;
    int			    w;
    int			    h;
    register unsigned long  leftMask;
    register unsigned long  rightMask;
    int			    nmiddle;
    int			    y;
    int			    srcy;

    unsigned long	    *pdstBase;

    psrc = (int *)(((hpPrivPixmapPtr)tile->devPrivate.ptr)->bits);
    widthSrc = (int)(((hpPrivPixmapPtr)tile->devPrivate.ptr)->stride) >> 2;
    tileHeight = tile->drawable.height * widthSrc;

    cfbGetLongWidthAndPointer (pDrawable, widthDst, pdstBase);

    SET_REGISTERS_FOR_WRITING(pDrawable->pScreen, ~0, GXcopy);
    if (tile->devKind == PIXMAP_FRAME_BUFFER)
        WAIT_READY_TO_RENDER(tile->drawable.pScreen);

    while (nBox--)
    {
	w = pBox->x2 - pBox->x1;
	h = pBox->y2 - pBox->y1;
	y = pBox->y1;
	pdst = pdstBase + (pBox->y1 * widthDst) + (pBox->x1 >> PWSH);
	srcy = (y * widthSrc) % tileHeight;

#define StepTile    rrop_xor = psrc[srcy]; \
		    srcy += widthSrc; \
		    if (srcy == tileHeight) \
		        srcy = 0;

	if ( ((pBox->x1 & PIM) + w) < PPW)
	{
	    maskpartialbits(pBox->x1, w, leftMask);
	    rightMask = ~leftMask;
	    while (h--)
	    {
		StepTile
		*pdst = (*pdst & rightMask) | (rrop_xor & leftMask);
		pdst += widthDst;
	    }
	}
	else
	{
	    maskbits(pBox->x1, w, leftMask, rightMask, nmiddle);

	    if (leftMask)
	    {
		if (rightMask)
		{
		    Expand (StepTile
			    RROP_SOLID_MASK(pdst, leftMask); pdst++;,
			    RROP_SOLID_MASK(pdst, rightMask);,
			    1)
		}
		else
		{
		    Expand (StepTile
			    RROP_SOLID_MASK(pdst, leftMask); pdst++;,
			    ,
			    1)
		}
	    }
	    else
	    {
		if (rightMask)
		{
		    Expand (StepTile
			    ,
			    RROP_SOLID_MASK(pdst, rightMask);,
			    0)
		}
		else
		{
		    Expand (StepTile
			    ,
			    ,
			    0)
		}
	    }
	}
        pBox++;
    }
}
