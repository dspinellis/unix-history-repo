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

/* $XConsortium: cfbgc.c,v 5.32.1.1 90/03/21 10:16:47 rws Exp $ */

#include "X.h"
#include "Xmd.h"
#include "Xproto.h"
#include "../cfb/cfb.h"
#include "fontstruct.h"
#include "dixfontstr.h"
#include "gcstruct.h"
#include "windowstr.h"
#include "pixmapstr.h"
#include "scrnintstr.h"
#include "region.h"

#include "topcat.h"
#include "mistruct.h"
#include "mibstore.h"

#include "../cfb/cfbmskbits.h"
#include "../cfb/cfb8bit.h"

extern RegionPtr hpcCopyArea();

static void topcatValidateGC(), cfbChangeGC(), cfbCopyGC(), cfbDestroyGC();
static void cfbChangeClip(), cfbDestroyClip(), cfbCopyClip();
static cfbDestroyOps();

static GCFuncs cfbGCFuncs = {
    topcatValidateGC,
    cfbChangeGC,
    cfbCopyGC,
    cfbDestroyGC,
    cfbChangeClip,
    cfbDestroyClip,
    cfbCopyClip,
};

#define tcImageGlyphBlt cfbImageGlyphBlt8

static GCOps	cfbTEOps1Rect = {
    cfbSolidSpansCopy,
    cfbSetSpans,
    tcPutImage,
    hpcCopyArea,
    tcCopyPlane,
    cfbPolyPoint,
#if PPW == 4
    cfb8LineSS1Rect,
    cfb8SegmentSS1Rect,
#else
    cfbLineSS,
    cfbSegmentSS,
#endif
    miPolyRectangle,
#if PPW == 4
    cfbZeroPolyArcSS8Copy,
#else
    miZeroPolyArc,
#endif
    cfbFillPoly1RectCopy,
    tcPolyFillRect,
    cfbPolyFillArcSolidCopy,
    miPolyText8,
    miPolyText16,
    miImageText8,
    miImageText16,
#if PPW == 4
    cfbTEGlyphBlt8,
    cfbPolyGlyphBlt8,
    cfbPushPixels8,
#else
    cfbTEGlyphBlt,
    miPolyGlyphBlt,
    mfbPushPixels,
#endif
    NULL,
};

static GCOps	cfbTEOps = {
    cfbSolidSpansCopy,
    cfbSetSpans,
    tcPutImage,
    hpcCopyArea,
    tcCopyPlane,
    cfbPolyPoint,
    cfbLineSS,
    cfbSegmentSS,
    miPolyRectangle,
#if PPW == 4
    cfbZeroPolyArcSS8Copy,
#else
    miZeroPolyArc,
#endif
    miFillPolygon,
    tcPolyFillRect,
    cfbPolyFillArcSolidCopy,
    miPolyText8,
    miPolyText16,
    miImageText8,
    miImageText16,
#if PPW == 4
    cfbTEGlyphBlt8,
    cfbPolyGlyphBlt8,
    cfbPushPixels8,
#else
    cfbTEGlyphBlt,
    miPolyGlyphBlt,
    mfbPushPixels,
#endif
    NULL,
};

static GCOps	cfbNonTEOps1Rect = {
    cfbSolidSpansCopy,
    cfbSetSpans,
    tcPutImage,
    hpcCopyArea,
    tcCopyPlane,
    cfbPolyPoint,
#if PPW == 4
    cfb8LineSS1Rect,
    cfb8SegmentSS1Rect,
#else
    cfbLineSS,
    cfbSegmentSS,
#endif
    miPolyRectangle,
#if PPW == 4
    cfbZeroPolyArcSS8Copy,
#else
    miZeroPolyArc,
#endif
    cfbFillPoly1RectCopy,
    tcPolyFillRect,
    cfbPolyFillArcSolidCopy,
    miPolyText8,
    miPolyText16,
    miImageText8,
    miImageText16,
#if PPW == 4
    cfbImageGlyphBlt8,
    cfbPolyGlyphBlt8,
    cfbPushPixels8,
#else
    miImageGlyphBlt,
    miPolyGlyphBlt,
    mfbPushPixels,
#endif
    NULL,
};

static GCOps	cfbNonTEOps = {
    cfbSolidSpansCopy,
    cfbSetSpans,
    tcPutImage,
    hpcCopyArea,
    tcCopyPlane,
    cfbPolyPoint,
    cfbLineSS,
    cfbSegmentSS,
    miPolyRectangle,
#if PPW == 4
    cfbZeroPolyArcSS8Copy,
#else
    miZeroPolyArc,
#endif
    miFillPolygon,
    tcPolyFillRect,
    cfbPolyFillArcSolidCopy,
    miPolyText8,
    miPolyText16,
    miImageText8,
    miImageText16,
#if PPW == 4
    cfbImageGlyphBlt8,
    cfbPolyGlyphBlt8,
    cfbPushPixels8,
#else
    miImageGlyphBlt,
    miPolyGlyphBlt,
    mfbPushPixels,
#endif
    NULL,
};

static GCOps	tcWinOps1Rect = {
    tcSolidFS,
    cfbSetSpans,
    tcPutImage,
    hpcCopyArea,
    tcCopyPlane,
    cfbPolyPoint,
#if PPW == 4
    cfb8LineSS1Rect,
    cfb8SegmentSS1Rect,
#else
    cfbLineSS,
    cfbSegmentSS,
#endif
    miPolyRectangle,
    cfbZeroPolyArcSS8Copy,
    cfbFillPoly1RectCopy,
    tcPolyFillRect,
    cfbPolyFillArcSolidCopy,
    miPolyText8,
    miPolyText16,
    miImageText8,
    miImageText16,
    tcImageGlyphBlt,
    cfbPolyGlyphBlt8,
    cfbPushPixels8,
    NULL,
};

static GCOps	tcWinOps = {
    tcSolidFS,
    cfbSetSpans,
    tcPutImage,
    hpcCopyArea,
    tcCopyPlane,
    cfbPolyPoint,
    cfbLineSS,
    cfbSegmentSS,
    miPolyRectangle,
    cfbZeroPolyArcSS8Copy,
    miFillPolygon,
    tcPolyFillRect,
    cfbPolyFillArcSolidCopy,
    miPolyText8,
    miPolyText16,
    miImageText8,
    miImageText16,
    tcImageGlyphBlt,
    cfbPolyGlyphBlt8,
    cfbPushPixels8,
    NULL,
};

static GCOps	tcWinOTEOps1Rect = {
    tcSolidFS,
    cfbSetSpans,
    tcPutImage,
    hpcCopyArea,
    tcCopyPlane,
    cfbPolyPoint,
#if PPW == 4
    cfb8LineSS1Rect,
    cfb8SegmentSS1Rect,
#else
    cfbLineSS,
    cfbSegmentSS,
#endif
    miPolyRectangle,
    cfbZeroPolyArcSS8Copy,
    cfbFillPoly1RectCopy,
    tcPolyFillRect,
    cfbPolyFillArcSolidCopy,
    tcPolyOptText8,
    miPolyText16,
    tcImageOptTEText8,
    miImageText16,
    tcImageGlyphBlt,
    cfbPolyGlyphBlt8,
    cfbPushPixels8,
    NULL,
};

static GCOps	tcWinOTEOps = {
    tcSolidFS,
    cfbSetSpans,
    tcPutImage,
    hpcCopyArea,
    tcCopyPlane,
    cfbPolyPoint,
    cfbLineSS,
    cfbSegmentSS,
    miPolyRectangle,
    cfbZeroPolyArcSS8Copy,
    miFillPolygon,
    tcPolyFillRect,
    cfbPolyFillArcSolidCopy,
    tcPolyOptText8,
    miPolyText16,
    tcImageOptTEText8,
    miImageText16,
    tcImageGlyphBlt,
    cfbPolyGlyphBlt8,
    cfbPushPixels8,
    NULL,
};

static GCOps	tcWinONonTEOps1Rect = {
    tcSolidFS,
    cfbSetSpans,
    tcPutImage,
    hpcCopyArea,
    tcCopyPlane,
    cfbPolyPoint,
#if PPW == 4
    cfb8LineSS1Rect,
    cfb8SegmentSS1Rect,
#else
    cfbLineSS,
    cfbSegmentSS,
#endif
    miPolyRectangle,
    cfbZeroPolyArcSS8Copy,
    cfbFillPoly1RectCopy,
    tcPolyFillRect,
    cfbPolyFillArcSolidCopy,
    tcPolyOptText8,
    miPolyText16,
    tcImageOptText8,
    miImageText16,
    tcImageGlyphBlt,
    cfbPolyGlyphBlt8,
    cfbPushPixels8,
    NULL,
};

static GCOps	tcWinONonTEOps = {
    tcSolidFS,
    cfbSetSpans,
    tcPutImage,
    hpcCopyArea,
    tcCopyPlane,
    cfbPolyPoint,
    cfbLineSS,
    cfbSegmentSS,
    miPolyRectangle,
    cfbZeroPolyArcSS8Copy,
    miFillPolygon,
    tcPolyFillRect,
    cfbPolyFillArcSolidCopy,
    tcPolyOptText8,
    miPolyText16,
    tcImageOptText8,
    miImageText16,
    tcImageGlyphBlt,
    cfbPolyGlyphBlt8,
    cfbPushPixels8,
    NULL,
};


static GCOps *
tcMatchCommon (pGC, devPriv, pWin)
     GCPtr   pGC;
     cfbPrivGCPtr devPriv;
     WindowPtr pWin;
{
    if (pGC->lineWidth != 0)
	return 0;
    if (pGC->lineStyle != LineSolid)
	return 0;
    if (pGC->fillStyle != FillSolid)
	return 0;
    if (devPriv->rop != GXcopy)
	return 0;
    if (!pWin)
    {
	if (pGC->font &&
	    FONTMAXBOUNDS(pGC->font,rightSideBearing) -
	    FONTMINBOUNDS(pGC->font,leftSideBearing) <= 32 &&
	    FONTMINBOUNDS(pGC->font,characterWidth) >= 0)
	{
	    if (TERMINALFONT(pGC->font)
#if PPW == 4
		&& FONTMAXBOUNDS(pGC->font,characterWidth) >= 4
#endif
		)
		if (devPriv->oneRect)
		    return &cfbTEOps1Rect;
		else
		    return &cfbTEOps;
	    else
		if (devPriv->oneRect)
		    return &cfbNonTEOps1Rect;
		else
		    return &cfbNonTEOps;
	}
    }
    else
    {
#if 0			/* XXXX */
	if (pGC->font && pGC->font->devPriv[pGC->pScreen->myNum])
	{
	    if (TERMINALFONT(pGC->font))
		if (devPriv->oneRect)
		    return &tcWinOTEOps1Rect;
		else
		    return &tcWinOTEOps;
	    else
		if (devPriv->oneRect)
		    return &tcWinONonTEOps1Rect;
		else
		    return &tcWinONonTEOps;
	}
#endif
	if (devPriv->oneRect)
	    return &tcWinOps1Rect;
	else
	    return &tcWinOps;
    }
    return 0;
}

Bool
topcatCreateGC(pGC)
    register GCPtr pGC;
{
    cfbPrivGC  *pPriv;

    if (pGC->depth == 1)
	return mfbCreateGC(pGC);
    pGC->clientClip = NULL;
    pGC->clientClipType = CT_NONE;

    /*
     * some of the output primitives aren't really necessary, since they
     * will be filled in ValidateGC because of dix/CreateGC() setting all
     * the change bits.  Others are necessary because although they depend
     * on being a color frame buffer, they don't change 
     */

    pGC->ops = &cfbNonTEOps;
    pGC->funcs = &cfbGCFuncs;

    /* cfb wants to translate before scan conversion */
    pGC->miTranslate = 1;

    pPriv = (cfbPrivGC *)(pGC->devPrivates[cfbGCPrivateIndex].ptr);
    pPriv->rop = pGC->alu;
    pPriv->oneRect = FALSE;
    pPriv->fExpose = TRUE;
    pPriv->freeCompClip = FALSE;
    pPriv->pRotatedPixmap = (PixmapPtr) NULL;
    return TRUE;
}

/*ARGSUSED*/
static void
cfbChangeGC(pGC, mask)
    GC		    *pGC;
    BITS32	    mask;
{
    return;
}

static void
cfbDestroyGC(pGC)
    GC 			*pGC;
{
    cfbPrivGC *pPriv;

    pPriv = (cfbPrivGC *)(pGC->devPrivates[cfbGCPrivateIndex].ptr);
    if (pPriv->pRotatedPixmap)
	cfbDestroyPixmap(pPriv->pRotatedPixmap);
    if (pPriv->freeCompClip)
	(*pGC->pScreen->RegionDestroy)(pPriv->pCompositeClip);
    cfbDestroyOps (pGC->ops);
}

/*
 * create a private op array for a gc
 */

static GCOps *
cfbCreateOps (prototype)
    GCOps	*prototype;
{
    GCOps	*ret;
    extern Bool	Must_have_memory;

    /* XXX */ Must_have_memory = TRUE;
    ret = (GCOps *) xalloc (sizeof(GCOps));
    /* XXX */ Must_have_memory = FALSE;
    if (!ret)
	return 0;
    *ret = *prototype;
    ret->devPrivate.val = 1;
    return ret;
}

static
cfbDestroyOps (ops)
    GCOps   *ops;
{
    if (ops->devPrivate.val)
	xfree (ops);
}

/* Clipping conventions
	if the drawable is a window
	    CT_REGION ==> pCompositeClip really is the composite
	    CT_other ==> pCompositeClip is the window clip region
	if the drawable is a pixmap
	    CT_REGION ==> pCompositeClip is the translated client region
		clipped to the pixmap boundary
	    CT_other ==> pCompositeClip is the pixmap bounding box
*/

static void
topcatValidateGC(pGC, changes, pDrawable)
    register GCPtr  pGC;
    Mask	    changes;
    DrawablePtr	    pDrawable;
{
    WindowPtr   pWin;
    int         mask;		/* stateChanges */
    int         index;		/* used for stepping through bitfields */
    int		new_rrop;
    int         new_line, new_text, new_fillspans, new_fillarea;
    int		new_rotate;
    int		xrot, yrot;
    /* flags for changing the proc vector */
    cfbPrivGCPtr devPriv;
    int		oneRect;

    new_rotate = pGC->lastWinOrg.x != pDrawable->x ||
		 pGC->lastWinOrg.y != pDrawable->y;

    pGC->lastWinOrg.x = pDrawable->x;
    pGC->lastWinOrg.y = pDrawable->y;
    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	pWin = (WindowPtr) pDrawable;
    }
    else
    {
	pWin = (WindowPtr) NULL;
    }

    devPriv = ((cfbPrivGCPtr) (pGC->devPrivates[cfbGCPrivateIndex].ptr));

    new_rrop = FALSE;
    new_line = FALSE;
    new_text = FALSE;
    new_fillspans = FALSE;
    new_fillarea = FALSE;

    /*
     * if the client clip is different or moved OR the subwindowMode has
     * changed OR the window's clip has changed since the last validation
     * we need to recompute the composite clip 
     */

    if ((changes & (GCClipXOrigin|GCClipYOrigin|GCClipMask|GCSubwindowMode)) ||
	(pDrawable->serialNumber != (pGC->serialNumber & DRAWABLE_SERIAL_BITS))
	)
    {
	ScreenPtr pScreen = pGC->pScreen;

	if (pWin) {
	    RegionPtr   pregWin;
	    Bool        freeTmpClip, freeCompClip;

	    if (pGC->subWindowMode == IncludeInferiors) {
		pregWin = NotClippedByChildren(pWin);
		freeTmpClip = TRUE;
	    }
	    else {
		pregWin = &pWin->clipList;
		freeTmpClip = FALSE;
	    }
	    freeCompClip = devPriv->freeCompClip;

	    /*
	     * if there is no client clip, we can get by with just keeping
	     * the pointer we got, and remembering whether or not should
	     * destroy (or maybe re-use) it later.  this way, we avoid
	     * unnecessary copying of regions.  (this wins especially if
	     * many clients clip by children and have no client clip.) 
	     */
	    if (pGC->clientClipType == CT_NONE) {
		if (freeCompClip)
		    (*pScreen->RegionDestroy) (devPriv->pCompositeClip);
		devPriv->pCompositeClip = pregWin;
		devPriv->freeCompClip = freeTmpClip;
	    }
	    else {
		/*
		 * we need one 'real' region to put into the composite
		 * clip. if pregWin the current composite clip are real,
		 * we can get rid of one. if pregWin is real and the
		 * current composite clip isn't, use pregWin for the
		 * composite clip. if the current composite clip is real
		 * and pregWin isn't, use the current composite clip. if
		 * neither is real, create a new region. 
		 */

		(*pScreen->TranslateRegion)(pGC->clientClip,
					    pDrawable->x + pGC->clipOrg.x,
					    pDrawable->y + pGC->clipOrg.y);
						  
		if (freeCompClip)
		{
		    (*pGC->pScreen->Intersect)(devPriv->pCompositeClip,
					       pregWin, pGC->clientClip);
		    if (freeTmpClip)
			(*pScreen->RegionDestroy)(pregWin);
		}
		else if (freeTmpClip)
		{
		    (*pScreen->Intersect)(pregWin, pregWin, pGC->clientClip);
		    devPriv->pCompositeClip = pregWin;
		}
		else
		{
		    devPriv->pCompositeClip = (*pScreen->RegionCreate)(NullBox,
								       0);
		    (*pScreen->Intersect)(devPriv->pCompositeClip,
					  pregWin, pGC->clientClip);
		}
		devPriv->freeCompClip = TRUE;
		(*pScreen->TranslateRegion)(pGC->clientClip,
					    -(pDrawable->x + pGC->clipOrg.x),
					    -(pDrawable->y + pGC->clipOrg.y));
						  
	    }
	}			/* end of composite clip for a window */
	else {
	    BoxRec      pixbounds;

	    /* XXX should we translate by drawable.x/y here ? */
	    pixbounds.x1 = 0;
	    pixbounds.y1 = 0;
	    pixbounds.x2 = pDrawable->width;
	    pixbounds.y2 = pDrawable->height;

	    if (devPriv->freeCompClip)
		(*pScreen->RegionReset)(devPriv->pCompositeClip, &pixbounds);
	    else {
		devPriv->freeCompClip = TRUE;
		devPriv->pCompositeClip = (*pScreen->RegionCreate)(&pixbounds,
								   1);
	    }

	    if (pGC->clientClipType == CT_REGION)
	    {
		(*pScreen->TranslateRegion)(devPriv->pCompositeClip,
					    -pGC->clipOrg.x, -pGC->clipOrg.y);
		(*pScreen->Intersect)(devPriv->pCompositeClip,
				      devPriv->pCompositeClip,
				      pGC->clientClip);
		(*pScreen->TranslateRegion)(devPriv->pCompositeClip,
					    pGC->clipOrg.x, pGC->clipOrg.y);
	    }
	}			/* end of composute clip for pixmap */
	oneRect = REGION_NUM_RECTS(devPriv->pCompositeClip) == 1;
	if (oneRect != devPriv->oneRect)
	    new_line = TRUE;
	devPriv->oneRect = oneRect;
    }

    mask = changes;
    while (mask) {
	index = lowbit (mask);
	mask &= ~index;

	/*
	 * this switch acculmulates a list of which procedures might have
	 * to change due to changes in the GC.  in some cases (e.g.
	 * changing one 16 bit tile for another) we might not really need
	 * a change, but the code is being paranoid. this sort of batching
	 * wins if, for example, the alu and the font have been changed,
	 * or any other pair of items that both change the same thing. 
	 */
	switch (index) {
	case GCFunction:
	case GCForeground:
	    new_rrop = TRUE;
	    break;
	case GCPlaneMask:
	    new_rrop = TRUE;
	    new_text = TRUE;
	    break;
	case GCBackground:
	    break;
	case GCLineStyle:
	case GCLineWidth:
	    new_line = TRUE;
	    break;
	case GCJoinStyle:
	case GCCapStyle:
	    break;
	case GCFillStyle:
	    new_text = TRUE;
	    new_fillspans = TRUE;
	    new_line = TRUE;
	    new_fillarea = TRUE;
	    break;
	case GCFillRule:
	    break;
	case GCTile:
	    new_fillspans = TRUE;
	    new_fillarea = TRUE;
	    break;

	case GCStipple:
	    if (pGC->stipple)
	    {
		int width = pGC->stipple->drawable.width;
		PixmapPtr nstipple;

		if ((width <= 32) && !(width & (width - 1)) &&
		    (nstipple = cfbCopyPixmap(pGC->stipple)))
		{
		    cfbPadPixmap(nstipple);
		    cfbDestroyPixmap(pGC->stipple);
		    pGC->stipple = nstipple;
		}
	    }
	    new_fillspans = TRUE;
	    new_fillarea = TRUE;
	    break;

	case GCTileStipXOrigin:
	    new_rotate = TRUE;
	    break;

	case GCTileStipYOrigin:
	    new_rotate = TRUE;
	    break;

	case GCFont:
	    new_text = TRUE;
	    break;
	case GCSubwindowMode:
	    break;
	case GCGraphicsExposures:
	    break;
	case GCClipXOrigin:
	    break;
	case GCClipYOrigin:
	    break;
	case GCClipMask:
	    break;
	case GCDashOffset:
	    break;
	case GCDashList:
	    break;
	case GCArcMode:
	    break;
	default:
	    break;
	}
    }

    /*
     * If the drawable has changed,  ensure suitable
     * entries are in the proc vector. 
     */
    if (pDrawable->serialNumber != (pGC->serialNumber & (DRAWABLE_SERIAL_BITS))) {
	new_fillspans = TRUE;	/* deal with FillSpans later */
    }

    if (new_rotate || new_fillspans)
    {
	Bool new_pix = FALSE;

	xrot = pGC->patOrg.x + pDrawable->x;
	yrot = pGC->patOrg.y + pDrawable->y;

	switch (pGC->fillStyle)
	{
	case FillTiled:
	    if (!pGC->tileIsPixel)
	    {
		int width = pGC->tile.pixmap->drawable.width * PSZ;

		if ((width <= 32) && !(width & (width - 1)))
		{
		    cfbCopyRotatePixmap(pGC->tile.pixmap,
					&devPriv->pRotatedPixmap,
					xrot, yrot);
		    new_pix = TRUE;
		}
	    }
	    break;
#if (PPW == 4)
	case FillStippled:
	case FillOpaqueStippled:
	    {
		int width = pGC->stipple->drawable.width;

		if ((width <= 32) && !(width & (width - 1)))
		{
		    mfbCopyRotatePixmap(pGC->stipple,
					&devPriv->pRotatedPixmap, xrot, yrot);
		    new_pix = TRUE;
		}
	    }
	    break;
#endif
	}
	if (!new_pix && devPriv->pRotatedPixmap)
	{
	    cfbDestroyPixmap(devPriv->pRotatedPixmap);
	    devPriv->pRotatedPixmap = (PixmapPtr) NULL;
	}
    }

    if (new_rrop)
    {
	int old_rrop;

	old_rrop = devPriv->rop;
	devPriv->rop = cfbReduceRasterOp (pGC->alu, pGC->fgPixel,
					  pGC->planemask,
					  &devPriv->and, &devPriv->xor);
	if (old_rrop == devPriv->rop)
	    new_rrop = FALSE;
	else
	{
#if PPW ==  4
	    new_line = TRUE;
	    new_text = TRUE;
#endif
	    new_fillspans = TRUE;
	    new_fillarea = TRUE;
	}
    }

    if (new_rrop || new_fillspans || new_text || new_fillarea || new_line)
    {
	GCOps	*newops;

	if (newops = tcMatchCommon (pGC, devPriv, pWin))
 	{
	    if (pGC->ops->devPrivate.val)
		cfbDestroyOps (pGC->ops);
	    pGC->ops = newops;
	    new_rrop = new_line = new_fillspans = new_text = new_fillarea = 0;
	}
 	else
 	{
	    if (!pGC->ops->devPrivate.val)
	    {
		pGC->ops = cfbCreateOps (pGC->ops);
		pGC->ops->devPrivate.val = 1;
	    }
	}
    }

    /* deal with the changes we've collected */
    if (new_line)
    {
	pGC->ops->FillPolygon = miFillPolygon;
	if (devPriv->oneRect && pGC->fillStyle == FillSolid)
	{
	    switch (devPriv->rop) {
	      case GXcopy:
		pGC->ops->FillPolygon = cfbFillPoly1RectCopy;
		break;
	      default:
		pGC->ops->FillPolygon = cfbFillPoly1RectGeneral;
		break;
	    }
	}
	if (pGC->lineWidth == 0)
	{
#if PPW == 4
	    if ((pGC->lineStyle == LineSolid) && (pGC->fillStyle == FillSolid))
	    {
		switch (devPriv->rop)
		{
		  case GXxor:
		    pGC->ops->PolyArc = cfbZeroPolyArcSS8Xor;
		    break;
		  case GXcopy:
		    pGC->ops->PolyArc = cfbZeroPolyArcSS8Copy;
		    break;
		  default:
		    pGC->ops->PolyArc = cfbZeroPolyArcSS8General;
		    break;
		}
	    }
	    else
#endif
		pGC->ops->PolyArc = miZeroPolyArc;
	}
	else
	    pGC->ops->PolyArc = miPolyArc;
	pGC->ops->PolySegment = miPolySegment;
	switch (pGC->lineStyle)
	{
	case LineSolid:
	    if(pGC->lineWidth == 0)
	    {
		if (pGC->fillStyle == FillSolid)
		{
#if PPW == 4
		    if (devPriv->oneRect)
		    {
			pGC->ops->Polylines = cfb8LineSS1Rect;
			pGC->ops->PolySegment = cfb8SegmentSS1Rect;
		    } else
#endif
		    {
			pGC->ops->Polylines = cfbLineSS;
			pGC->ops->PolySegment = cfbSegmentSS;
		    }
		}
 		else
		    pGC->ops->Polylines = miZeroLine;
	    }
	    else
		pGC->ops->Polylines = miWideLine;
	    break;
	case LineOnOffDash:
	case LineDoubleDash:
	    if (pGC->lineWidth == 0 && pGC->fillStyle == FillSolid)
	    {
		pGC->ops->Polylines = cfbLineSD;
		pGC->ops->PolySegment = cfbSegmentSD;
	    } else
		pGC->ops->Polylines = miWideDash;
	    break;
	}
    }

    if (new_text && (pGC->font))
    {
	if (!pWin)
	{
	    if (FONTMAXBOUNDS(pGC->font,rightSideBearing) -
		FONTMINBOUNDS(pGC->font,leftSideBearing) > 32 ||
		FONTMINBOUNDS(pGC->font,characterWidth) < 0)
	    {
		pGC->ops->PolyGlyphBlt = miPolyGlyphBlt;
		pGC->ops->ImageGlyphBlt = miImageGlyphBlt;
	    }
	    else
	    {
#if PPW == 4
		if (pGC->fillStyle == FillSolid)
		{
		    if (devPriv->rop == GXcopy)
			pGC->ops->PolyGlyphBlt = cfbPolyGlyphBlt8;
		    else
			pGC->ops->PolyGlyphBlt = cfbPolyGlyphRop8;
		}
		else
#endif
		    pGC->ops->PolyGlyphBlt = miPolyGlyphBlt;
		/* special case ImageGlyphBlt for terminal emulator fonts */
		if (TERMINALFONT(pGC->font) &&
		    (pGC->planemask & PMSK) == PMSK
#if PPW == 4
		    && FONTMAXBOUNDS(pGC->font,characterWidth) >= 4
#endif
		    )
		{
#if PPW == 4
		    pGC->ops->ImageGlyphBlt = cfbTEGlyphBlt8;
#else
		    pGC->ops->ImageGlyphBlt = cfbTEGlyphBlt;
#endif
		}
		else
		{
#if PPW == 4
		    pGC->ops->ImageGlyphBlt = cfbImageGlyphBlt8;
#else
		    pGC->ops->ImageGlyphBlt = miImageGlyphBlt;
#endif
		}
	    }
	}
	else
	{
	    pGC->ops->ImageGlyphBlt = tcImageGlyphBlt;

	    if (FONTMAXBOUNDS(pGC->font,rightSideBearing) -
		FONTMINBOUNDS(pGC->font,leftSideBearing) > 32 ||
		FONTMINBOUNDS(pGC->font,characterWidth) < 0)
		pGC->ops->PolyGlyphBlt = miPolyGlyphBlt;
	    else
#if PPW == 4
		if (pGC->fillStyle == FillSolid)
		{
		    if (devPriv->rop == GXcopy)
			pGC->ops->PolyGlyphBlt = cfbPolyGlyphBlt8;
		    else
			pGC->ops->PolyGlyphBlt = cfbPolyGlyphRop8;
		}
		else
#endif
		    pGC->ops->PolyGlyphBlt = miPolyGlyphBlt;

#if 0
	    if (pGC->font->devPriv[pGC->pScreen->myNum])
	    {
		pGC->ops->PolyText8 = tcPolyOptText8;
		if (TERMINALFONT(pGC->font))
		    pGC->ops->ImageText8 = tcImageOptTEText8;
		else
		    pGC->ops->ImageText8 = tcImageOptText8;
	    }
	    else
#endif
	    {
		pGC->ops->PolyText8 = miPolyText8;
		pGC->ops->ImageText8 = miImageText8;
	    }
	}
    }    


    if (new_fillspans) {
	switch (pGC->fillStyle) {
	case FillSolid:
	    if (pDrawable->type == DRAWABLE_WINDOW)
		pGC->ops->FillSpans = tcSolidFS;
	    else
		switch (devPriv->rop) {
		  case GXcopy:
		    pGC->ops->FillSpans = cfbSolidSpansCopy;
		    break;
		  case GXxor:
		    pGC->ops->FillSpans = cfbSolidSpansXor;
		    break;
		  default:
		    pGC->ops->FillSpans = cfbSolidSpansGeneral;
		    break;
		}
	    break;
	case FillTiled:
#if 1 /* XXX */
	    if (devPriv->pRotatedPixmap)
	    {
		if (pGC->alu == GXcopy && (pGC->planemask & PMSK) == PMSK)
		    pGC->ops->FillSpans = cfbTile32FSCopy;
		else
		    pGC->ops->FillSpans = cfbTile32FSGeneral;
	    }
	    else
#endif
		pGC->ops->FillSpans = cfbUnnaturalTileFS;
	    break;
	case FillStippled:
#if PPW == 4
	    if (devPriv->pRotatedPixmap)
		pGC->ops->FillSpans = cfb8Stipple32FS;
	    else
#endif
		pGC->ops->FillSpans = cfbUnnaturalStippleFS;
	    break;
	case FillOpaqueStippled:
#if PPW == 4
	    if (devPriv->pRotatedPixmap)
		pGC->ops->FillSpans = cfb8OpaqueStipple32FS;
            else
#endif
		pGC->ops->FillSpans = cfbUnnaturalStippleFS;
	    break;
	default:
	    FatalError("cfbValidateGC: illegal fillStyle\n");
	}
    } /* end of new_fillspans */

    if (new_fillarea) {
#if PPW != 4
	pGC->ops->PolyFillRect = miPolyFillRect;
	if (pGC->fillStyle == FillSolid || pGC->fillStyle == FillTiled)
	{
	    pGC->ops->PolyFillRect = tcPolyFillRect;
	}
#endif
#if PPW == 4
	pGC->ops->PushPixels = mfbPushPixels;
	if (pGC->fillStyle == FillSolid && devPriv->rop == GXcopy)
	    pGC->ops->PushPixels = cfbPushPixels8;
#endif
	pGC->ops->PolyFillArc = miPolyFillArc;

	switch (pGC->fillStyle)
	{
	case FillSolid:
	    switch (devPriv->rop)
            {
	      case GXcopy:
                pGC->ops->PolyFillArc = cfbPolyFillArcSolidCopy;
		break;
	      default:
		pGC->ops->PolyFillArc = cfbPolyFillArcSolidGeneral;
		break;
            }
	    break;
#if (PPW == 4)
	case FillStippled:
	case FillOpaqueStippled:
	    pGC->ops->PolyFillRect = tcPolyFillStippledRect;
	    break;
#endif
	}
    }
}

static void
cfbDestroyClip(pGC)
    GCPtr	pGC;
{
    if(pGC->clientClipType == CT_NONE)
	return;
    else if (pGC->clientClipType == CT_PIXMAP)
    {
	cfbDestroyPixmap((PixmapPtr)(pGC->clientClip));
    }
    else
    {
	/* we know we'll never have a list of rectangles, since
	   ChangeClip immediately turns them into a region 
	*/
        (*pGC->pScreen->RegionDestroy)(pGC->clientClip);
    }
    pGC->clientClip = NULL;
    pGC->clientClipType = CT_NONE;
}

static void
cfbChangeClip(pGC, type, pvalue, nrects)
    GCPtr	pGC;
    int		type;
    pointer	pvalue;
    int		nrects;
{
    cfbDestroyClip(pGC);
    if(type == CT_PIXMAP)
    {
	pGC->clientClip = (pointer) (*pGC->pScreen->BitmapToRegion)((PixmapPtr)pvalue);
	(*pGC->pScreen->DestroyPixmap)(pvalue);
    }
    else if (type == CT_REGION) {
	/* stuff the region in the GC */
	pGC->clientClip = pvalue;
    }
    else if (type != CT_NONE)
    {
	pGC->clientClip = (pointer) (*pGC->pScreen->RectsToRegion)(nrects,
						    (xRectangle *)pvalue,
						    type);
	xfree(pvalue);
    }
    pGC->clientClipType = (type != CT_NONE && pGC->clientClip) ? CT_REGION :
								 CT_NONE;
    pGC->stateChanges |= GCClipMask;
}

static void
cfbCopyClip (pgcDst, pgcSrc)
    GCPtr pgcDst, pgcSrc;
{
    RegionPtr prgnNew;

    switch(pgcSrc->clientClipType)
    {
      case CT_PIXMAP:
	((PixmapPtr) pgcSrc->clientClip)->refcnt++;
	/* Fall through !! */
      case CT_NONE:
        cfbChangeClip(pgcDst, (int)pgcSrc->clientClipType, pgcSrc->clientClip,
		      0);
        break;
      case CT_REGION:
        prgnNew = (*pgcSrc->pScreen->RegionCreate)(NULL, 1);
        (*pgcSrc->pScreen->RegionCopy)(prgnNew,
                                       (RegionPtr)(pgcSrc->clientClip));
        cfbChangeClip(pgcDst, CT_REGION, (pointer)prgnNew, 0);
        break;
    }
}

/*ARGSUSED*/
static void
cfbCopyGC (pGCSrc, changes, pGCDst)
    GCPtr	pGCSrc;
    Mask 	changes;
    GCPtr	pGCDst;
{
    return;
}
