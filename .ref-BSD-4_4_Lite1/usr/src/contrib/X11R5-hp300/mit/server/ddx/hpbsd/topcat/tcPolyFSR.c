	/* This file contains the same code as hpPolyFSR.c 'cept for
	 *   code in PaintBlockClipped() which can paint a block in 1
	 *   block move (instead of 2) which means no visual glitches.
	 * Topcat and Catseye code
	 */

#include "X.h"
#include "servermd.h"
#include "gcstruct.h"
#include "window.h"
#include "pixmapstr.h"
#include "scrnintstr.h"
#include "regionstr.h"
#include "windowstr.h"

#include "topcat.h"
#include "../cfb/cfb.h"

extern u_char XHP_NewRule[16][6];

#define PIXER(Drawable) ((hpPrivPixmapPtr)((PixmapPtr)Drawable)->devPrivate.ptr)

/*
 * tcPaintBlockClipped
 *   This routine paints a solid-colored block clipped to the composite
 * clipping region described in the devPriv of the GC.
 * Basically it programs the replacement rule registers to place a 
 * rectangle "width" pixels wide by "height" pixels tall
 * with the appropriate color (fgPixel) and alu starting at x, y.
 *  ~jra
 *  ~hp
 *  ~cd
 *  ~sph
 */
static void
tcPaintBlockClipped(dst, pGC, x,y, width,height)
     DrawablePtr dst;
     GC *pGC;
     int x, y, width, height;
{ 
    ScreenPtr pScreen = dst->pScreen;
    RegionPtr pRegion =
	((cfbPrivGC *)pGC->devPrivates[cfbGCPrivateIndex].ptr)->pCompositeClip;
    u_char pMask = getPlanesMask(pScreen);
    int
	clippedWidth,clippedHeight, startx,starty,
	zmask = pMask & pGC->planemask,
	srcpix = pGC->fgPixel,
	alu = pGC->alu,
	nbox = REGION_NUM_RECTS(pRegion),
	ctx = 0, cty = 0;	/* clip list translation */
    register TOPCAT *gp_hardware = getTcHardware(pScreen);
    register BoxPtr pBox = REGION_RECTS(pRegion);

    /* translate cliplist from pixmap coordinates to window coordinates */
    if (dst->type == DRAWABLE_PIXMAP)	/* offscreen memory pixmap */
    {
	ctx = PIXER(dst)->pChunk->x;
	cty = PIXER(dst)->pChunk->y;
    }

    waitbusy(pMask, gp_hardware);

    gp_hardware -> write_enable = zmask & srcpix;
    gp_hardware -> window_move_replacement_rule = XHP_NewRule[alu][3];
    gp_hardware -> write_enable = zmask & ~srcpix;
    gp_hardware -> window_move_replacement_rule = XHP_NewRule[alu][0];
    gp_hardware -> write_enable = zmask;
    gp_hardware -> pixel_write_replacement_rule = GXcopy;
    gp_hardware -> frame_buf_write_enable = zmask;

    for (; nbox--; pBox++)
    {
	/*
	 * for each clipping rectangle, put out the portion of the
	 * block contained in the clipping rect
	 */
	clipper(x,y,width,height,
		pBox->x1 +ctx, pBox->y1 +cty, pBox->x2 +ctx, pBox->y2 +cty,
		&startx,&starty, &clippedWidth,&clippedHeight);

	/*
	 * put out the block part in this clip box
	 */
	if (0<clippedWidth && 0<clippedHeight)
	{
	    clippedWidth <<= gp_hardware -> bits;
	    startx <<= gp_hardware -> bits;

	    waitbusy(pMask, gp_hardware);

	    gp_hardware -> source_x = startx;
	    gp_hardware -> source_y = starty;
	    gp_hardware -> dest_x = startx;
	    gp_hardware -> dest_y = starty;
	    gp_hardware -> window_width = clippedWidth;
	    gp_hardware -> window_height = clippedHeight;

	    gp_hardware -> start_move = zmask;
	}
    }
}

/*
 * tcPolyFillStippledRect -- A sometimes fast fill routine for filling
 * rectangles in video ram with a stipple.
 */
void
tcPolyFillStippledRect(pDrawable, pGC, nrectFill, prect)
     DrawablePtr pDrawable;
     GCPtr pGC;
     int nrectFill;	 	/* number of rectangles to fill */
     register xRectangle *prect; /* Pointer to first rectangle to fill */
{

    PixmapPtr pTile, pOldTile, cfbCreateOffscreenPixmap();
    cfbPrivGC *priv;
    GC *tempGC, *GetScratchGC();

    if (!(pGC->planemask))
	return;

    priv = (cfbPrivGC *) pGC->devPrivates[cfbGCPrivateIndex].ptr;

    if (pGC->fillStyle == FillStippled ||
	(pTile = cfbCreateOffscreenPixmap(pDrawable->pScreen,
					  priv->pRotatedPixmap->drawable.width,
					  priv->pRotatedPixmap->drawable.height,
					  pDrawable->depth)) == NULL)
    {
	tcPolyFillRect(pDrawable, pGC, nrectFill, prect);
	return;
    }

    tempGC = GetScratchGC(pTile->drawable.depth,pTile->drawable.pScreen);
    tempGC->fgPixel = pGC->fgPixel;
    tempGC->bgPixel = pGC->bgPixel;
    ValidateGC(pTile, tempGC);

    (pGC->ops->CopyPlane)(priv->pRotatedPixmap, pTile, tempGC, 0, 0,
			  priv->pRotatedPixmap->drawable.width,
			  priv->pRotatedPixmap->drawable.height,
			  0, 0, (unsigned long)0x1);

    FreeScratchGC(tempGC);

    pOldTile = priv->pRotatedPixmap;
    priv->pRotatedPixmap = pTile;
    pGC->fillStyle = FillTiled;
    ValidateGC(pDrawable, pGC);

    tcPolyFillRect(pDrawable, pGC, nrectFill, prect);

    priv->pRotatedPixmap = pOldTile;
    pGC->fillStyle = FillOpaqueStippled;
    ValidateGC(pDrawable, pGC);

    cfbDestroyPixmap(pTile);
}
