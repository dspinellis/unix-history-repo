
#include "X.h"
#include "Xmd.h"
#include "servermd.h"
#include "gcstruct.h"
#include "window.h"
#include "pixmapstr.h"
#include "scrnintstr.h"
#include "windowstr.h"

#include "../cfb/cfb.h"
#include "../cfb/cfbmskbits.h"
#include "topcat.h"

#if PPW == 4
extern void cfb8FillRectOpaqueStippled32();
extern void cfb8FillRectTransparentStippled32();
extern void cfb8FillRectStippledUnnatural();
#endif

extern void cfbFillRectSolidCopy(), cfbFillRectSolidXor (), cfbFillRectSolidGeneral ();

extern void cfbFillRectTile32Copy (), cfbFillRectTile32General ();
extern void cfbFillRectTileOdd ();

extern unsigned char XHP_NewRule[16][6];

#define PIXER(Drawable)  ((hpPrivPixmapPtr)((PixmapPtr)Drawable)->devPrivate.ptr)

/*
 * tcFillBoxSolid - solid fill a rectangle in Screen Memory
 */
void
tcFillBoxSolid(pDrawable, nBox, pBox, pixel, isCopy)
     DrawablePtr pDrawable;
     int nBox;
     BoxPtr pBox;
     unsigned long pixel;
     Bool isCopy;
{
    TOPCAT *hardware = getTcHardware(pDrawable->pScreen);
    int zmask;			/* plane mask - set to all planes */
    int XHP_bits = hardware->bits; /* 1 if low-res topcat else 0 */
    int xoff, yoff;
    register int x, y, h, w;

    if (pDrawable->type == DRAWABLE_PIXMAP) /* offscreen pixmap */
    {
	xoff = PIXER(pDrawable)->pChunk->x;
	yoff = PIXER(pDrawable)->pChunk->y;
    }
    else
    {
	xoff = 0; yoff = 0;
    }
    zmask = getPlanesMask(pDrawable->pScreen);

    while (nBox--)
    {
	x = pBox->x1 + xoff;
	y = pBox->y1 + yoff;
	w = pBox->x2 - pBox->x1;
	h = pBox->y2 - pBox->y1;
	
	if (w > 0 && h > 0)
	{
	    waitbusy(zmask, hardware);
	    if (isCopy)
	    {
		hardware->write_enable = zmask & pixel;
		hardware->window_move_replacement_rule =
		    XHP_NewRule[GXcopy][3];
		hardware->write_enable = zmask & ~pixel;
		hardware->window_move_replacement_rule =
		    XHP_NewRule[GXcopy][0];
		hardware->write_enable = zmask;
		hardware->pixel_write_replacement_rule = GXcopy;
	    }
	    else
	    {
		hardware->write_enable = zmask & pixel;
		hardware->window_move_replacement_rule =
		    XHP_NewRule[GXxor][3];
		hardware->write_enable = zmask & ~pixel;
		hardware->window_move_replacement_rule =
		    XHP_NewRule[GXxor][0];
		hardware->write_enable = zmask;
		hardware->pixel_write_replacement_rule = GXxor;
	    }
	    hardware->source_x = x << XHP_bits;
	    hardware->source_y = y;
	    hardware->dest_x = x << XHP_bits;
	    hardware->dest_y = y;
	    hardware->window_width = w << XHP_bits;
	    hardware->window_height = h;
	    hardware->start_move = zmask;
	}
	pBox++;
    }
}

/*
 * tcFillRectSolid - solid fill a rectangle in Screen Memory
 */
void
tcFillRectSolid(pDrawable, pGC, nBox, pBox)
     DrawablePtr pDrawable;
     GCPtr pGC;
     int nBox;
     BoxPtr pBox;
{
    TOPCAT *hardware = getTcHardware(pDrawable->pScreen);
    int zmask;			/* plane mask - set to all planes */
    int XHP_bits = hardware->bits; /* 1 if low-res topcat else 0 */
    int xoff, yoff;
    register int x, y, h, w;
    unsigned long pixel;

    pixel = (pGC->alu == GXinvert) ? pGC->planemask : pGC->fgPixel;
    
    if (pDrawable->type == DRAWABLE_PIXMAP) /* offscreen pixmap */
    {
	xoff = PIXER(pDrawable)->pChunk->x;
	yoff = PIXER(pDrawable)->pChunk->y;
    }
    else
    {
	xoff = 0; yoff = 0;
    }
    zmask = getPlanesMask(pDrawable->pScreen);

    while (nBox--)
    {
	x = pBox->x1 + xoff;
	y = pBox->y1 + yoff;
	w = pBox->x2 - pBox->x1;
	h = pBox->y2 - pBox->y1;
	
	if (w > 0 && h > 0)
	{
	    waitbusy(zmask, hardware);
	    if (pGC->alu == GXcopy)
	    {
		hardware->write_enable = zmask & pixel;
		hardware->window_move_replacement_rule =
		    XHP_NewRule[GXcopy][3];
		hardware->write_enable = zmask & ~pixel;
		hardware->window_move_replacement_rule =
		    XHP_NewRule[GXcopy][0];
		hardware->write_enable = zmask;
		hardware->pixel_write_replacement_rule = GXcopy;
	    }
	    else
	    {
		hardware->write_enable = zmask & pixel;
		hardware->window_move_replacement_rule =
		    XHP_NewRule[GXxor][3];
		hardware->write_enable = zmask & ~pixel;
		hardware->window_move_replacement_rule =
		    XHP_NewRule[GXxor][0];
		hardware->write_enable = zmask;
		hardware->pixel_write_replacement_rule = GXxor;
	    }
	    hardware->source_x = x << XHP_bits;
	    hardware->source_y = y;
	    hardware->dest_x = x << XHP_bits;
	    hardware->dest_y = y;
	    hardware->window_width = w << XHP_bits;
	    hardware->window_height = h;
	    hardware->start_move = zmask;
	}
	pBox++;
    }
}

void
tcFillRectTile (pDrawable, pGC, nBox, pBox)
    DrawablePtr	pDrawable;
    GCPtr	pGC;
    int		nBox;
    BoxPtr	pBox;
{
    int	xrot, yrot;
    PixmapPtr pTile;
 
    pTile = ((cfbPrivGC *) pGC->devPrivates[cfbGCPrivateIndex].ptr)->
	pRotatedPixmap;
    if (!pTile)
    {
	pTile = pGC->tile.pixmap;
	xrot = pDrawable->x + pGC->patOrg.x;
	yrot = pDrawable->y + pGC->patOrg.y;
    }
    else
    {
	xrot = 0;
	yrot = 0;
    }
    tcFillBoxTile (pDrawable, nBox, pBox, pTile, xrot, yrot, pGC->alu, pGC->planemask);
}


#define NUM_STACK_RECTS	1024

void
tcPolyFillRect(pDrawable, pGC, nrectFill, prectInit)
    DrawablePtr pDrawable;
    register GCPtr pGC;
    int		nrectFill; 	/* number of rectangles to fill */
    xRectangle	*prectInit;  	/* Pointer to first rectangle to fill */
{
    xRectangle	    *prect;
    RegionPtr	    prgnClip;
    register BoxPtr pbox;
    register BoxPtr pboxClipped;
    BoxPtr	    pboxClippedBase;
    BoxPtr	    pextent;
    BoxRec	    stackRects[NUM_STACK_RECTS];
    cfbPrivGC	    *priv;
    int		    numRects;
    void	    (*BoxFill)();
    int		    n;
    int		    xorg, yorg;
    int		    onScreen;

    priv = (cfbPrivGC *) pGC->devPrivates[cfbGCPrivateIndex].ptr;
    prgnClip = priv->pCompositeClip;
    onScreen = (pDrawable->type == DRAWABLE_WINDOW ||
	 ((PixmapPtr)pDrawable)->devKind == PIXMAP_FRAME_BUFFER);

    BoxFill = 0;
    switch (pGC->fillStyle)
    {
    case FillSolid:
	if (onScreen)
	    BoxFill = tcFillRectSolid;
	else
	switch (priv->rop) {
	case GXcopy:
	    BoxFill = cfbFillRectSolidCopy;
	    break;
	case GXxor:
	    BoxFill = cfbFillRectSolidXor;
	    break;
	default:
	    BoxFill = cfbFillRectSolidGeneral;
	    break;
	}
	break;
    case FillTiled:
	
	if (!((cfbPrivGCPtr) pGC->devPrivates[cfbGCPrivateIndex].ptr)->
							pRotatedPixmap)
	{
	    if (onScreen && pGC->tile.pixmap->devKind == PIXMAP_FRAME_BUFFER)
		BoxFill = tcFillRectTile;
	    else
		BoxFill = cfbFillRectTileOdd;
	    
	}
	else
	{
	    if (onScreen &&
		((cfbPrivGC *) pGC->devPrivates[cfbGCPrivateIndex].ptr)->
		    pRotatedPixmap->devKind == PIXMAP_FRAME_BUFFER)
		BoxFill = tcFillRectTile;
	    else
		if (pGC->alu == GXcopy && (pGC->planemask & PMSK) == PMSK)
		    BoxFill = cfbFillRectTile32Copy;
		else
		    BoxFill = cfbFillRectTile32General;
	}
	break;
#if (PPW == 4)
    case FillStippled:
	if (!((cfbPrivGCPtr) pGC->devPrivates[cfbGCPrivateIndex].ptr)->
							pRotatedPixmap)
	    BoxFill = cfb8FillRectStippledUnnatural;
	else
	    BoxFill = cfb8FillRectTransparentStippled32;
	break;
    case FillOpaqueStippled:
	if (!((cfbPrivGCPtr) pGC->devPrivates[cfbGCPrivateIndex].ptr)->
							pRotatedPixmap)
	    BoxFill = cfb8FillRectStippledUnnatural;
	else
	    BoxFill = cfb8FillRectOpaqueStippled32;
	break;
#endif
    }
    prect = prectInit;
    xorg = pDrawable->x;
    yorg = pDrawable->y;
    if (xorg || yorg)
    {
	prect = prectInit;
	n = nrectFill;
	while(n--)
	{
	    prect->x += xorg;
	    prect->y += yorg;
	    prect++;
	}
    }

    prect = prectInit;

    numRects = REGION_NUM_RECTS(prgnClip) * nrectFill;
    if (numRects > NUM_STACK_RECTS)
    {
	pboxClippedBase = (BoxPtr)ALLOCATE_LOCAL(numRects * sizeof(BoxRec));
	if (!pboxClippedBase)
	    return;
    }
    else
	pboxClippedBase = stackRects;

    pboxClipped = pboxClippedBase;
	
    if (REGION_NUM_RECTS(prgnClip) == 1)
    {
	int x1, y1, x2, y2, bx2, by2;

	pextent = REGION_RECTS(prgnClip);
	x1 = pextent->x1;
	y1 = pextent->y1;
	x2 = pextent->x2;
	y2 = pextent->y2;
    	while (nrectFill--)
    	{
	    if ((pboxClipped->x1 = prect->x) < x1)
		pboxClipped->x1 = x1;
    
	    if ((pboxClipped->y1 = prect->y) < y1)
		pboxClipped->y1 = y1;
    
	    bx2 = (int) prect->x + (int) prect->width;
	    if (bx2 > x2)
		bx2 = x2;
	    pboxClipped->x2 = bx2;
    
	    by2 = (int) prect->y + (int) prect->height;
	    if (by2 > y2)
		by2 = y2;
	    pboxClipped->y2 = by2;

	    prect++;
	    if ((pboxClipped->x1 < pboxClipped->x2) &&
		(pboxClipped->y1 < pboxClipped->y2))
	    {
		pboxClipped++;
	    }
    	}
    }
    else
    {
	int x1, y1, x2, y2, bx2, by2;

	pextent = (*pGC->pScreen->RegionExtents)(prgnClip);
	x1 = pextent->x1;
	y1 = pextent->y1;
	x2 = pextent->x2;
	y2 = pextent->y2;
    	while (nrectFill--)
    	{
	    BoxRec box;
    
	    if ((box.x1 = prect->x) < x1)
		box.x1 = x1;
    
	    if ((box.y1 = prect->y) < y1)
		box.y1 = y1;
    
	    bx2 = (int) prect->x + (int) prect->width;
	    if (bx2 > x2)
		bx2 = x2;
	    box.x2 = bx2;
    
	    by2 = (int) prect->y + (int) prect->height;
	    if (by2 > y2)
		by2 = y2;
	    box.y2 = by2;
    
	    prect++;
    
	    if ((box.x1 >= box.x2) || (box.y1 >= box.y2))
	    	continue;
    
	    n = REGION_NUM_RECTS (prgnClip);
	    pbox = REGION_RECTS(prgnClip);
    
	    /* clip the rectangle to each box in the clip region
	       this is logically equivalent to calling Intersect()
	    */
	    while(n--)
	    {
		pboxClipped->x1 = max(box.x1, pbox->x1);
		pboxClipped->y1 = max(box.y1, pbox->y1);
		pboxClipped->x2 = min(box.x2, pbox->x2);
		pboxClipped->y2 = min(box.y2, pbox->y2);
		pbox++;

		/* see if clipping left anything */
		if(pboxClipped->x1 < pboxClipped->x2 && 
		   pboxClipped->y1 < pboxClipped->y2)
		{
		    pboxClipped++;
		}
	    }
    	}
    }
    if (pboxClipped != pboxClippedBase)
	(*BoxFill) (pDrawable, pGC,
		    pboxClipped-pboxClippedBase, pboxClippedBase);
    if (pboxClippedBase != stackRects)
    	DEALLOCATE_LOCAL(pboxClippedBase);
}
