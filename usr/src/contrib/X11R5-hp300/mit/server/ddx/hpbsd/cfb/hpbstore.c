	/* moreBS.c : more backing store stuff
	 */

#include "X.h"
#include "Xmd.h"
#include "Xproto.h"
#include "gcstruct.h"
#include "windowstr.h"

#include "scrnintstr.h"
#include "regionstr.h"
#include "mibstore.h"
#include "cfb.h"

extern void MemToMem(), ScreenToScreen(), MemToScreen();

#define PIXER(Drawable) ((hpPrivPixmapPtr)((PixmapPtr)Drawable)->devPrivate.ptr)

/*-
 *-----------------------------------------------------------------------
 * hpSaveAreas --
 *	Function called by miSaveAreas to actually fetch the areas to be
 *	saved into the backing pixmap. The region to save is
 *	already destination-relative and we're given the offset to the
 *	window origin, so we have only to create an array of points of the
 *	u.l. corners of the boxes in the region translated to the screen
 *	coordinate system and fetch the screen pixmap out of its devPrivate
 *	field....
 * screen -> pixmap
 *-----------------------------------------------------------------------
 */
void 
hpSaveAreas(pPixmap, prgnSave, xorg, yorg)
     PixmapPtr pPixmap;  	/* Backing pixmap */
     RegionPtr prgnSave; 	/* Region to save (pixmap-relative) */
     int xorg, yorg;    	/* X,Y origin of region */
{
    BoxRec sbox;
    register int a,b;
    GC gc;

    getPrivScreenPtr(pPixmap->drawable.pScreen)->CursorOff
	(pPixmap->drawable.pScreen);

    sbox.x1 = sbox.y1 = 0; sbox.x2 = sbox.y2 = 10000;
    gc.alu = GXcopy;
    gc.planemask = ~0;

    if (pPixmap->devKind == PIXMAP_FRAME_BUFFER)
    {
	a = PIXER(pPixmap)->pChunk->x;
	b = PIXER(pPixmap)->pChunk->y;

	ScreenToScreen(&pPixmap->drawable, &gc,
		       xorg,yorg, sbox.x2,sbox.y2,
		       0,0,
		       &sbox,1,
		       REGION_RECTS(prgnSave), REGION_NUM_RECTS(prgnSave),
		       0,0, a,b);
    }
    else
    {
	MemToMem(
		 getPrivScreenPtr(pPixmap->drawable.pScreen)->pDrawable,
		 &pPixmap->drawable,
		 &gc,
		 xorg,yorg, sbox.x2,sbox.y2,
		 0,0,
		 &sbox,1, REGION_RECTS(prgnSave), REGION_NUM_RECTS(prgnSave));
    }
}

/*-
 *-----------------------------------------------------------------------
 * hpRestoreAreas --
 *	Function called by miRestoreAreas to actually fetch the areas to be
 *	restored from the backing pixmap. The region to restore is
 *	already destination-relative and we're given the offset to the
 *	window origin, so we have only to create an array of points of the
 *	u.l. corners of the boxes in the region translated to the pixmap
 *	coordinate system and fetch the screen pixmap out of its devPrivate
 *	field....
 * pixmap -> screen
 *-----------------------------------------------------------------------
 */
void 
hpRestoreAreas(pPixmap, prgnRestore, xorg, yorg)
     PixmapPtr pPixmap;	/* Backing pixmap */
     RegionPtr prgnRestore; 	/* Region to restore (screen-relative)*/
     int xorg, yorg;	/* origin of window */
{
    BoxRec sbox;
    register int a,b;
    GC gc;

    getPrivScreenPtr(pPixmap->drawable.pScreen)->CursorOff
	(pPixmap->drawable.pScreen);

    sbox.x1 = sbox.y1 = 0;
    sbox.x2 = pPixmap->drawable.width; sbox.y2 = pPixmap->drawable.height;
    gc.alu = GXcopy;
    gc.planemask = ~0;

    if (pPixmap->devKind == PIXMAP_FRAME_BUFFER)
    {
	a = PIXER(pPixmap)->pChunk->x;
	b = PIXER(pPixmap)->pChunk->y;

	ScreenToScreen(&pPixmap->drawable, &gc,
		       0,0, sbox.x2,sbox.y2, xorg,yorg,
		       &sbox,1,
		       REGION_RECTS(prgnRestore),REGION_NUM_RECTS(prgnRestore),
		       a,b, 0,0);
    }
    else
    {
	gc.pScreen = pPixmap->drawable.pScreen;
	MemToScreen(&pPixmap->drawable, 
		    getPrivScreenPtr(pPixmap->drawable.pScreen)->pDrawable,
		    &gc,
		    0,0, sbox.x2,sbox.y2, xorg,yorg,
		    REGION_NUM_RECTS(prgnRestore), REGION_RECTS(prgnRestore),
		    0,0);
    }
}
