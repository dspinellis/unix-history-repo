
#include "X.h"
#include "Xmd.h"
#include "servermd.h"
#include "gcstruct.h"
#include "window.h"
#include "pixmapstr.h"
#include "scrnintstr.h"

#include "../cfb/cfb.h"
#include "topcat.h"

#define PIXER(Drawable)  ((hpPrivPixmapPtr)((PixmapPtr)Drawable)->devPrivate.ptr)

void 
tcFillBoxTile(pDrawable, nBox, pBox, tile, xrot, yrot, rop, planemask)
    DrawablePtr	    pDrawable;
    int		    nBox;	/* number of boxes to fill */
    register BoxPtr pBox;	/* pointer to list of boxes to fill */
    PixmapPtr	    tile;	/* tile */
    int		    xrot, yrot;
    int		    rop;
    int		    planemask;
{
    int xoff, yoff;
    register hpChunk *pTileChunk; /* descriptive info for tiles bits */
    void (*bitMover)();
    ScreenPtr pScreen = pDrawable->pScreen;
    int tileWidth;		/* width of tile in pixels */
    int tileHeight;		/* height of tile */

    int         yT, yB, xL, xR, xLWidth, yTHeight,
                fullX, fullXLimit, fullY, fullYLimit,
                xCoord, yCoord;
    int destx, desty;
    int w;			/* width of current box */
    int h;			/* height of current box */
    

    pTileChunk = ((hpPrivPixmapPtr) (tile->devPrivate.ptr))->pChunk;
    /* rop = GXcopy /*pGC->alu*/;
    bitMover = getPrivScreenPtr(pScreen)->MoveBits;
    tileHeight = tile->drawable.height;
    tileWidth = tile->drawable.width;
    
    if (pDrawable->type == DRAWABLE_PIXMAP) /* offscreen pixmap */
    {
	xoff = PIXER(pDrawable)->pChunk->x;
	yoff = PIXER(pDrawable)->pChunk->y;
    }
    else
    {
	xoff = 0; yoff = 0;
    }

    for (; nBox--; pBox++)
    {
	w = pBox->x2 - pBox->x1;
	h = pBox->y2 - pBox->y1;
	
	/* offset into tile on left edge */
	xL = (pBox->x1 - xrot) % tileWidth;
	if (xL<0) xL += tileWidth;
	/* number of pixels used on left */
	xLWidth = (xL) ? (tileWidth - xL) : 0; 

	if (xLWidth >= w)
	{			/* one tile covers full x range */
	    xLWidth = w;
	    xR = 0;
	}
	else
	{			/* see if there's a part tile on right edge */
	    xR = (pBox->x2 - xrot) % tileWidth;
	    if (xR<0) xR += tileWidth;
	}
    
	/* y offset in tile at top */
	yT = (pBox->y1 - yrot) % tileHeight; 
	if (yT<0) yT += tileHeight; 
	/* number of pixels used on top */
	yTHeight = (yT) ? (tileHeight - yT) : 0;
	if (yTHeight >= h)
	{			/* tile covers full y range */
	    yTHeight = h;
	    yB = 0;
	}
	else
	{			/* portion of tile used on bottom */
	    yB = (pBox->y2 - yrot) % tileHeight; 
	    if (yB<0) yB += tileHeight; 
	}

	destx = pBox->x1 + xoff;
	desty = pBox->y1 + yoff;

	fullX = (w - xLWidth) / tileWidth; /* full tiles across */
	fullY = (h - yTHeight) / tileHeight; /* full vertical tiles */
	fullXLimit = destx + xLWidth + fullX * tileWidth; 
	fullYLimit = desty + yTHeight + fullY * tileHeight;

	if (rop != GXcopy)
	{
	    /* Now the actual work...
	       First, fill partial horizontal band across top of rectangle */
	    if (yT)
	    {
		if (xL)
		{ /* fill the top left corner if it's a partial tile */
		    (*bitMover)(pScreen, planemask, rop,
				pTileChunk->x + xL, pTileChunk->y + yT,
				destx, desty, xLWidth, yTHeight);
		}
		/* fill full width words in middle */
		for (xCoord = destx + xLWidth;
		     xCoord < fullXLimit;
		     xCoord += tileWidth)
		    (*bitMover)(pScreen, planemask, rop,
				pTileChunk->x, pTileChunk->y + yT,
				xCoord, desty, tileWidth, yTHeight);
		/* finally, partial tile on right edge. */
		if (xR)
		    (*bitMover)(pScreen, planemask, rop,
				pTileChunk->x, pTileChunk->y + yT,
				xCoord, desty, xR, yTHeight);
	    }
	    /* fill full tile height bands in middle */
	    for (yCoord = desty + yTHeight;
		 yCoord < fullYLimit;
		 yCoord += tileHeight)
	    {
		/* partial vertical band on left edge of rectangle */
		if (xL)
		    (*bitMover)(pScreen, planemask, rop,
				pTileChunk->x + xL, pTileChunk->y,
				destx, yCoord, xLWidth, tileHeight);
		/* now fill full width words in middle */
		for (xCoord = destx + xLWidth;
		     xCoord < fullXLimit;
		     xCoord += tileWidth)
		    (*bitMover)(pScreen, planemask, rop,
				pTileChunk->x, pTileChunk->y,
				xCoord, yCoord, tileWidth, tileHeight);
		/* finally, partial tile on right edge. */
		if (xR)
		    (*bitMover)(pScreen, planemask, rop,
				pTileChunk->x, pTileChunk->y,
				xCoord, yCoord, xR, tileHeight);
	    }
	    /* finally, partial height band across the bottom */
	    if (yB)
	    {
		/* partial vertical band on left edge of rectangle */
		if (xL)
		    (*bitMover)(pScreen, planemask, rop,
				pTileChunk->x + xL, pTileChunk->y,
				destx, yCoord, xLWidth, yB);
		/* now fill full width words in middle */
		for (xCoord = destx + xLWidth;
		     xCoord < fullXLimit;
		     xCoord += tileWidth)
		    (*bitMover)(pScreen, planemask, rop,
				pTileChunk->x, pTileChunk->y,
				xCoord, yCoord, tileWidth, yB);
		/* finally, partial tile on right edge. */
		if (xR)
		    (*bitMover)(pScreen, planemask, rop,
				pTileChunk->x, pTileChunk->y,
				xCoord, yCoord, xR, yB);
	    }
	}
	else    /* gxcopy so we can use on-screen bits as the source */
	{
	    /*
	     * We know that we have at least one whole tile to put out
	     * in either the X or Y direction.
	     *
	     * x,y of repeat cell. Start with the cell for horiz. repeat.
	     * These same vars will be used for the vertical repeat also
	     */
	    int cellx = destx + xLWidth; /* x of horiz. repeat tile */
	    int celly = desty + yTHeight;/* y of vert. repeat tile */
	    int cellWidth = tileWidth;
	    int cellHeight = tileHeight;
	    int xRemaining = w;
	    int yRemaining = h;

	    xCoord = cellx; 
	    yCoord = celly;

	    if (xL && yT)
	    {
		/*
		 * fill the whole corner with pieces of the tile
		 */

		(*bitMover)(pScreen, planemask, rop,
			    pTileChunk->x + xL, pTileChunk->y + yT,
			    destx, desty, xLWidth, yTHeight);
		xRemaining -= xLWidth;
		yRemaining -= yTHeight;
		
		if (!xRemaining && !yRemaining)
		    continue; /* if one piece was enough */

		if (xRemaining < cellWidth) cellWidth = xRemaining;
		if (yRemaining < cellHeight) cellHeight = yRemaining;

		/* fill left edge */
		if (yRemaining)
		{
		    (*bitMover)(pScreen, planemask, rop,
				pTileChunk->x + xL, pTileChunk->y,
				destx, desty + yTHeight, xLWidth, cellHeight);
		}

		/* fill top edge */
		if (xRemaining)
		{
		    (*bitMover)(pScreen, planemask, rop,
				pTileChunk->x, pTileChunk->y + yT,
				destx + xLWidth, desty, cellWidth, yTHeight);
		}

		if (xRemaining && yRemaining)
		{
		    (*bitMover)(pScreen, planemask, rop,
				pTileChunk->x, pTileChunk->y,
				destx + xLWidth, desty + yTHeight,
				cellWidth, cellHeight);
		}
		xRemaining -= cellWidth;
		yRemaining -= cellHeight;
		if (!xRemaining && !yRemaining)
		    continue;
		cellHeight += yTHeight;
	    }
	    else if (xL)
	    {
		/* fill left edge */
		if (yRemaining < cellHeight) cellHeight = yRemaining;
		(*bitMover)(pScreen, planemask, rop,
			    pTileChunk->x + xL, pTileChunk->y,
			    destx, desty, xLWidth, cellHeight);
		xRemaining -= xLWidth;
		yRemaining -= cellHeight;

		if (xRemaining)
		{
		    if (xRemaining < cellWidth) cellWidth = xRemaining;
		    (*bitMover)(pScreen, planemask, rop,
				pTileChunk->x, pTileChunk->y,
				destx + xLWidth, desty,
				cellWidth, cellHeight);
		    xRemaining -= cellWidth;
		}
	    }
	    else if (yT)
	    {
		/* fill top edge */
		if (xRemaining < cellWidth) cellWidth = xRemaining;
		(*bitMover)(pScreen, planemask, rop,
			    pTileChunk->x, pTileChunk->y + yT,
			    destx, desty, cellWidth, yTHeight);
		yRemaining -= yTHeight;
		xRemaining -= cellWidth;

		if (yRemaining)
		{
		    if (yRemaining < cellHeight) cellHeight = yRemaining;
		    (*bitMover)(pScreen, planemask, rop,
				pTileChunk->x, pTileChunk->y,
				destx, desty + yTHeight, cellWidth, cellHeight);
		    yRemaining -= cellHeight;
		    cellHeight += yTHeight;
		}
		else
		    cellHeight = yTHeight;
	    }
	    else  /* tile is aligned w/ upper left corner */
	    {
		if (xRemaining < cellWidth) cellWidth = xRemaining;
		if (yRemaining < cellHeight) cellHeight = yRemaining;
		(*bitMover)(pScreen, planemask, rop,
			    pTileChunk->x, pTileChunk->y,
			    destx, desty, cellWidth, cellHeight);
		xRemaining -= cellWidth;
		yRemaining -= cellHeight;
	    }

	    /*
	     * Now all the funky corner stuff is filled in.  We can go on to
	     * replicate the horizontal repeat cell across the screen
	     */

	    /* fill the rest of the first row */
	    xCoord = destx + xLWidth + cellWidth;
	    cellx = destx + xLWidth;
	    while (xRemaining)
	    {
		if (cellWidth > xRemaining) cellWidth = xRemaining;
		(*bitMover)(pScreen, planemask, rop,
			    cellx, desty,
			    xCoord, desty, cellWidth, cellHeight);
		xRemaining -= cellWidth;
		xCoord += cellWidth;
		cellWidth += cellWidth;
	    }

	    /* fill the rest of the vertical space */
	    cellx = destx;
	    celly = desty + yTHeight;
	    
	    yCoord = desty + cellHeight;
	    cellHeight -= yTHeight;

	    while (yRemaining)
	    {
		if (cellHeight > yRemaining) cellHeight = yRemaining;
		(*bitMover)(pScreen, planemask, rop,
			    cellx, celly,
			    destx, yCoord, w, cellHeight);
		yRemaining -= cellHeight;
		yCoord += cellHeight;
		cellHeight += cellHeight;
	    }
	}
    }
}
