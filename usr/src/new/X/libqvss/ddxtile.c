/* tile.c	Perform a raster operation involving a pattern
 *
 *	TileFill	Patterns a portion of the screen
 *
 *	Modification History
 *
 *   Carver 8601.27 Added line to PixmapPut to adjust the func to compensate
 *		    for the reverse flag being set in the pixmap descriptor.
 *
 *   Carver 8510.25 Removed error checking in calls to copyrmsk
 *		       and copybmsk.  No errors are ever return.
 */

#include "ddxqvss.h"
#include "qvss.h"
#include "vstagbl.h"

extern BITMAP pbm;

/*ARGSUSED*/
TileFill (tile, xoff, yoff, xymask, dstx, dsty, width, height,
	  clips, clipcount, func, zmask)
	register PIXMAP *tile;
	register BITMAP *xymask;
	int xoff, yoff, dstx, dsty, width, height, zmask;
	register int func;
	CLIP *clips;
{
	register BITMAP *bm = (BITMAP *) tile->data;
	extern char SSMap[];

	/* ADJUST MAP FOR REVERSE PIXMAP */

	func = SSMap[ func | (tile->kind & InvertFlag)];

	if (!(zmask & 1))  return;
	if (PTYPE(tile) == BitmapPixmap) 
	  {
	  if (xymask == 0)
            {
	    copyrmsk (VSTA$K_SRC_HT_BITMAP, (short *)bm->data, bm->width, 
		      bm->height, xoff, yoff,
		      width, height,
		      (short *)pbm.data, pbm.width, pbm.height,
		      dstx, dsty, func, clipcount, clips);
	    }
	  else
	    {
	    copybmsk (VSTA$K_SRC_HT_BITMAP, (short *)bm->data, bm->width, 
		      bm->height, xoff, yoff,
		      (short *) xymask->data, xymask->width, xymask->height,
		      0, 0, width, height,
		      (short *)pbm.data, pbm.width, pbm.height,
		      dstx, dsty, func, clipcount, clips);
	    };
	  }
	else 
	  {
	  if (xymask == 0)
	    {
	    copyrmsk(VSTA$K_SRC_CONST, tile->data, 0, 0, 0, 0,
			width, height,
			(short *)pbm.data, pbm.width, pbm.height,
			dstx, dsty, func, clipcount, clips);
	    }
	  else
	    {
	    copybmsk (VSTA$K_SRC_CONST, tile->data, 0, 0, 0, 0,
		      (short *) xymask->data, xymask->width, xymask->height,
		      0, 0, width, height,
		      (short *)pbm.data, pbm.width, pbm.height,
		      dstx, dsty, func, clipcount, clips);
	    }
	  }

	return;
}
