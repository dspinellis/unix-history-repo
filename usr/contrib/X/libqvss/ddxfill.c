/* fill.c	Perform a simple raster operation a section of the screen
 *
 *	PixFill	Do a function on the screen
 *
 *	Modification History
 *
 *	Carver 8510.25 Removed error checking in calls to copyrmsk
 *		       and copybmsk.  No errors are ever return.
 *
 */

#include "ddxqvss.h"
#include "qvss.h"
#include "vstagbl.h"

extern BITMAP pbm;

/*ARGSUSED*/
PixFill (srcpix, xymask, dstx, dsty, width, height, clips, clipcount,
	 func, zmask)
	int srcpix, dstx, dsty, width, height, clipcount, zmask;
	register BITMAP *xymask;
	register int func;
	CLIP *clips;
{
	int constant = srcpix & 1;

	if (!(zmask & 1)) {
	    return;
	}
	if (xymask == 0)
	  {
	  copyrmsk (VSTA$K_SRC_CONST, constant, 0, 0, 0, 0,
			  width, height, (short *) pbm.data,
			  pbm.width, pbm.height, dstx, dsty, func,
			  clipcount, clips);
	  return;
	  }

	copybmsk (VSTA$K_SRC_CONST, constant, 0, 0, 0, 0,
			(short *) xymask->data, xymask->width, 
			xymask->height, 0, 0, width, height, 
			(short *) pbm.data,
			pbm.width, pbm.height, dstx, dsty, func,
			clipcount, clips);
	return;
}

